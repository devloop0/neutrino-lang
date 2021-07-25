import "parse/parser.hsp"

import <"std/lib">
import <"std/io">

import "lex/lexer.hsp"
import "lex/token.hsp"
import "util/vector.hsp"
import "util/base.hsp"
import "util/generic_funcs.hsp"
import "util/error.hsp"
import "parse/util.hsp"
import "util/hash_set.hsp"

using std::lib::NULL;
using std::io::printf;
using neutrino::util::n_malloc;
using neutrino::util::n_free;

namespace neutrino { namespace parse {

func type lexer_data* lexer_data_init(type lex::lexer* l) {
	type lexer_data* ld = n_malloc(sizeof{type lexer_data})
		as type lexer_data*;
	ld->lexer = l;
	ld->buffer = util::vector_init(sizeof{type lex::token*}, deref_token_free);
	ld->pos = 0;
	ld->is_fresh = true;
	return ld;
}

func void lexer_data_delete(type lexer_data* ld) {
	lex::lexer_delete(ld->lexer);
	util::vector_delete(ld->buffer);

	n_free(ld as byte*);
}

func void deref_lexer_data_free(byte* b) {
	type lexer_data* ld = b as type lexer_data** @;
	lexer_data_delete(ld);
}

func type parser* parser_init(type lex::lexer* l) {
	type parser* p = n_malloc(sizeof{type parser})
		as type parser*;
	p->all_buffers = util::vector_init(sizeof{type lexer_data*},
		deref_lexer_data_free);
	p->buffer_stack = util::stack_init(sizeof{type lexer_data*},
		util::no_free);

	type lexer_data* initial_lexer_data = lexer_data_init(l);
	util::vector_append(p->all_buffers, initial_lexer_data$ as byte*);
	util::stack_push(p->buffer_stack, initial_lexer_data$ as byte*);

	p->final_eof_token = NULL as type lex::token*;
	p->included_uids = util::hs_init(
		util::uint_hash, util::uint_eq, util::no_free);

	return p;
}

func void parser_add_lexer(type parser* p, type lex::lexer* l) {
	type lexer_data* ld = lexer_data_init(l);
	util::vector_append(p->all_buffers, ld$ as byte*);
	util::stack_push(p->buffer_stack, ld$ as byte*);
}

func void parser_delete(type parser* p) {
	util::vector_delete(p->all_buffers);
	util::stack_delete(p->buffer_stack);
	if (p->final_eof_token != NULL as type lex::token*)
		lex::token_delete(p->final_eof_token);

	util::hs_delete(p->included_uids);
	n_free(p as byte*);
}

func[static] type lex::token* parser_pop_helper(type parser* p, bool destructive) {
	unsigned int st_size = util::stack_size(p->buffer_stack),
		orig_st_size = st_size;

	while (st_size != 0) {
		bool initial = st_size == orig_st_size;
		type lexer_data* ld = util::stack_at(p->buffer_stack, st_size - 1)
			as type lexer_data** @;

		unsigned int uid = lex::lexer_unique_identifier(ld->lexer);
		if (ld->is_fresh && util::hs_contains(p->included_uids, uid as byte*)) {
			util::maybe_ice(st_size != 1,
				"parser_pop_helper",
				"Base file cannot be a duplicate!");

			if (st_size != 1)
				util::stack_pop(p->buffer_stack);
			st_size--;
		}
		else {
			if (ld->is_fresh) {
				ld->is_fresh = false;
				util::hs_add(p->included_uids, uid as byte*);
			}

			unsigned int offset = initial ? 1 : 0;
			if (ld->pos + offset < util::vector_size(ld->buffer)) {
				type lex::token* ret = util::vector_at(ld->buffer, ld->pos + offset)
					as type lex::token** @;
				if (destructive && initial)
					ld->pos++;
				return ret;
			}

			if (destructive && initial)
				ld->pos++;

			type lex::token* ret = lex::lex(ld->lexer);
			if (ret != NULL as type lex::token*) {
				util::vector_append(ld->buffer, ret$ as byte*);
				return ret;
			}

			if (st_size != 1)
				util::stack_pop(p->buffer_stack);
			st_size--;
		}
	}

	type lexer_data* ld = util::stack_top(p->buffer_stack) as type lexer_data** @;
	if (p->final_eof_token == NULL as type lex::token*) {
		p->final_eof_token = lex::token_basic_init(ld->lexer,
			ld->lexer->line, ld->lexer->line,
			ld->lexer->col, ld->lexer->col,
			ld->lexer->pos, 0, lex::token_kind::EOF);
	}
	return p->final_eof_token;
}

func type lex::token* parser_pop(type parser* p) {
	return parser_pop_helper(p, true);
}

func type lex::token* parser_peek(type parser* p) {
	type lexer_data* ld = util::stack_top(p->buffer_stack)
		as type lexer_data** @;
	if (ld->pos < util::vector_size(ld->buffer))
		return util::vector_at(ld->buffer, ld->pos) as type lex::token** @;

	type lex::token* ret = parser_pop_helper(p, false);
	return ret;
}

func unsigned int parser_pos(type parser* p) {
	type lexer_data* ld = util::stack_top(p->buffer_stack)
		as type lexer_data** @;
	return ld->pos;
}

func void parser_set_pos(type parser* p, unsigned int pos) {
	type lexer_data* ld = util::stack_top(p->buffer_stack)
		as type lexer_data** @;
	ld->pos = pos;
}

func type lexer_data* parser_lexer_data(type parser* p) {
	return util::stack_top(p->buffer_stack) as type lexer_data** @;
}

func bool token_accept(type lex::token* tok, unsigned int k) {
	return tok->kind == lex::token_kind::INVALID
		|| tok->kind == k;
}

} } // namespace neutrino::parse
