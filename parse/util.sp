import "parse/util.hsp"

import <"std/lib">
import <"std/io">
import <"std/string">

import "parse/parser.hsp"
import "ast/ast.hsp"
import "lex/token.hsp"
import "util/vector.hsp"
import "util/generic_funcs.hsp"
import "util/error.hsp"
import "util/base.hsp"
import "util/file.hsp"
import "util/string.hsp"

using neutrino::util::n_malloc;
using neutrino::util::n_strdup;
using neutrino::util::n_free;
using std::lib::NULL;
using std::io::printf;
using std::string::strrchr;
using std::string::strlen;
using std::string::strcpy;
using std::string::strcat;
using std::string::strncpy;

namespace neutrino { namespace parse {

func type ast::metadata* metadata_parser_init(unsigned int spos,
	type parser* p) {
	type lexer_data* ld = parser_lexer_data(p);
	return ast::metadata_init(ld->buffer, spos, parser_pos(p));
}

func void deref_token_free(byte* b) {
	type lex::token* tok = b as type lex::token** @;
	lex::token_delete(tok);
}

func void deref_exp_free(byte* b) {
	type ast::exp* e = b as type ast::exp** @;
	ast::exp_delete(e);
}

func void deref_typ_free(byte* b) {
	type ast::typ* t = b as type ast::typ** @;
	ast::typ_delete(t);
}

func void deref_stmt_free(byte* b) {
	type ast::stmt* s = b as type ast::stmt** @;
	ast::stmt_delete(s);
}

func void deref_asm_component_free(byte* b) {
	type ast::asm_component* ac = b as type ast::asm_component** @;
	ast::asm_component_delete(ac);
}

func void deref_decl_component_free(byte* b) {
	type ast::decl_component* dc = b as type ast::decl_component** @;
	ast::decl_component_delete(dc);
}

func void deref_decl_free(byte* b) {
	type ast::decl* d = b as type ast::decl** @;
	ast::decl_delete(d);
}

func void deref_top_level_free(byte* b) {
	type ast::top_level* tl = b as type ast::top_level** @;
	ast::top_level_delete(tl);
}

func void deref_qualified_identifier_free(byte* b) {
	type ast::qualified_identifier* qi =
		b as type ast::qualified_identifier** @;
	ast::qualified_identifier_delete(qi);
}

func void deref_import_alias_data_free(byte* b) {
	type ast::import_alias_data* iad = b as type ast::import_alias_data** @;
	ast::import_alias_data_delete(iad);
}

func void deref_member_init_free(byte* b) {
	type ast::member_init* mi = b as type ast::member_init** @;
	ast::member_init_delete(mi);
}

func void deref_aggregate_member_free(byte* b) {
	type ast::aggregate_member* am = b as type ast::aggregate_member** @;
	ast::aggregate_member_delete(am);
}

func void deref_pat_free(byte* b) {
	type ast::pat* ap = b as type ast::pat** @;
	ast::pat_delete(ap);
}

func void deref_member_pat_free(byte* b) {
	type ast::member_pat* mp = b as type ast::member_pat** @;
	ast::member_pat_delete(mp);
}

func void deref_type_alias_value_free(byte* b) {
	type ast::type_alias_value* tav = b as type ast::type_alias_value** @;
	ast::type_alias_value_delete(tav);
}

func void deref_capture_data_free(byte* b) {
	type ast::capture_data* cd = b as type ast::capture_data** @;
	ast::capture_data_delete(cd);
}

func void deref_match_branch_free(byte* b) {
	type ast::match_branch* mb = b as type ast::match_branch** @;
	ast::match_branch_delete(mb);
}

func void token_free(byte* b) {
	type lex::token* tok = b as type lex::token*;
	lex::token_delete(tok);
}

func void exp_free(byte* b) {
	type ast::exp* e = b as type ast::exp*;
	ast::exp_delete(e);
}

func void typ_free(byte* b) {
	type ast::typ* t = b as type ast::typ*;
	ast::typ_delete(t);
}

func void stmt_free(byte* b) {
	type ast::stmt* s = b as type ast::stmt*;
	ast::stmt_delete(s);
}

func type ast::attribute* parse_attribute(type parser* p) {
	type lex::token* ptok = parser_peek(p);
	type lex::token* start = ptok;
	unsigned int spos = parser_pos(p);

	util::maybe_ice(token_accept(ptok, lex::token_kind::AT),
		"parse_attribute",
		"This should be unreachable!");
	parser_pop(p);

	ptok = parser_peek(p);
	type lex::token* obracket = ptok;
	if (!token_accept(ptok, lex::token_kind::OPEN_BRACKET)) {
		util::report_token(util::error_kind::ERROR,
			"Expected an open bracket ('[') after here to start an attribute specification.", start);
		return NULL as type ast::attribute*;
	}
	parser_pop(p);

	type util::vector* attrs_list = util::vector_init(sizeof{type ast::attribute_value},
		util::no_free);

	ptok = parser_peek(p);
	if (!token_accept(ptok, lex::token_kind::CLOSE_BRACKET)) {
		while (true) {
			if (!token_accept(ptok, lex::token_kind::IDENT)) {
				util::report_token(util::error_kind::ERROR,
					"Expected an identifier here to specify an attribute key.", ptok);
				return NULL as type ast::attribute*;
			}
			parser_pop(p);

			type ast::attribute_value av;
			av.key = ptok;
			av.value = NULL as type lex::token*;

			ptok = parser_peek(p);
			type lex::token* eq = ptok;
			if (token_accept(ptok, lex::token_kind::EQ)) {
				parser_pop(p);

				ptok = parser_peek(p);
				if (!token_accept(ptok, lex::token_kind::STRING)
					&& !token_accept(ptok, lex::token_kind::INTEGER)) {
					util::report_token(util::error_kind::ERROR,
						"Expected an equals ('=') to specify an attribute value.", eq);
					return NULL as type ast::attribute*;
				}
				parser_pop(p);

				av.value = ptok;
			}

			util::vector_append(attrs_list, av$ as byte*);

			ptok = parser_peek(p);
			if (token_accept(ptok, lex::token_kind::COMMA))
				parser_pop(p), ptok = parser_peek(p);
			else if (token_accept(ptok, lex::token_kind::CLOSE_BRACKET))
				break;
		}
	}

	ptok = parser_peek(p);
	if (!token_accept(ptok, lex::token_kind::CLOSE_BRACKET)) {
		util::report_token(util::error_kind::ERROR,
			"Expected a close bracket (']') here to end an attribute specification.", ptok);
		util::report_token(util::error_kind::NOTE,
			"To match an open bracket ('[') here.", obracket);
		return NULL as type ast::attribute*;
	}
	parser_pop(p);

	return ast::attribute_init(attrs_list, metadata_parser_init(spos, p));
}

func type ast::qualified_identifier* parse_qualified_identifier(
	type parser* p) {
	type util::vector* ret = util::vector_init(
		sizeof{type lex::token*}, util::no_free);

	type lex::token* ptok = parser_peek(p);
	unsigned int spos = parser_pos(p);

	if (!token_accept(ptok, lex::token_kind::IDENT)
		&& !token_accept(ptok, lex::token_kind::COLON_COLON)) {
		return NULL as type ast::qualified_identifier*;
	}

	if (token_accept(ptok, lex::token_kind::IDENT)) {
		util::vector_append(ret, ptok$ as byte*);
		ptok = parser_pop(p);
	}

	while (token_accept(ptok, lex::token_kind::COLON_COLON)) {
		if (util::vector_empty(ret)) {
			ptok = NULL as type lex::token*;
			util::vector_append(ret, ptok$ as byte*);
		}

		ptok = parser_pop(p);
		if (!token_accept(ptok, lex::token_kind::IDENT)) {
			util::vector_delete(ret);
			return NULL as type ast::qualified_identifier*;
		}

		util::vector_append(ret, ptok$ as byte*);
		ptok = parser_pop(p);
	}

	return ast::qualified_identifier_init(ret,
		metadata_parser_init(spos, p));
}

func type util::string* resolve_include(type parser* p, type lex::token* f) {
	type lexer_data* ld = parser_lexer_data(p);
	char* file_name = util::string_data(lex::lexer_file_name(ld->lexer));

	char* last_slash = strrchr(file_name, util::PATH_SEP@);

	if (last_slash == NULL as char*) {
		type util::string* ret = util::string_init("");
		util::string_catc(ret, f->text);
		util::string_catc(ret, HEADER_EXT);
		return ret;
	}

	unsigned int pre_length = last_slash as unsigned int
		- file_name  as unsigned int;
	char* pre_path = n_malloc(sizeof{char} * ((pre_length + 1) + 1)) as char*;
	strncpy(pre_path, file_name, pre_length + 1);
	pre_path[pre_length + 1] = 0;
	type util::string* ret = util::string_init_move(pre_path);

	util::string_catc(ret, f->text);
	util::string_catc(ret, HEADER_EXT);
	return ret;
}

} } // namespace neutrino::parse
