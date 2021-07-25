import "parse/parser.hsp"

import <"std/lib">
import <"std/io">

import "util/hash_table.hsp"
import "util/generic_funcs.hsp"
import "ast/ast.hsp"
import "lex/token.hsp"
import "util/error.hsp"
import "util/vector.hsp"
import "parse/util.hsp"

using std::lib::NULL;
using std::io::printf;

namespace neutrino { namespace parse {

func[static] type ast::exp* parse_primary(type parser* p) {
	type lex::token* ptok = parser_peek(p);
	unsigned int spos = parser_pos(p);

	if (token_accept(ptok, lex::token_kind::INTEGER)
		|| token_accept(ptok, lex::token_kind::DECIMAL)
		|| token_accept(ptok, lex::token_kind::CHARACTER)
		|| token_accept(ptok, lex::token_kind::STRING)
		|| token_accept(ptok, lex::token_kind::TRUE)
		|| token_accept(ptok, lex::token_kind::FALSE)) {
		parser_pop(p);
		return ast::exp_primary_init(ast::primary_literal_init(ptok,
			metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::BACKSLASH)) {
		type ast::function* l = parse_fun(p, true);
		if (l == NULL as type ast::function*)
			return NULL as type ast::exp*;

		return ast::exp_primary_init(ast::primary_lambda_init(l));
	}
	else if (token_accept(ptok, lex::token_kind::OPEN_PAR)) {
		type lex::token* start = ptok;
		parser_pop(p);

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::OPEN_BRACE)) {
			type ast::stmt* s = parse_compound(p);
			if (s == NULL as type ast::stmt*)
				return NULL as type ast::exp*;

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a close parenthesis (')') here.", ptok);
				util::report_token(util::error_kind::NOTE,
					"To match an open parenthesis ('(') here.", start);
				return NULL as type ast::exp*;
			}
			parser_pop(p);

			return ast::exp_primary_init(ast::primary_stmt_init(s,
				metadata_parser_init(spos, p)));
		}
		else {
			ptok = parser_peek(p);
			if (token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
				parser_pop(p);
				type util::vector* tu = util::vector_init(sizeof{type ast::exp*}, deref_exp_free);
				return ast::exp_primary_init(ast::primary_tuple_init(tu, metadata_parser_init(spos, p)));
			}

			type ast::exp* nested = parse_assignment_exp(p);
			if (nested == NULL as type ast::exp*)
				return NULL as type ast::exp*;

			ptok = parser_peek(p);
			if (token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
				parser_pop(p);
				return ast::exp_primary_init(ast::primary_parenthesized_init(nested,
					metadata_parser_init(spos, p)));
			}

			type util::vector* tu = util::vector_init(sizeof{type ast::exp*}, deref_exp_free);
			util::vector_append(tu, nested$ as byte*);

			ptok = parser_peek(p);
			while (token_accept(ptok, lex::token_kind::COMMA)) {
				parser_pop(p);

				ptok = parser_peek(p);
				if (token_accept(ptok, lex::token_kind::CLOSE_PAR))
					break;
				
				type ast::exp* curr = parse_assignment_exp(p);
				if (curr == NULL as type ast::exp*)
					return NULL as type ast::exp*;

				util::vector_append(tu, curr$ as byte*);

				ptok = parser_peek(p);
			}

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a close parenthesis (')') here.", ptok);
				util::report_token(util::error_kind::NOTE,
					"To match an open parenthesis ('(') here.", start);
				return NULL as type ast::exp*;
			}
			parser_pop(p);

			return ast::exp_primary_init(ast::primary_tuple_init(tu, metadata_parser_init(spos, p)));
		}
	}
	else if (token_accept(ptok, lex::token_kind::OPEN_BRACKET)) {
		type lex::token* start = ptok;

		type util::vector* array = util::vector_init(sizeof{type ast::exp*},
			deref_exp_free);

		ptok = parser_pop(p);
		while (!token_accept(ptok, lex::token_kind::CLOSE_BRACKET)) {
			type ast::exp* elem = parse_assignment_exp(p);
			if (elem == NULL as type ast::exp*)
				return NULL as type ast::exp*;
			util::vector_append(array, elem$ as byte*);

			ptok = parser_peek(p);
			if (token_accept(ptok, lex::token_kind::COMMA))
				ptok = parser_pop(p);
			else
				break;
		}

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::CLOSE_BRACKET)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a close bracket (']') here.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match an open bracket ('[') here.", start);
			return NULL as type ast::exp*;
		}
		parser_pop(p);

		return ast::exp_primary_init(ast::primary_array_init(array,
			metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::IDENT)
		|| token_accept(ptok, lex::token_kind::COLON_COLON)) {
		type lex::token* start = ptok;

		type ast::qualified_identifier* qident = parse_qualified_identifier(p);
		if (qident == NULL as type ast::qualified_identifier*) {
			util::report_token(util::error_kind::ERROR,
				"Invalid qualified identifier starting here.", start);
			return NULL as type ast::exp*;
		}

		ptok = parser_peek(p);
		type lex::token* obrace = ptok;
		if (!token_accept(ptok, lex::token_kind::OPEN_BRACE))
			return ast::exp_primary_init(ast::primary_qualified_identifier_init(qident));

		parser_pop(p);

		type util::vector* mis = util::vector_init(sizeof{type ast::member_init*},
			deref_member_init_free);

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::CLOSE_BRACE)) {
			parser_pop(p);

			return ast::exp_primary_init(ast::primary_struct_init_init(
				ast::struct_init_init(qident, mis, metadata_parser_init(spos, p))));
		}

		ptok = parser_peek(p);
		while (true) {
			unsigned int mispos = parser_pos(p);

			ptok = parser_peek(p);
			type lex::token* dot = ptok;
			if (!token_accept(ptok, lex::token_kind::DOT)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a dot ('.') here to begin a struct member assignment.", ptok);
				return NULL as type ast::exp*;
			}
			parser_pop(p);

			ptok = parser_peek(p);
			type lex::token* ident = ptok;
			if (!token_accept(ptok, lex::token_kind::IDENT)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a member name after here to signify which member is being assigned to.", dot);
				return NULL as type ast::exp*;
			}
			parser_pop(p);

			ptok = parser_peek(p);
			type lex::token* eq = ptok;
			if (!token_accept(ptok, lex::token_kind::EQ)) {
				util::report_token(util::error_kind::ERROR,
					"Expected an equals ('=') after a member name to begin an initialization.", ident);
				return NULL as type ast::exp*;
			}
			parser_pop(p);

			type ast::exp* init = parse_assignment_exp(p);
			if (init == NULL as type ast::exp*)
				return NULL as type ast::exp*;

			type ast::member_init* mi = ast::member_init_init(ident, init, metadata_parser_init(mispos, p));
			util::vector_append(mis, mi$ as byte*);

			ptok = parser_peek(p);
			if (token_accept(ptok, lex::token_kind::COMMA))
				parser_pop(p), ptok = parser_peek(p);
			else if (token_accept(ptok, lex::token_kind::CLOSE_BRACE))
				break;
		}

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::CLOSE_BRACE)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a close brace ('}') here to end a struct initialization.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match an open brace ('{') here.", obrace);
			return NULL as type ast::exp*;
		}
		parser_pop(p);

		return ast::exp_primary_init(ast::primary_struct_init_init(ast::struct_init_init(qident,
			mis, metadata_parser_init(spos, p))));
	}

	util::report_token(util::error_kind::ERROR,
		"Invalid expression starting here.", ptok);
	return NULL as type ast::exp*;
}

func[static] type ast::exp* parse_postfix(type parser* p) {
	unsigned int spos = parser_pos(p);
	type ast::exp* base = parse_primary(p);
	if (base == NULL as type ast::exp*)
		return NULL as type ast::exp*;

	while (true) {
		type lex::token* ptok = parser_peek(p);

		type ast::exp* curr = NULL as type ast::exp*;
		if (token_accept(ptok, lex::token_kind::OPEN_BRACKET)) {
			type lex::token* start = ptok;
			parser_pop(p);

			type ast::exp* index = parse_exp(p);
			if (index == NULL as type ast::exp*)
				return NULL as type ast::exp*;

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::CLOSE_BRACKET)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a close bracket (']') here.", ptok);
				util::report_token(util::error_kind::NOTE,
					"To match an open bracket ('[') here.", start);
				return NULL as type ast::exp*;
			}
			parser_pop(p);

			curr = ast::exp_postfix_init(ast::postfix_index_init(base, index,
				metadata_parser_init(spos, p)));
		}
		else if (token_accept(ptok, lex::token_kind::PLUS_PLUS)) {
			parser_pop(p);
			curr = ast::exp_postfix_init(ast::postfix_nullary_init(base,
				ast::postfix_kind::INCREMENT, metadata_parser_init(spos, p)));
		}
		else if (token_accept(ptok, lex::token_kind::MINUS_MINUS)) {
			parser_pop(p);
			curr = ast::exp_postfix_init(ast::postfix_nullary_init(base,
				ast::postfix_kind::DECREMENT, metadata_parser_init(spos, p)));
		}
		else if (token_accept(ptok, lex::token_kind::AT)) {
			parser_pop(p);
			curr = ast::exp_postfix_init(ast::postfix_nullary_init(base,
				ast::postfix_kind::AT, metadata_parser_init(spos, p)));
		}
		else if (token_accept(ptok, lex::token_kind::DOLLAR)) {
			parser_pop(p);
			curr = ast::exp_postfix_init(ast::postfix_nullary_init(base,
				ast::postfix_kind::ADDRESS, metadata_parser_init(spos, p)));
		}
		else if (token_accept(ptok, lex::token_kind::DOT)
			|| token_accept(ptok, lex::token_kind::MINUS_GT)) {
			type lex::token* start = ptok;

			bool is_dot = token_accept(ptok, lex::token_kind::DOT);

			ptok = parser_pop(p);
			if (token_accept(ptok, lex::token_kind::OPEN_BRACKET)) {
				type lex::token* ob = ptok;
				parser_pop(p);

				ptok = parser_peek(p);
				type lex::token* index = ptok;
				if (!token_accept(ptok, lex::token_kind::INTEGER)) {
					util::report_token(util::error_kind::ERROR,
						"Expected an integer constant after here for a tuple index.", ob);
					return NULL as type ast::exp*;
				}
				parser_pop(p);

				ptok = parser_peek(p);
				if (!token_accept(ptok, lex::token_kind::CLOSE_BRACKET)) {
					util::report_token(util::error_kind::ERROR,
						"Expected a close bracket (']') here to end a tuple index.", ptok);
					util::report_token(util::error_kind::NOTE,
						"To match an open bracket ('[') here.", ob);
					return NULL as type ast::exp*;
				}
				parser_pop(p);

				curr = ast::exp_postfix_init(ast::postfix_member_access_init(base,
					is_dot ? ast::postfix_kind::DOT_INDEX : ast::postfix_kind::ARROW_INDEX,
					ast::member_data_init(index, false), metadata_parser_init(spos, p)));
			}
			else {
				type lex::token* member = ptok;
				if (!token_accept(ptok, lex::token_kind::IDENT)) {
					util::report_token(util::error_kind::ERROR,
						"Expected an identifier for a member access after here.", start);
					return NULL as type ast::exp*;
				}
				parser_pop(p);

				ptok = parser_peek(p);
				bool is_ufcs = token_accept(ptok, lex::token_kind::EXCL);
				if (is_ufcs)
					parser_pop(p);

				curr = ast::exp_postfix_init(ast::postfix_member_access_init(base,
					is_dot ? ast::postfix_kind::DOT : ast::postfix_kind::ARROW,
					ast::member_data_init(member, is_ufcs), metadata_parser_init(spos, p)));
			}
		}
		else if (token_accept(ptok, lex::token_kind::AS)) {
			type lex::token* start = ptok;
			parser_pop(p);

			type ast::typ* to = parse_typ(p);
			if (to == NULL as type ast::typ*)
				return NULL as type ast::exp*;

			curr = ast::exp_postfix_init(ast::postfix_as_init(base, to,
				metadata_parser_init(spos, p)));
		}
		else if (token_accept(ptok, lex::token_kind::OPEN_PAR)) {
			type lex::token* start = ptok;

			type util::vector* args = util::vector_init(sizeof{type ast::exp*},
				deref_exp_free);

			ptok = parser_pop(p);
			while (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
				type ast::exp* arg = parse_assignment_exp(p);
				if (arg == NULL as type ast::exp*)
					return NULL as type ast::exp*;
				util::vector_append(args, arg$ as byte*);

				ptok = parser_peek(p);
				if (token_accept(ptok, lex::token_kind::COMMA)) {
					ptok = parser_pop(p);
					if (token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
						util::report_token(util::error_kind::ERROR,
							"Expected an argument after a comma (',').", ptok);
						return NULL as type ast::exp*;
					}
				}
				else
					break;
			}

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a close parenthesis (')') here.", ptok);
				util::report_token(util::error_kind::NOTE,
					"To match an open parenthesis here.", start);
				return NULL as type ast::exp*;
			}
			parser_pop(p);

			curr = ast::exp_postfix_init(ast::postfix_arguments_init(base, args,
				metadata_parser_init(spos, p)));
		}
		else break;

		util::maybe_ice(curr != NULL as type ast::exp*
			&& curr->kind == ast::exp_kind::POSTFIX,
			"parse_postfix",
			"Expected 'curr' to be a postfix expression here!");

		if (base->kind == ast::exp_kind::POSTFIX)
			base->which.postfix->next = curr->which.postfix;
		base = curr;
	}

	return base;
}

func[static] type ast::exp* parse_unary(type parser* p) {
	type lex::token* ptok = parser_peek(p);
	unsigned int spos = parser_pos(p);

	bool nullary = false;
	unsigned int u_kind;
	if (token_accept(ptok, lex::token_kind::PLUS_PLUS)) {
		parser_pop(p);
		u_kind = ast::unary_kind::INCREMENT;
		nullary = true;
	}
	else if (token_accept(ptok, lex::token_kind::MINUS_MINUS)) {
		parser_pop(p);
		u_kind = ast::unary_kind::DECREMENT;
		nullary = true;
	}
	else if (token_accept(ptok, lex::token_kind::PLUS)) {
		parser_pop(p);
		u_kind = ast::unary_kind::PLUS;
		nullary = true;
	}
	else if (token_accept(ptok, lex::token_kind::MINUS)) {
		parser_pop(p);
		u_kind = ast::unary_kind::MINUS;
		nullary = true;
	}
	else if (token_accept(ptok, lex::token_kind::EXCL)) {
		parser_pop(p);
		u_kind = ast::unary_kind::NOT;
		nullary = true;
	}
	else if (token_accept(ptok, lex::token_kind::TILDE)) {
		parser_pop(p);
		u_kind = ast::unary_kind::CMPL;
		nullary = true;
	}
	else if (token_accept(ptok, lex::token_kind::NEW)
		|| token_accept(ptok, lex::token_kind::STK)
		|| token_accept(ptok, lex::token_kind::RESV)) {
		type lex::token* start = ptok;
		unsigned int k = 0;
		if (ptok->kind == lex::token_kind::NEW)
			k = ast::unary_kind::NEW;
		else if (ptok->kind == lex::token_kind::STK)
			k = ast::unary_kind::STK;
		else 
			k = ast::unary_kind::RESV;
		parser_pop(p);

		type ast::typ* t = parse_typ(p);
		if (t == NULL as type ast::typ*)
			return NULL as type ast::exp*;

		ptok = parser_peek(p);
		type lex::token* opar = ptok;
		if (!token_accept(ptok, lex::token_kind::OPEN_PAR)) {
			return ast::exp_unary_init(ast::unary_alloc_info_init(k,
				ast::alloc_info_init(t, NULL as type ast::exp*),
				metadata_parser_init(spos, p)));
		}
		parser_pop(p);

		type ast::exp* e = parse_exp(p);
		if (e == NULL as type ast::exp*)
			return NULL as type ast::exp*;

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a close parenthesis here.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match an open parenthesis here.", opar);
			return NULL as type ast::exp*;
		}
		parser_pop(p);

		return ast::exp_unary_init(ast::unary_alloc_info_init(k,
			ast::alloc_info_init(t, e),
			metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::DELETE)) {
		parser_pop(p);

		type ast::exp* e = parse_unary(p);
		if (e == NULL as type ast::exp*)
			return NULL as type ast::exp*;

		return ast::exp_unary_init(
			ast::unary_delete_init(e,
				metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::SIZEOF)
		|| token_accept(ptok, lex::token_kind::ALIGNOF)) {
		type lex::token* start = ptok;
		bool is_sizeof = token_accept(ptok, lex::token_kind::SIZEOF);

		ptok = parser_pop(p);
		if (token_accept(ptok, lex::token_kind::OPEN_PAR)) {
			type lex::token* op_start = ptok;
			parser_pop(p);

			type ast::exp* e = parse_exp(p);
			if (e == NULL as type ast::exp*)
				return NULL as type ast::exp*;

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a close parenthesis (')') here.", ptok);
				util::report_token(util::error_kind::NOTE,
					"To match the open parenthesis ('(') here.", op_start);
				return NULL as type ast::exp*;
			}
			parser_pop(p);

			return ast::exp_unary_init(ast::unary_exp_init(is_sizeof
				? ast::unary_kind::SIZEOF_EXP : ast::unary_kind::ALIGNOF_EXP,
				e, metadata_parser_init(spos, p)));
		}
		else if (token_accept(ptok, lex::token_kind::OPEN_BRACE)) {
			type lex::token* ob_start = ptok;
			parser_pop(p);

			type ast::typ* t = parse_typ(p);
			if (t == NULL as type ast::typ*)
				return NULL as type ast::exp*;

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::CLOSE_BRACE)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a close brace ('}') here.", ptok);
				util::report_token(util::error_kind::NOTE,
					"To match the open brace ('{') here.", ob_start);
				return NULL as type ast::exp*;
			}
			parser_pop(p);

			return ast::exp_unary_init(ast::unary_typ_init(is_sizeof
				? ast::unary_kind::SIZEOF_TYP : ast::unary_kind::ALIGNOF_TYP,
				t, metadata_parser_init(spos, p)));
		}
		else {
			util::report_token(util::error_kind::ERROR,
				"Expected an open brace ('{') or open parenthesis ('(') to start a 'sizeof' or 'alignof' expression.",
				start);
			return NULL as type ast::exp*;
		}

		util::ice("parse_unary", "This should be unreachable while parsing sizeof's/alignof's!");
	}
	
	if (nullary) {
		type ast::exp* u = parse_unary(p);
		if (u == NULL as type ast::exp*)
			return NULL as type ast::exp*;

		return ast::exp_unary_init(ast::unary_nullary_init(u_kind, u,
			metadata_parser_init(spos, p)));
	}
	else
		return parse_postfix(p);
}

func[static] type ast::exp* parse_left_assoc_binary(type parser* p,
	fn type ast::exp*(type parser*) base_parser,
	type util::hash_table* tok2op) {
	unsigned int spos = parser_pos(p);
	type ast::exp* lhs = base_parser(p);
	if (lhs == NULL as type ast::exp*)
		return NULL as type ast::exp*;

	while (true) {
		type lex::token* ptok = parser_peek(p);

		unsigned int op;
		if (!util::ht_get(tok2op, ptok->kind as byte*, op$ as byte**))
			break;
		parser_pop(p);

		type ast::exp* rhs = base_parser(p);
		if (rhs == NULL as type ast::exp*)
			return NULL as type ast::exp*;

		lhs = ast::exp_binary_init(ast::binary_init(lhs, ptok, op, rhs,
			metadata_parser_init(spos, p)));
	}
	return lhs;
}

func[static] type ast::exp* parse_multiplicative(type parser* p) {
	type util::hash_table* tok2op = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);
	util::ht_set(tok2op, lex::token_kind::STAR as byte*,
		ast::binary_op_kind::MULT as byte*);
	util::ht_set(tok2op, lex::token_kind::SLASH as byte*,
		ast::binary_op_kind::DIV as byte*);
	util::ht_set(tok2op, lex::token_kind::PERCENT as byte*,
		ast::binary_op_kind::MOD as byte*);

	type ast::exp* ret = parse_left_assoc_binary(p, parse_unary, tok2op);

	util::ht_delete(tok2op);
	return ret;
}

func[static] type ast::exp* parse_additive(type parser* p) {
	type util::hash_table* tok2op = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);
	util::ht_set(tok2op, lex::token_kind::PLUS as byte*,
		ast::binary_op_kind::ADD as byte*);
	util::ht_set(tok2op, lex::token_kind::MINUS as byte*,
		ast::binary_op_kind::SUB as byte*);

	type ast::exp* ret = parse_left_assoc_binary(p, parse_multiplicative, tok2op);

	util::ht_delete(tok2op);
	return ret;
}

func[static] type ast::exp* parse_shift(type parser* p) {
	type util::hash_table* tok2op = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);
	util::ht_set(tok2op, lex::token_kind::GT_GT as byte*,
		ast::binary_op_kind::SHR as byte*);
	util::ht_set(tok2op, lex::token_kind::LT_LT as byte*,
		ast::binary_op_kind::SHL as byte*);

	type ast::exp* ret = parse_left_assoc_binary(p, parse_additive, tok2op);

	util::ht_delete(tok2op);
	return ret;
}

func[static] type ast::exp* parse_relational(type parser* p) {
	type util::hash_table* tok2op = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);
	util::ht_set(tok2op, lex::token_kind::GT as byte*,
		ast::binary_op_kind::GT as byte*);
	util::ht_set(tok2op, lex::token_kind::LT as byte*,
		ast::binary_op_kind::LT as byte*);
	util::ht_set(tok2op, lex::token_kind::GT_EQ as byte*,
		ast::binary_op_kind::GTE as byte*);
	util::ht_set(tok2op, lex::token_kind::LT_EQ as byte*,
		ast::binary_op_kind::LTE as byte*);

	type ast::exp* ret = parse_left_assoc_binary(p, parse_shift, tok2op);

	util::ht_delete(tok2op);
	return ret;
}

func[static] type ast::exp* parse_equality(type parser* p) {
	type util::hash_table* tok2op = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);
	util::ht_set(tok2op, lex::token_kind::EQ_EQ as byte*,
		ast::binary_op_kind::EQ_EQ as byte*);
	util::ht_set(tok2op, lex::token_kind::EXCL_EQ as byte*,
		ast::binary_op_kind::NE as byte*);

	type ast::exp* ret = parse_left_assoc_binary(p, parse_relational, tok2op);

	util::ht_delete(tok2op);
	return ret;
}

func[static] type ast::exp* parse_bitwise_and(type parser* p) {
	type util::hash_table* tok2op = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);
	util::ht_set(tok2op, lex::token_kind::AMP as byte*,
		ast::binary_op_kind::BAND as byte*);

	type ast::exp* ret = parse_left_assoc_binary(p, parse_equality, tok2op);

	util::ht_delete(tok2op);
	return ret;
}

func[static] type ast::exp* parse_bitwise_xor(type parser* p) {
	type util::hash_table* tok2op = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);
	util::ht_set(tok2op, lex::token_kind::CARET as byte*,
		ast::binary_op_kind::BXOR as byte*);

	type ast::exp* ret = parse_left_assoc_binary(p, parse_bitwise_and, tok2op);

	util::ht_delete(tok2op);
	return ret;
}

func[static] type ast::exp* parse_bitwise_or(type parser* p) {
	type util::hash_table* tok2op = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);
	util::ht_set(tok2op, lex::token_kind::BAR as byte*,
		ast::binary_op_kind::BOR as byte*);

	type ast::exp* ret = parse_left_assoc_binary(p, parse_bitwise_xor, tok2op);

	util::ht_delete(tok2op);
	return ret;
}

func[static] type ast::exp* parse_logical_and(type parser* p) {
	type util::hash_table* tok2op = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);
	util::ht_set(tok2op, lex::token_kind::AMP_AMP as byte*,
		ast::binary_op_kind::LAND as byte*);

	type ast::exp* ret = parse_left_assoc_binary(p, parse_bitwise_or, tok2op);

	util::ht_delete(tok2op);
	return ret;
}

func[static] type ast::exp* parse_logical_or(type parser* p) {
	type util::hash_table* tok2op = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);
	util::ht_set(tok2op, lex::token_kind::BAR_BAR as byte*,
		ast::binary_op_kind::LOR as byte*);

	type ast::exp* ret = parse_left_assoc_binary(p, parse_logical_and, tok2op);

	util::ht_delete(tok2op);
	return ret;
}

func[static] type ast::exp* parse_conditional(type parser* p) {
	unsigned int spos = parser_pos(p);
	type ast::exp* c = parse_logical_or(p);
	if (c == NULL as type ast::exp*)
		return NULL as type ast::exp*;

	type lex::token* ptok = parser_peek(p);
	if (token_accept(ptok, lex::token_kind::QUESTION_MARK)) {
		parser_pop(p);

		type ast::exp* l = parse_exp(p);
		if (l == NULL as type ast::exp*)
			return NULL as type ast::exp*;

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::COLON)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a ':' to separate different paths of a ternary conditional.", ptok);
			return NULL as type ast::exp*;
		}
		parser_pop(p);

		type ast::exp* r = parse_conditional(p);
		if (r == NULL as type ast::exp*)
			return NULL as type ast::exp*;

		return ast::exp_ternary_init(ast::ternary_init(c, l, r,
			metadata_parser_init(spos, p)));
	}

	return c;
}

func type ast::exp* parse_assignment_exp(type parser* p) {
	unsigned int spos = parser_pos(p);
	type lex::token* ptok = parser_peek(p);

	if (token_accept(ptok, lex::token_kind::BACKTICK)) {
		type lex::token* start_pa = ptok;
		parser_pop(p);

		type ast::pat* lhs = parse_pat(p);
		if (lhs == NULL as type ast::pat*)
			return NULL as type ast::exp*;

		ptok = parser_peek(p);
		type lex::token* end_bt = ptok;
		if (!token_accept(ptok, lex::token_kind::BACKTICK)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a backtick ('`') here to delimit a pattern for assignment.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match a backtick ('`') here.", start_pa);
			return NULL as type ast::exp*;
		}
		parser_pop(p);

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::EQ)) {
			util::report_token(util::error_kind::ERROR,
				"Expected an equals ('=') after here to assign a pattern to an expression.", end_bt);
			return NULL as type ast::exp*;
		}
		parser_pop(p);

		type ast::exp* rhs = parse_exp(p);
		if (rhs == NULL as type ast::exp*)
			return NULL as type ast::exp*;

		return ast::exp_pat_assign_init(ast::pat_assign_init(lhs, rhs,
			metadata_parser_init(spos, p)));
	}

	type ast::exp* c = parse_conditional(p);
	if (c == NULL as type ast::exp*)
		return NULL as type ast::exp*;

	if (c->kind != ast::exp_kind::PRIMARY
		&& c->kind != ast::exp_kind::POSTFIX
		&& c->kind != ast::exp_kind::UNARY) {
		return c;
	}

	unsigned int op_kind = 0x0;

	ptok = parser_peek(p);
	if (token_accept(ptok, lex::token_kind::EQ))
		parser_pop(p), op_kind = ast::binary_op_kind::EQ;
	else if (token_accept(ptok, lex::token_kind::STAR_EQ))
		parser_pop(p), op_kind = ast::binary_op_kind::MULT_EQ;
	else if (token_accept(ptok, lex::token_kind::SLASH_EQ))
		parser_pop(p), op_kind = ast::binary_op_kind::DIV_EQ;
	else if (token_accept(ptok, lex::token_kind::PERCENT_EQ))
		parser_pop(p), op_kind = ast::binary_op_kind::MOD_EQ;
	else if (token_accept(ptok, lex::token_kind::PLUS_EQ))
		parser_pop(p), op_kind = ast::binary_op_kind::ADD_EQ;
	else if (token_accept(ptok, lex::token_kind::MINUS_EQ))
		parser_pop(p), op_kind = ast::binary_op_kind::SUB_EQ;
	else if (token_accept(ptok, lex::token_kind::GT_GT_EQ))
		parser_pop(p), op_kind = ast::binary_op_kind::SHR_EQ;
	else if (token_accept(ptok, lex::token_kind::LT_LT_EQ))
		parser_pop(p), op_kind = ast::binary_op_kind::SHL_EQ;
	else if (token_accept(ptok, lex::token_kind::AMP_EQ))
		parser_pop(p), op_kind = ast::binary_op_kind::BAND_EQ;
	else if (token_accept(ptok, lex::token_kind::BAR_EQ))
		parser_pop(p), op_kind = ast::binary_op_kind::BOR_EQ;
	else if (token_accept(ptok, lex::token_kind::CARET_EQ))
		parser_pop(p), op_kind = ast::binary_op_kind::BXOR_EQ;

	if (op_kind == 0x0)
		return c;

	type ast::exp* r = parse_assignment_exp(p);
	if (r == NULL as type ast::exp*)
		return NULL as type ast::exp*;

	return ast::exp_binary_init(ast::binary_init(c, ptok, op_kind, r,
		metadata_parser_init(spos, p)));
}

func type ast::exp* parse_exp(type parser* p) {
	return parse_assignment_exp(p);
}

} } // namespace neutrino::parse
