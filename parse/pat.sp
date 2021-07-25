import "parse/parser.hsp"

import <"std/io">
import <"std/lib">

import "ast/ast.hsp"
import "lex/token.hsp"
import "util/error.hsp"
import "parse/util.hsp"
import "util/vector.hsp"

using std::io::printf;
using std::lib::NULL;

namespace neutrino { namespace parse {

func type ast::pat* parse_pat(type parser* p) {
	type lex::token* ptok = parser_peek(p), start = ptok;
	unsigned int spos = parser_pos(p);

	if (token_accept(ptok, lex::token_kind::UNDERSCORE)) {
		parser_pop(p);

		return ast::pat_wildcard_init(metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::OPEN_PAR)) {
		type lex::token* opar = ptok;
		parser_pop(p);

		unsigned int ignore_kind = 0x0;

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
			parser_pop(p);

			type util::vector* elems = util::vector_init(sizeof{type ast::pat*}, deref_pat_free);
			return ast::pat_tuple_init(ast::tuple_pat_init(ast::tuple_ignore_kind::NONE, elems,
				NULL as type util::vector*, metadata_parser_init(spos, p)));
		}
		else if (token_accept(ptok, lex::token_kind::DOT_DOT_DOT)) {
			type lex::token* dots = ptok;
			ignore_kind = ast::tuple_ignore_kind::BEGINNING;
			parser_pop(p);

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::COMMA)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a comma after a '...' to enumerate tuple elements.", dots);
				return NULL as type ast::pat*;
			}
			parser_pop(p);

		}
		else 
			ignore_kind = ast::tuple_ignore_kind::NONE;

		type ast::pat* initial = parse_pat(p);
		if (initial == NULL as type ast::pat*)
			return NULL as type ast::pat*;

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
			parser_pop(p);

			if (ignore_kind != ast::tuple_ignore_kind::NONE) {
				type util::vector* elems = util::vector_init(sizeof{type ast::pat*}, deref_pat_free);
				util::vector_append(elems, initial$ as byte*);

				return ast::pat_tuple_init(ast::tuple_pat_init(ignore_kind, elems,
					NULL as type util::vector*, metadata_parser_init(spos, p)));
			}
			return ast::pat_nested_init(initial, metadata_parser_init(spos, p));
		}

		type util::vector* first = util::vector_init(sizeof{type ast::pat*}, deref_pat_free),
			last = NULL as type util::vector*;
		type util::vector* to_use = first;

		util::vector_append(to_use, initial$ as byte*);

		ptok = parser_peek(p);
		while (token_accept(ptok, lex::token_kind::COMMA)) {
			parser_pop(p);

			ptok = parser_peek(p);
			if (token_accept(ptok, lex::token_kind::CLOSE_PAR))
				break;
			else if (token_accept(ptok, lex::token_kind::DOT_DOT_DOT)) {
				if (ignore_kind != ast::tuple_ignore_kind::NONE) {
					util::report_token(util::error_kind::ERROR,
						"Cannot have multiple '...''s in a tuple pattern.", ptok);
					return NULL as type ast::pat*;
				}
				parser_pop(p);

				to_use = last = util::vector_init(sizeof{type ast::pat*}, deref_pat_free);

				ptok = parser_peek(p);
				if (token_accept(ptok, lex::token_kind::COMMA))
					ignore_kind = ast::tuple_ignore_kind::MIDDLE;
				else
					ignore_kind = ast::tuple_ignore_kind::END;
			}
			else {
				type ast::pat* curr = parse_pat(p);
				if (curr == NULL as type ast::pat*)
					return NULL as type ast::pat*;
				util::vector_append(to_use, curr$ as byte*);
			}

			ptok = parser_peek(p);
		}

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a close parenthesis (')') here to end a tuple pattern.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match an open parenthesis ('(') here.", opar);
			return NULL as type ast::pat*;
		}
		parser_pop(p);

		return ast::pat_tuple_init(ast::tuple_pat_init(ignore_kind, first, last, metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::MUT)) {
		type lex::token* tok_mut = ptok;
		parser_pop(p);

		ptok = parser_peek(p);
		type lex::token* ident = ptok;
		if (!token_accept(ptok, lex::token_kind::IDENT)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a 'mut' modified on an identifier pattern after here.", tok_mut);
			return NULL as type ast::pat*;
		}
		parser_pop(p);

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::MINUS_GT)) {
			parser_pop(p);
			type ast::typ* tt = parse_typ(p);
			if (tt == NULL as type ast::typ*)
				return NULL as type ast::pat*;
			return ast::pat_ident_init(ast::ident_pat_init(true, ident, tt, metadata_parser_init(spos, p)));
		}

		return ast::pat_ident_init(ast::ident_pat_init(true, ident, NULL as type ast::typ*,
			metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::INTEGER)
		|| token_accept(ptok, lex::token_kind::DECIMAL)
		|| token_accept(ptok, lex::token_kind::STRING)
		|| token_accept(ptok, lex::token_kind::CHARACTER)
		|| token_accept(ptok, lex::token_kind::TRUE)
		|| token_accept(ptok, lex::token_kind::FALSE)) {
		parser_pop(p);

		return ast::pat_literal_init(ptok, metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::OPEN_BRACKET)) {
		type lex::token* obracket = ptok;
		parser_pop(p);

		type ast::range_bound* start_bound = NULL as type ast::range_bound*;
		unsigned int sbspos = parser_pos(p);

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::COLON_COLON)
			|| token_accept(ptok, lex::token_kind::IDENT)) {
			type ast::qualified_identifier* qi = parse_qualified_identifier(p);
			if (qi == NULL as type ast::qualified_identifier*) {
				util::report_token(util::error_kind::ERROR,
					"Invalid qualified identifier starting here for a start bound of a range.", ptok);
				return NULL as type ast::pat*;
			}

			bool inclusive = token_accept(ptok, lex::token_kind::EQ);
			if (inclusive)
				parser_pop(p);

			start_bound = ast::range_bound_qualified_identifier_init(qi, inclusive,
				metadata_parser_init(sbspos, p));
		}
		else if (token_accept(ptok, lex::token_kind::INTEGER)
			|| token_accept(ptok, lex::token_kind::CHARACTER)) {
			type lex::token* lit = ptok;
			parser_pop(p);

			ptok = parser_peek(p);
			bool inclusive = token_accept(ptok, lex::token_kind::EQ);
			if (inclusive)
				parser_pop(p);

			start_bound = ast::range_bound_literal_init(lit, inclusive,
				metadata_parser_init(sbspos, p));
		}

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::DOT_DOT_DOT)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a '...' to specify a range after a valid integral (or absent) start bound for a range.", ptok);
			return NULL as type ast::pat*;
		}
		parser_pop(p);

		ptok = parser_peek(p);
		type ast::range_bound* end_bound = NULL as type ast::range_bound*;
		unsigned int ebspos = parser_pos(p);
		bool inclusive = token_accept(ptok, lex::token_kind::EQ);
		if (inclusive)
			parser_pop(p);

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::COLON_COLON)
			|| token_accept(ptok, lex::token_kind::IDENT)) {
			type ast::qualified_identifier* qi = parse_qualified_identifier(p);
			if (qi == NULL as type ast::qualified_identifier*) {
				util::report_token(util::error_kind::ERROR,
					"Invalid qualified identifier starting here for the end bound of a range.", ptok);
				return NULL as type ast::pat*;
			}

			end_bound = ast::range_bound_qualified_identifier_init(qi, inclusive,
				metadata_parser_init(ebspos, p));
		}
		else if (token_accept(ptok, lex::token_kind::INTEGER)
			|| token_accept(ptok, lex::token_kind::CHARACTER)) {
			parser_pop(p);

			end_bound = ast::range_bound_literal_init(ptok, inclusive,
				metadata_parser_init(ebspos, p));
		}

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::CLOSE_BRACKET)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a close bracket (']') here to end a range.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match an open bracket ('[') here.", obracket);
			return NULL as type ast::pat*;
		}
		parser_pop(p);

		return ast::pat_range_init(ast::range_pat_init(start_bound, end_bound, 
			metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::OPEN_BRACE)) {
		type lex::token* obrace = ptok;
		parser_pop(p);

		type ast::exp* e = parse_exp(p);
		if (e == NULL as type ast::exp*)
			return NULL as type ast::pat*;

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::CLOSE_BRACE)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a close brace ('}') here to end an expression pattern.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match an open brace ('{') here.", obrace);
			return NULL as type ast::pat*;
		}
		parser_pop(p);

		return ast::pat_exp_init(e, metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::IDENT)
		|| token_accept(ptok, lex::token_kind::COLON_COLON)) {
		type lex::token* first = ptok;
		parser_pop(p);

		ptok = parser_peek(p);
		if (token_accept(first, lex::token_kind::IDENT)
			&& !token_accept(ptok, lex::token_kind::COLON_COLON)
			&& !token_accept(ptok, lex::token_kind::OPEN_PAR)
			&& !token_accept(ptok, lex::token_kind::OPEN_BRACE)) {
			if (token_accept(ptok, lex::token_kind::MINUS_GT)) {
				parser_pop(p);

				type ast::typ* tt = parse_typ(p);
				if (tt == NULL as type ast::typ*)
					return NULL as type ast::pat*;
				return ast::pat_ident_init(ast::ident_pat_init(false, first, tt,
					metadata_parser_init(spos, p)));
			}

			return ast::pat_ident_init(ast::ident_pat_init(false, first, NULL as type ast::typ*,
				metadata_parser_init(spos, p)));
		}

		parser_set_pos(p, spos);
		type lex::token* ptok = parser_peek(p), start = ptok;

		type ast::qualified_identifier* qi = parse_qualified_identifier(p);
		if (qi == NULL as type ast::qualified_identifier*) {
			util::report_token(util::error_kind::ERROR,
				"Invalid qualified identifier starting here for a 'struct' or 'variant' name.", start);
			return NULL as type ast::pat*;
		}

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::OPEN_PAR)) {
			type lex::token* opar = ptok;
			parser_pop(p);

			type util::vector* nested = util::vector_init(sizeof{type ast::pat*}, deref_pat_free);

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
				while (true) {
					type ast::pat* curr_pat = parse_pat(p);
					if (curr_pat == NULL as type ast::pat*)
						return NULL as type ast::pat*;

					util::vector_append(nested, curr_pat$ as byte*);

					ptok = parser_peek(p);
					if (token_accept(ptok, lex::token_kind::COMMA))
						parser_pop(p), ptok = parser_peek(p);
					else
						break;
				}
			}

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a close parenthesis (')') here to end a pattern constructor list.", ptok);
				util::report_token(util::error_kind::NOTE,
					"To match an open parenthesis ('(') here.", opar);
				return NULL as type ast::pat*;
			}
			parser_pop(p);

			return ast::pat_constructor_init(ast::constructor_pat_init(qi,
				nested, metadata_parser_init(spos, p)));
		}
		else if (token_accept(ptok, lex::token_kind::OPEN_BRACE)) {
			type lex::token* obrace = ptok;
			parser_pop(p);

			type util::vector* members = util::vector_init(sizeof{type ast::member_pat*}, deref_member_pat_free);

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::CLOSE_BRACE)) {
				while (true) {
					unsigned int mpspos = parser_pos(p);
					ptok = parser_peek(p);

					bool is_mut = token_accept(ptok, lex::token_kind::MUT);
					if (is_mut)
						parser_pop(p);

					ptok = parser_peek(p);
					type lex::token* dot = ptok;
					if (!token_accept(ptok, lex::token_kind::DOT)) {
						util::report_token(util::error_kind::ERROR,
							"Expected a dot ('.') here to start a member pattern.", ptok);
						return NULL as type ast::pat*;
					}
					parser_pop(p);

					ptok = parser_peek(p);
					type lex::token* member = ptok;
					if (!token_accept(ptok, lex::token_kind::IDENT)) {
						util::report_token(util::error_kind::ERROR,
							"Expected a member identifier after here to indicate the member that a pattern is being applied to.", dot);
						return NULL as type ast::pat*;
					}
					parser_pop(p);

					ptok = parser_peek(p);
					if (token_accept(ptok, lex::token_kind::COLON) && !is_mut) {
						parser_pop(p);

						type ast::pat* nested = parse_pat(p);
						if (nested == NULL as type ast::pat*)
							return NULL as type ast::pat*;

						type ast::member_pat* mp = ast::member_pat_init(is_mut, member, nested,
							metadata_parser_init(mpspos, p));
						util::vector_append(members, mp$ as byte*);
					}
					else {
						type ast::member_pat* mp = ast::member_pat_init(is_mut, member, NULL as type ast::pat*,
							metadata_parser_init(mpspos, p));
						util::vector_append(members, mp$ as byte*);
					}

					ptok = parser_peek(p);
					if (token_accept(ptok, lex::token_kind::COMMA))
						parser_pop(p), ptok = parser_peek(p);
					else
						break;
				}
			}

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::CLOSE_BRACE)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a close brace ('}') here to end a struct pattern.", ptok);
				util::report_token(util::error_kind::NOTE,
					"To match an open brace ('{') here.", obrace);
				return NULL as type ast::pat*;
			}
			parser_pop(p);

			return ast::pat_struct_init(ast::struct_pat_init(qi, members, metadata_parser_init(spos, p)));
		}

		return ast::pat_constructor_init(ast::constructor_pat_init(qi,
			NULL as type util::vector*, metadata_parser_init(spos, p)));
	}

	util::report_token(util::error_kind::ERROR,
		"Invalid pattern starting here.", start);
	return NULL as type ast::pat*;
}

} } // namespace neutrino::parse
