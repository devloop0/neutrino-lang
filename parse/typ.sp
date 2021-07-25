import "parse/parser.hsp"

import <"std/lib">
import <"std/io">

import "lex/token.hsp"
import "ast/ast.hsp"
import "util/error.hsp"
import "parse/util.hsp"
import "util/generic_funcs.hsp"

using std::io::printf;
using std::lib::NULL;

namespace neutrino { namespace parse {

func[static] type ast::typ_qualifiers* parse_pointer_levels(type parser* p) {
	type util::vector* qualifiers = util::vector_init(sizeof{bool}, util::no_free);

	while (true) {
		type lex::token* ptok = parser_peek(p);
		unsigned int spos = parser_pos(p);

		if (token_accept(ptok, lex::token_kind::MUT)) {
			parser_pop(p);

			ptok = parser_peek(p);
			if (token_accept(ptok, lex::token_kind::STAR)) {
				parser_pop(p);

				bool is_mut = true;
				util::vector_append(qualifiers, is_mut$ as byte*);
			}
			else {
				parser_set_pos(p, spos);
				break;
			}
		}
		else if (token_accept(ptok, lex::token_kind::STAR)) {
			parser_pop(p);

			bool is_mut = false;
			util::vector_append(qualifiers, is_mut$ as byte*);
		}
		else
			break;
	}

	return ast::typ_qualifiers_init(qualifiers);
}

func type ast::typ* parse_typ(type parser* p) {
	type lex::token* ptok = parser_peek(p), start = ptok;
	unsigned int spos = parser_pos(p);

	type ast::typ_qualifiers* tqs = parse_pointer_levels(p);
	ptok = parser_peek(p);
	bool is_mut = token_accept(ptok, lex::token_kind::MUT);
	if (is_mut)
		parser_pop(p);

	tqs->none_specified = util::vector_empty(tqs->qualifiers) && !is_mut;
	util::vector_append(tqs->qualifiers, is_mut$ as byte*);

	ptok = parser_peek(p);
	if (token_accept(ptok, lex::token_kind::SIGNED)
		|| token_accept(ptok, lex::token_kind::UNSIGNED)) {
		type lex::token* start = ptok;

		unsigned int prim_kind = 0x0;
		bool is_signed = token_accept(ptok, lex::token_kind::SIGNED);

		ptok = parser_pop(p);
		if (token_accept(ptok, lex::token_kind::CHAR)) {
			prim_kind = is_signed
				? ast::primitive_kind::SIGNED_INT
				: ast::primitive_kind::UNSIGNED_INT;
		}
		else if (token_accept(ptok, lex::token_kind::BYTE)) {
			prim_kind = is_signed
				? ast::primitive_kind::SIGNED_BYTE
				: ast::primitive_kind::UNSIGNED_BYTE;
		}
		else if (token_accept(ptok, lex::token_kind::SHORT)) {
			prim_kind = is_signed
				? ast::primitive_kind::SIGNED_SHORT
				: ast::primitive_kind::UNSIGNED_SHORT;
		}
		else if (token_accept(ptok, lex::token_kind::INT)) {
			prim_kind = is_signed
				? ast::primitive_kind::SIGNED_INT
				: ast::primitive_kind::UNSIGNED_INT;
		}
		else if (token_accept(ptok, lex::token_kind::LONG)) {
			prim_kind = is_signed
				? ast::primitive_kind::SIGNED_LONG
				: ast::primitive_kind::UNSIGNED_LONG;
		}
		else {
			util::report_token(util::error_kind::ERROR,
				"Expected a valid 'signed' or 'unsigned' type after here.", start);
			return NULL as type ast::typ*;
		}
		parser_pop(p);

		return ast::typ_primitive_init(tqs, prim_kind, metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::FLOAT)) {
		parser_pop(p);
		return ast::typ_primitive_init(tqs, ast::primitive_kind::FLOAT, metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::DOUBLE)) {
		parser_pop(p);
		return ast::typ_primitive_init(tqs, ast::primitive_kind::DOUBLE, metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::VOID)) {
		parser_pop(p);
		return ast::typ_primitive_init(tqs, ast::primitive_kind::VOID, metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::BOOL)) {
		parser_pop(p);
		return ast::typ_primitive_init(tqs, ast::primitive_kind::BOOL, metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::CHAR)) {
		parser_pop(p);
		return ast::typ_primitive_init(tqs, ast::primitive_kind::UNSIGNED_CHAR, metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::BYTE)) {
		parser_pop(p);
		return ast::typ_primitive_init(tqs, ast::primitive_kind::SIGNED_BYTE, metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::SHORT)) {
		parser_pop(p);
		return ast::typ_primitive_init(tqs, ast::primitive_kind::SIGNED_SHORT, metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::INT)) {
		parser_pop(p);
		return ast::typ_primitive_init(tqs, ast::primitive_kind::SIGNED_INT, metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::LONG)) {
		parser_pop(p);
		return ast::typ_primitive_init(tqs, ast::primitive_kind::SIGNED_LONG, metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::FN)) {
		type lex::token* start = ptok;
		ptok = parser_pop(p);
		
		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::OPEN_PAR)) {
			util::report_token(util::error_kind::ERROR,
				"Expected an open parenthesis ('(') to start an parameter type list for a function type.", ptok);
			return NULL as type ast::typ*;
		}
		ptok = parser_pop(p);

		bool is_variadic = false;
		type util::vector* par_typs = util::vector_init(sizeof{type ast::typ*},
			deref_typ_free);
		if (token_accept(ptok, lex::token_kind::DOT_DOT_DOT)) {
			is_variadic = true;
			parser_pop(p);
		}
		else {
			while (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
				ptok = parser_peek(p);

				type ast::typ* par_typ = parse_typ(p);
				if (par_typ == NULL as type ast::typ*)
					return NULL as type ast::typ*;
				util::vector_append(par_typs, par_typ$ as byte*);

				ptok = parser_peek(p);
				if (token_accept(ptok, lex::token_kind::COMMA)) {
					ptok = parser_pop(p);
					if (token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
						util::report_token(util::error_kind::ERROR,
							"Expected another parameter type after a comma (',').", ptok);
						return NULL as type ast::typ*;
					}
					else if (token_accept(ptok, lex::token_kind::DOT_DOT_DOT)) {
						is_variadic = true;
						parser_pop(p);
						break;
					}
				}
				else break;
			}
		}

		ptok = parser_peek(p);
		type lex::token* cpar = ptok;
		if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a close parenthesis (')') here to end a function type.", ptok);
			return NULL as type ast::typ*;
		}
		parser_pop(p);

		ptok = parser_peek(p);
		type lex::token* colon = ptok;
		if (!token_accept(ptok, lex::token_kind::COLON)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a colon (':') after here to specify the return type of a function type.", cpar);
			return NULL as type ast::typ*;
		}
		parser_pop(p);

		ptok = parser_peek(p);

		type ast::typ* rt = parse_typ(p);
		if (rt == NULL as type ast::typ*)
			return NULL as type ast::typ*;

		return ast::typ_fn_typ_init(tqs, ast::fn_typ_init(par_typs, is_variadic, rt),
			metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::IDENT)
		|| token_accept(ptok, lex::token_kind::COLON_COLON)) {
		type ast::qualified_identifier* qident = parse_qualified_identifier(p);
		if (qident == NULL as type ast::qualified_identifier*) {
			util::report_token(util::error_kind::ERROR,
				"Expected a valid identifier to name a type after here.", start);
			return NULL as type ast::typ*;
		}

		return ast::typ_aggregate_name_init(tqs, qident, metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::OPEN_PAR)) {
		type lex::token* opar = ptok;
		parser_pop(p);

		type ast::typ* initial = parse_typ(p);
		if (initial == NULL as type ast::typ*)
			return NULL as type ast::typ*;
		
		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
			parser_pop(p);

			util::maybe_ice(!util::vector_empty(tqs->qualifiers)
				&& !util::vector_empty(initial->typ_qualifiers->qualifiers),
				"parse_typ",
				"Expected at least one type qualifier for types here.");

			tqs->none_specified = tqs->none_specified && initial->typ_qualifiers->none_specified;
			bool* old_initial = util::vector_at(initial->typ_qualifiers->qualifiers, 0) as bool*,
				curr_final = util::vector_at(tqs->qualifiers, util::vector_size(tqs->qualifiers) - 1) as bool*;
			curr_final@ = curr_final@ || old_initial@;

			unsigned int prev_size = util::vector_size(initial->typ_qualifiers->qualifiers);
			for (unsigned int i = 1; i < prev_size; i++) {
				bool b = util::vector_at(initial->typ_qualifiers->qualifiers, i) as bool* @;
				util::vector_append(tqs->qualifiers, b$ as byte*);
			}

			ast::typ_qualifiers_delete(initial->typ_qualifiers);
			ast::metadata_delete(initial->metadata);
			initial->typ_qualifiers = tqs;
			initial->metadata = metadata_parser_init(spos, p);

			return initial;
		}

		type util::vector* tt = util::vector_init(sizeof{type ast::typ*}, deref_typ_free);
		util::vector_append(tt, initial$ as byte*);

		ptok = parser_peek(p);
		while (token_accept(ptok, lex::token_kind::COMMA)) {
			parser_pop(p);
			
			ptok = parser_peek(p);
			if (token_accept(ptok, lex::token_kind::CLOSE_PAR))
				break;

			type ast::typ* curr = parse_typ(p);
			if (curr == NULL as type ast::typ*)
				return NULL as type ast::typ*;

			util::vector_append(tt, curr$ as byte*);

			ptok = parser_peek(p);
		}

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a close parenthesis (')') here to end a tuple type.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match an open parenthesis ('(') here.", opar);
			return NULL as type ast::typ*;
		}
		parser_pop(p);

		return ast::typ_tup_init(tqs, tt, metadata_parser_init(spos, p));
	}

	util::report_token(util::error_kind::ERROR,
		"Invalid type starting here.", start);
	return NULL as type ast::typ*;
}

} } // namespace neutrino::parse
