import "parse/parser.hsp"

import <"std/io">
import <"std/lib">

import "ast/ast.hsp"
import "parse/util.hsp"
import "util/vector.hsp"
import "util/error.hsp"
import "lex/token.hsp"
import "util/generic_funcs.hsp"

using std::io::printf;
using std::lib::NULL;

namespace neutrino { namespace parse {

func type ast::function* parse_fun(type parser* p, bool is_lambda) {
	type lex::token* ptok = parser_peek(p);
	unsigned int spos = parser_pos(p);
	type lex::token* start = ptok;

	if (is_lambda) {
		if (!token_accept(ptok, lex::token_kind::BACKSLASH)) {
			util::ice("parse_fun",
				"This should be unreachable (for a lambda)!");
		}
	}
	else {
		if (!token_accept(ptok, lex::token_kind::FUN)) {
			util::ice("parse_fun",
				"This should be unreachable (for a function)!");
		}
	}
	parser_pop(p);

	ptok = parser_peek(p);
	type lex::token* name = NULL as type lex::token*;
	if (!is_lambda) {
		name = ptok;
		if (!token_accept(ptok, lex::token_kind::IDENT)) {
			util::report_token(util::error_kind::ERROR,
				"Expected an identifier to name a function here.", ptok);
			return NULL as type ast::function*;
		}
		parser_pop(p);
	}
	else
		name = NULL as type lex::token*;

	ptok = parser_peek(p);
	type lex::token* opar = ptok;
	if (!token_accept(ptok, lex::token_kind::OPEN_PAR)) {
		util::report_token(util::error_kind::ERROR,
			"Expected an open parenthesis ('(') here to start a function's parameter list.", ptok);
		return NULL as type ast::function*;
	}
	parser_pop(p);

	type util::vector* parameters = util::vector_init(sizeof{type ast::decl*},
		deref_decl_free);
	bool is_variadic = false;

	ptok = parser_peek(p);
	while (!token_accept(ptok, lex::token_kind::CLOSE_PAR)
		&& !token_accept(ptok, lex::token_kind::EOF)) {
		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::DOT_DOT_DOT)) {
			parser_pop(p);
			is_variadic = true;
			break;
		}

		type ast::decl* d = parse_decl(p, true);
		if (d == NULL as type ast::decl*)
			return NULL as type ast::function*;
		util::vector_append(parameters, d$ as byte*);

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::COMMA))
			parser_pop(p);
		else
			break;

		ptok = parser_peek(p);
	}

	ptok = parser_peek(p);
	type lex::token* cpar = ptok;
	if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
		util::report_token(util::error_kind::ERROR,
			"Expected a close parenthesis (')') here to end a function's parameter list.", ptok);
		util::report_token(util::error_kind::NOTE,
			"To match an open parenthesis ('(') here.", opar);
		return NULL as type ast::function*;
	}
	parser_pop(p);

	ptok = parser_peek(p);
	type lex::token* colon = ptok;
	if (!token_accept(ptok, lex::token_kind::COLON)) {
		util::report_token(util::error_kind::ERROR,
			"Expected a colon (':') after here to denote the return type of a function.", cpar);
		return NULL as type ast::function*;
	}
	parser_pop(p);

	ptok = parser_peek(p);
	type ast::typ* rt = parse_typ(p);
	if (rt == NULL as type ast::typ*)
		return NULL as type ast::function*;

	ptok = parser_peek(p);
	if (token_accept(ptok, lex::token_kind::SEMICOLON) && !is_lambda) {
		parser_pop(p);
		return ast::function_init(NULL as type ast::attribute*,
			name, parameters, is_variadic, rt, NULL as type util::vector*,
			NULL as type ast::stmt*, metadata_parser_init(spos, p));
	}

	type util::vector* capture_list = NULL as type util::vector*;
	ptok = parser_peek(p);
	if (is_lambda && token_accept(ptok, lex::token_kind::USING)) {
		type lex::token* start_cl = ptok;
		parser_pop(p);

		capture_list = util::vector_init(sizeof{type ast::capture_data*}, deref_capture_data_free);

		ptok = parser_peek(p);
		type lex::token* cl_opar = ptok;
		if (!token_accept(ptok, lex::token_kind::OPEN_PAR)) {
			util::report_token(util::error_kind::ERROR,
				"Expected an open parenthesis ('(') after here to start a capture list.", start_cl);
			return NULL as type ast::function*;
		}
		parser_pop(p);

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
			while (true) {
				unsigned int cdspos = parser_pos(p);
				bool is_mut = token_accept(ptok, lex::token_kind::MUT);
				if (is_mut)
					parser_pop(p);

				ptok = parser_peek(p);
				bool is_pointer = token_accept(ptok, lex::token_kind::STAR);
				bool is_pointee_mut = false;
				if (is_pointer) {
					parser_pop(p);

					ptok = parser_peek(p);
					is_pointee_mut = token_accept(ptok, lex::token_kind::MUT);
					if (is_pointee_mut)
						parser_pop(p);
				}
				else
					is_pointee_mut = false;

				ptok = parser_peek(p);
				type lex::token* ident = ptok;
				if (!token_accept(ptok, lex::token_kind::IDENT)) {
					util::report_token(util::error_kind::ERROR,
						"Expected an identifier here to capture.", ptok);
					return NULL as type ast::function*;
				}
				parser_pop(p);

				type ast::capture_data* cd = ast::capture_data_init(is_mut, is_pointer, is_pointee_mut,
					ident, metadata_parser_init(cdspos, p));
				util::vector_append(capture_list, cd$ as byte*);

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
				"Expected a close parenthesis (')') here to end a capture list.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match an open parenthesis ('(') here.", cl_opar);
			return NULL as type ast::function*;
		}
		parser_pop(p);
	}
	else
		capture_list = NULL as type util::vector*;

	ptok = parser_peek(p);
	if (!token_accept(ptok, lex::token_kind::OPEN_BRACE)) {
		util::report_token(util::error_kind::ERROR,
			"Expected an open brace ('{') here to start a function definition.", ptok);
		return NULL as type ast::function*;
	}

	type ast::stmt* body = parse_compound(p);
	if (body == NULL as type ast::stmt*)
		return NULL as type ast::function*;
	return ast::function_init(NULL as type ast::attribute*,
		name, parameters, is_variadic, rt, capture_list,
		body, metadata_parser_init(spos, p));
}

func[static] type ast::top_level* parse_namespace_or_alias(type parser* p) {
	type lex::token* ptok = parser_peek(p);
	unsigned int spos = parser_pos(p);
	type lex::token* nstok = ptok;

	if (!token_accept(ptok, lex::token_kind::NAMESPACE)) {
		util::ice("parse_namespace",
			"This should be unreachable!");
	}
	parser_pop(p);

	type ast::qualified_identifier* name = parse_qualified_identifier(p);
	if (name == NULL as type ast::qualified_identifier*) {
		util::report_token(util::error_kind::ERROR,
			"Expected a valid qualified identifier after here for a namespace name.", nstok);
		return NULL as type ast::top_level*;
	}

	ptok = parser_peek(p);
	if (token_accept(ptok, lex::token_kind::SEMICOLON)) {
		parser_pop(p);
		return ast::top_level_namespace_init(ast::namespace_decls_init(NULL as type ast::attribute*,
			name, NULL as type util::vector*, metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::OPEN_BRACE)) {
		type lex::token* obrace = ptok;
		parser_pop(p);

		type util::vector* decls = util::vector_init(sizeof{type ast::top_level*},
			deref_top_level_free);

		ptok = parser_peek(p);
		while (!token_accept(ptok, lex::token_kind::CLOSE_BRACE)
			&& !token_accept(ptok, lex::token_kind::EOF)) {
			type ast::top_level* tl = parse_top_level(p);
			if (tl == NULL as type ast::top_level*)
				return NULL as type ast::top_level*;

			util::vector_append(decls, tl$ as byte*);

			ptok = parser_peek(p);
		}

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::CLOSE_BRACE)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a close brace ('}') to end a namespace definition here.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match an open brace ('{') here.", obrace);
			return NULL as type ast::top_level*;
		}
		parser_pop(p);

		return ast::top_level_namespace_init(ast::namespace_decls_init(NULL as type ast::attribute*,
			name, decls, metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::EQ)) {
		if (util::vector_size(name->name) != 1) {
			util::report_token(util::error_kind::ERROR,
				"Expected an unqualified identifier to assign to for a namespace alias.", nstok);
			return NULL as type ast::top_level*;
		}
		type lex::token* short_name = util::vector_at(name->name, util::vector_size(name->name) - 1)
			as type lex::token** @;

		type lex::token* eq = ptok;
		parser_pop(p);

		type ast::qualified_identifier* fns = parse_qualified_identifier(p);
		if (fns == NULL as type ast::qualified_identifier*) {
			util::report_token(util::error_kind::ERROR,
				"Expected a valid namespace name to alias from after here.", eq);
			return NULL as type ast::top_level*;
		}

		type ast::namespace_alias* na = ast::namespace_alias_init(NULL as type ast::attribute*,
			short_name, fns, metadata_parser_init(spos, p));
		
		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a semicolon (';') here to end a namespace alias.", ptok);
			return NULL as type ast::top_level*;
		}
		parser_pop(p);

		return ast::top_level_stmt_init(ast::stmt_namespace_alias_init(na,
			metadata_parser_init(spos, p)));
	}

	util::report_token(util::error_kind::ERROR,
		"Expected a semicolon (';') for a namespace declaration, open brace ('{') for a namespace definition, or equals ('=') for a namespace alias here.", ptok);
	return NULL as type ast::top_level*;
}

func type ast::top_level* parse_top_level(type parser* p) {
	unsigned int spos = parser_pos(p);
	type lex::token* ptok = parser_peek(p);

	type ast::attribute* attr = NULL as type ast::attribute*;
	if (token_accept(ptok, lex::token_kind::AT)) {
		attr = parse_attribute(p);
		if (attr == NULL as type ast::attribute*)
			return NULL as type ast::top_level*;
	}
	bool has_attr = attr != NULL as type ast::attribute*;

	ptok = parser_peek(p);
	if (token_accept(ptok, lex::token_kind::FUN)) {
		type ast::function* ret = parse_fun(p, false);
		if (has_attr && ret != NULL as type ast::function*) {
			ret->attribute = attr;
		}

		if (ret == NULL as type ast::function*)
			return NULL as type ast::top_level*;
		else
			return ast::top_level_fun_init(ret);
	}
	else if (token_accept(ptok, lex::token_kind::USING)) {
		type ast::stmt* s = parse_using(p);
		if (s == NULL as type ast::stmt*)
			return NULL as type ast::top_level*;
		if (has_attr)
			s->which.using_data->attribute = attr;
		return ast::top_level_stmt_init(s);
	}
	else if (token_accept(ptok, lex::token_kind::STRUCT)
		|| token_accept(ptok, lex::token_kind::ENUM)
		|| token_accept(ptok, lex::token_kind::UNION)
		|| token_accept(ptok, lex::token_kind::VARIANT)) {
		type ast::stmt* a = parse_aggregate(p);
		if (a == NULL as type ast::stmt*)
			return NULL as type ast::top_level*;
		if (has_attr)
			a->which.aggregate->attribute = attr;
		return ast::top_level_stmt_init(a);
	}
	else if (!has_attr
		&& token_accept(ptok, lex::token_kind::SEMICOLON)) {
		parser_pop(p);
		return ast::top_level_empty_init(metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::NAMESPACE)) {
		type ast::top_level* ret = parse_namespace_or_alias(p);
		if (has_attr && ret != NULL as type ast::top_level*) {
			switch (ret->kind) {
			case ast::top_level_kind::NAMESPACE:
				ret->which.namespace_decls->attribute = attr;
				break;
			case ast::top_level_kind::NAMESPACE_ALIAS:
				ret->which.stmt->which.namespace_alias->attribute = attr;
				break;
			default:
				util::ice("tck_top_level",
					"Unexpected top_level_kind while trying to insert attribute into a namespace or a namespace alias!");
			}
		}
		return ret;
	}
	else if (!has_attr
		&& token_accept(ptok, lex::token_kind::INCLUDE)) {
		type ast::stmt* s = parse_include(p);
		if (s == NULL as type ast::stmt*)
			return NULL as type ast::top_level*;
		return ast::top_level_stmt_init(s);
	}
	else if (token_accept(ptok, lex::token_kind::IMPORT)) {
		type ast::stmt* i = parse_import(p);
		if (i == NULL as type ast::stmt*)
			return NULL as type ast::top_level*;
		if (has_attr)
			i->which.import_data->attribute = attr;
		return ast::top_level_stmt_init(i);
	}
	else if (token_accept(ptok, lex::token_kind::TYPE)) {
		type ast::stmt* t = parse_type_alias(p);
		if (t == NULL as type ast::stmt*)
			return NULL as type ast::top_level*;
		if (has_attr)
			t->which.type_alias->attribute = attr;
		return ast::top_level_stmt_init(t);
	}
	else {
		if (has_attr)
			ast::attribute_delete(attr);

		parser_set_pos(p, spos);
		type ast::decl* d = parse_decl(p, false);
		if (d == NULL as type ast::decl*) {
			util::report_token(util::error_kind::ERROR,
				"Expected one of a 'fun', 'using', 'namespace', 'import'/'include', aggregate, empty, or declaration for a top-level statement starting here.",
				ptok);
			return NULL as type ast::top_level*;
		}

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a semicolon (';') here to end a top-level declaration.", ptok);
			return NULL as type ast::top_level*;
		}
		parser_pop(p);

		type ast::stmt* s = ast::stmt_decl_init(d);
		return ast::top_level_stmt_init(s);
	}

	util::report_token(util::error_kind::ERROR,
		"Invalid top-level statement starting here.", ptok);
	return NULL as type ast::top_level*;
}

func type ast::prog* parse_prog(type parser* p) {
	unsigned int spos = parser_pos(p);
	type util::vector* top_levels = util::vector_init(sizeof{type ast::top_level*},
		deref_top_level_free);

	type lex::token* ptok = parser_peek(p);
	while (!token_accept(ptok, lex::token_kind::EOF)) {
		type ast::top_level* tl = parse_top_level(p);
		if (tl == NULL as type ast::top_level*)
			return NULL as type ast::prog*;

		util::vector_append(top_levels, tl$ as byte*);
		ptok = parser_peek(p);
	}

	return ast::prog_init(top_levels, metadata_parser_init(spos, p));
}

} } // namespace neutrino::parse
