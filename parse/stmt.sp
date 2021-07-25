import "parse/parser.hsp"

import <"std/lib">
import <"std/io">

import "ast/ast.hsp"
import "lex/token.hsp"
import "util/error.hsp"
import "parse/util.hsp"
import "util/generic_funcs.hsp"
import "util/file.hsp"
import "util/base.hsp"

using std::io::printf;
using std::lib::NULL;
using neutrino::util::n_malloc;
using neutrino::util::n_free;

namespace neutrino { namespace parse {

func[static] type ast::stmt* parse_labeled(type parser* p) {
	unsigned int spos = parser_pos(p);
	type lex::token* ptok = parser_peek(p);

	if (token_accept(ptok, lex::token_kind::CASE)) {
		type lex::token* start = ptok;
		parser_pop(p);

		type ast::exp* e = parse_exp(p);
		if (e == NULL as type ast::exp*)
			return NULL as type ast::stmt*;

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::COLON)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a colon (':') after a 'case' statement's expression starting here.", start);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		type ast::stmt* s = parse_stmt(p);
		if (s == NULL as type ast::stmt*)
			return NULL as type ast::stmt*;
		return ast::stmt_labeled_init(ast::labeled_case_init(e, s,
			metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::DEFAULT)) {
		type lex::token* start = ptok;
		parser_pop(p);

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::COLON)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a colon ('.') to start a 'default' statement after here.", start);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		type ast::stmt* s = parse_stmt(p);
		if (s == NULL as type ast::stmt*)
			return NULL as type ast::stmt*;

		return ast::stmt_labeled_init(ast::labeled_default_init(s,
			metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::LABEL)) {
		type lex::token* start = ptok;
		ptok = parser_pop(p);

		type lex::token* ident = ptok;
		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::IDENT)) {
			util::report_token(util::error_kind::ERROR,
				"Expected an identifier for a 'label' statement after here.", start);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::COLON)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a colon (':') to start a label statement after here.", ident);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		type ast::stmt* s = parse_stmt(p);
		if (s == NULL as type ast::stmt*)
			return NULL as type ast::stmt*;

		return ast::stmt_labeled_init(ast::labeled_label_init(ident, s,
			metadata_parser_init(spos, p)));
	}
	
	return NULL as type ast::stmt*;
}

func type ast::decl* parse_decl(type parser* p, bool single) {
	type lex::token* ptok = parser_peek(p);

	type ast::attribute* attr = NULL as type ast::attribute*;
	if (token_accept(ptok, lex::token_kind::AT)) {
		attr = parse_attribute(p);
		if (attr == NULL as type ast::attribute*)
			return NULL as type ast::decl*;
	}

	unsigned int spos = parser_pos(p);
	ptok = parser_peek(p);
	bool is_static = token_accept(ptok, lex::token_kind::STATIC);
	if (is_static)
		parser_pop(p);

	ptok = parser_peek(p);
	bool is_const = token_accept(ptok, lex::token_kind::CONST);
	if (is_const)
		parser_pop(p);
	else if (token_accept(ptok, lex::token_kind::LET))
		parser_pop(p);
	else {
		util::report_token(util::error_kind::ERROR,
			"Expected one of a 'let' or 'const' here to denote a declaration.", ptok);
		return NULL as type ast::decl*;
	}

	type util::vector* dcs = util::vector_init(sizeof{type ast::decl_component*},
		deref_decl_component_free);

	do {
		ptok = parser_peek(p);
		unsigned int dcspos = parser_pos(p);

		type ast::pat* dcpat = parse_pat(p);
		if (dcpat == NULL as type ast::pat*)
			return NULL as type ast::decl*;

		type ast::typ* t = NULL as type ast::typ*;
		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::COLON)) {
			parser_pop(p);

			t = parse_typ(p);
			if (t == NULL as type ast::typ*)
				return NULL as type ast::decl*;
		}

		type ast::exp* init = NULL as type ast::exp*;
		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::EQ)) {
			parser_pop(p);

			init = parse_assignment_exp(p);
			if (init == NULL as type ast::exp*)
				return NULL as type ast::decl*;
		}

		type ast::decl_component* dc = ast::decl_component_init(dcpat, t, init,
			metadata_parser_init(dcspos, p), is_static, is_const);
		util::vector_append(dcs, dc$ as byte*);

		if (single)
			break;

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::COMMA))
			parser_pop(p), ptok = parser_peek(p);
		else
			break;
	} while (true);

	type ast::decl* ret = ast::decl_init(attr, is_static, is_const,
		dcs, metadata_parser_init(spos, p));
	return ret;
}

func[static] type ast::stmt* parse_decl_or_exp(type parser* p) {
	type lex::token* ptok = parser_peek(p);
	unsigned int start_pos = parser_pos(p);

	if (token_accept(ptok, lex::token_kind::AT)) {
		util::disable_reporting();
		ptok = parser_peek(p);
		type ast::attribute* a = parse_attribute(p);
		if (a != NULL as type ast::attribute*)
			ast::attribute_delete(a);
		util::enable_reporting();
	}

	ptok = parser_peek(p);
	bool is_decl = token_accept(ptok, lex::token_kind::STATIC)
		|| token_accept(ptok, lex::token_kind::CONST)
		|| token_accept(ptok, lex::token_kind::LET);

	parser_set_pos(p, start_pos);

	if (is_decl) {
		type ast::decl* d = parse_decl(p, false);
		if (d == NULL as type ast::decl*)
			return NULL as type ast::stmt*;

		return ast::stmt_decl_init(d);
	}
	else {
		type ast::exp* e = parse_exp(p);
		if (e == NULL as type ast::exp*)
			return NULL as type ast::stmt*;

		return ast::stmt_exp_init(e);
	}

	util::ice("parse_decl_or_exp", "This should be unreachable!");
}

func[static] type ast::stmt* parse_selection(type parser* p) {
	unsigned int spos = parser_pos(p);
	type lex::token* ptok = parser_peek(p);

	if (token_accept(ptok, lex::token_kind::IF)) {
		type lex::token* start = ptok;
		parser_pop(p);

		ptok = parser_peek(p);
		type lex::token* opar = ptok;
		if (!token_accept(ptok, lex::token_kind::OPEN_PAR)) {
			util::report_token(util::error_kind::ERROR,
				"Expected an open parenthesis ('(') after here to start an if statement.", start);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		type ast::stmt* d_or_e = parse_decl_or_exp(p);
		if (d_or_e == NULL as type ast::stmt*)
			return NULL as type ast::stmt*;

		type ast::exp* e = NULL as type ast::exp*;
		type ast::decl* d = NULL as type ast::decl*;
		switch (d_or_e->kind) {
		case ast::stmt_kind::DECL: {
			d = d_or_e->which.decl;

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a semicolon (';') after a declaration for an if statement here.", ptok);
				return NULL as type ast::stmt*;
			}
			parser_pop(p);

			e = parse_exp(p);
		}
			break;
		case ast::stmt_kind::EXP:
			e = d_or_e->which.exp;
			break;
		default:
			util::ice("parse_selection",
				"Expected a decl or exp for an if statement here!");
		}

		if (e == NULL as type ast::exp*)
			return NULL as type ast::stmt*;

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a close parenthesis (')') here.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match an open parenthesis ('(') here.", opar);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		type ast::stmt* s1 = parse_stmt(p);
		if (s1 == NULL as type ast::stmt*)
			return NULL as type ast::stmt*;

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::ELSE)) {
			return ast::stmt_selection_init(ast::selection_if_init(d, e, s1,
				metadata_parser_init(spos, p)));
		}
		parser_pop(p);

		type ast::stmt* s2 = parse_stmt(p);
		if (s2 == NULL as type ast::stmt*)
			return NULL as type ast::stmt*;

		return ast::stmt_selection_init(ast::selection_if_else_init(d, e, s1, s2,
			metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::SWITCH)) {
		type lex::token* start = ptok;
		parser_pop(p);

		ptok = parser_peek(p);
		type lex::token* opar = ptok;
		if (!token_accept(ptok, lex::token_kind::OPEN_PAR)) {
			util::report_token(util::error_kind::ERROR,
				"Expected an open parenthesis ('(') after here to start a switch statement.", start);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		type ast::stmt* d_or_e = parse_decl_or_exp(p);
		if (d_or_e == NULL as type ast::stmt*)
			return NULL as type ast::stmt*;

		type ast::decl* d = NULL as type ast::decl*;
		type ast::exp* e = NULL as type ast::exp*;
		switch (d_or_e->kind) {
		case ast::stmt_kind::DECL: {
			d = d_or_e->which.decl;

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a semicolon (';') after a declaration for a switch statement here.", ptok);
				return NULL as type ast::stmt*;
			}
			parser_pop(p);

			e = parse_exp(p);
		}
			break;
		case ast::stmt_kind::EXP:
			e = d_or_e->which.exp;
			break;
		default:
			util::ice("parse_selection",
				"Expected a decl or exp here for a switch statement!");
		}

		if (e == NULL as type ast::exp*)
			return NULL as type ast::stmt*;

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a close parenthesis (')') here.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match an open parenthesis here.", opar);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		type ast::stmt* s = parse_stmt(p);
		if (s == NULL as type ast::stmt*)
			return NULL as type ast::stmt*;

		return ast::stmt_selection_init(ast::selection_switch_init(d, e, s,
			metadata_parser_init(spos, p)));
	}

	util::ice("parse_selection", "This should be unreachable!");
}

func[static] type ast::stmt* parse_jump(type parser* p) {
	unsigned int spos = parser_pos(p);
	type lex::token* ptok = parser_peek(p);

	if (token_accept(ptok, lex::token_kind::GOTO)) {
		type lex::token* start = ptok;
		parser_pop(p);

		ptok = parser_peek(p);
		type lex::token* ident = ptok;
		if (!token_accept(ptok, lex::token_kind::IDENT)) {
			util::report_token(util::error_kind::ERROR,
				"Expected an identifier here to 'goto'.", ptok);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a semicolon (';') to end a goto statement.", start);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		return ast::stmt_jump_init(ast::jump_goto_init(ident,
			metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::RETURN)
		|| token_accept(ptok, lex::token_kind::ERR_RETURN)
		|| token_accept(ptok, lex::token_kind::DEFER)
		|| token_accept(ptok, lex::token_kind::ERR_DEFER)) {
		unsigned int k = 0x0;
		if (token_accept(ptok, lex::token_kind::RETURN))
			k = ast::jump_kind::RETURN;
		else if (token_accept(ptok, lex::token_kind::ERR_RETURN))
			k = ast::jump_kind::ERR_RETURN;
		else if (token_accept(ptok, lex::token_kind::DEFER))
			k = ast::jump_kind::DEFER;
		else
			k = ast::jump_kind::ERR_DEFER;
		
		type lex::token* start = ptok;
		parser_pop(p);

		ptok = parser_peek(p);

		if (k == ast::jump_kind::RETURN || k == ast::jump_kind::ERR_RETURN) {
			if (token_accept(ptok, lex::token_kind::SEMICOLON)) {
				parser_pop(p);
				return ast::stmt_jump_init(ast::jump_defer_return_init(
					k, NULL as type ast::exp*, metadata_parser_init(spos, p)));
			}
		}

		type ast::exp* e = parse_exp(p);
		if (e == NULL as type ast::exp*)
			return NULL as type ast::stmt*;

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a semicolon (';') to end a return statement.", start);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		return ast::stmt_jump_init(ast::jump_defer_return_init(k, e, metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::BREAK)
		|| token_accept(ptok, lex::token_kind::CONTINUE)) {
		type lex::token* start = ptok;
		bool is_break = token_accept(ptok, lex::token_kind::BREAK);

		ptok = parser_pop(p);
		if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a semicolon (';') to end a break or continue statement starting here.", start);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		type ast::metadata* m = metadata_parser_init(spos, p);
		return ast::stmt_jump_init(is_break ? ast::jump_break_init(m) : ast::jump_continue_init(m));
	}

	util::ice("parse_jump", "This should be unreachable!");
}

func type ast::stmt* parse_compound(type parser* p) {
	unsigned int spos = parser_pos(p);
	type lex::token* ptok = parser_peek(p);

	if (!token_accept(ptok, lex::token_kind::OPEN_BRACE))
		util::ice("parse_compound", "This should be unreachable!");
	parser_pop(p);
	type lex::token* start = ptok;

	type util::vector* stmts = util::vector_init(sizeof{type ast::stmt*},
		deref_stmt_free);

	ptok = parser_peek(p);
	while (!token_accept(ptok, lex::token_kind::CLOSE_BRACE)
		&& !token_accept(ptok, lex::token_kind::EOF)) {
		type ast::stmt* s = parse_stmt(p);
		if (s == NULL as type ast::stmt*)
			return NULL as type ast::stmt*;

		util::vector_append(stmts, s$ as byte*);
		ptok = parser_peek(p);
	}

	ptok = parser_peek(p);
	if (token_accept(ptok, lex::token_kind::EOF)) {
		util::report_token(util::error_kind::ERROR,
			"Expected a close brace ('}') to end a compound statement here.", ptok);
		util::report_token(util::error_kind::NOTE,
			"To match an open brace ('{') here.", start);
		return NULL as type ast::stmt*;
	}

	util::maybe_ice(token_accept(ptok, lex::token_kind::CLOSE_BRACE),
		"parse_compound", "Expected a close brace at this point!");
	parser_pop(p);
	return ast::stmt_compound_init(ast::compound_init(stmts, metadata_parser_init(spos, p)));
}

func type ast::stmt* parse_aggregate(type parser* p) {
	unsigned int spos = parser_pos(p);
	type lex::token* ptok = parser_peek(p);

	if (token_accept(ptok, lex::token_kind::STRUCT)
		|| token_accept(ptok, lex::token_kind::UNION)
		|| token_accept(ptok, lex::token_kind::VARIANT)) {
		type lex::token* start = ptok;
		unsigned int k = token_accept(ptok, lex::token_kind::STRUCT)
			? ast::aggregate_kind::STRUCT
			: (token_accept(ptok, lex::token_kind::VARIANT)
				? ast::aggregate_kind::VARIANT : ast::aggregate_kind::UNION);

		ptok = parser_pop(p);
		type lex::token* name = NULL as type lex::token*;
		if (token_accept(ptok, lex::token_kind::IDENT)) {
			parser_pop(p);
			name = ptok;
		}

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::SEMICOLON)) { 
			parser_pop(p);
			if (name == NULL as type lex::token*) {
				util::report_token(util::error_kind::ERROR,
					"Cannot forward-declare an anonymous struct/union/variant.", start);
				return NULL as type ast::stmt*;
			}

			return ast::stmt_aggregate_init(ast::aggregate_init(NULL as type ast::attribute*,
				k, name, NULL as type util::vector*, metadata_parser_init(spos, p)));
		}

		type lex::token* obrace = ptok;
		if (!token_accept(ptok, lex::token_kind::OPEN_BRACE)) {
			util::report_token(util::error_kind::ERROR,
				"Expected an open brace ('{') to start an aggregate.", start);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		type util::vector* members = util::vector_init(sizeof{type ast::aggregate_member*}, deref_aggregate_member_free);

		ptok = parser_peek(p);
		while (!token_accept(ptok, lex::token_kind::CLOSE_BRACE)
			&& !token_accept(ptok, lex::token_kind::EOF)) {
			ptok = parser_peek(p);
			type ast::attribute* attr = NULL as type ast::attribute*;
			if (token_accept(ptok, lex::token_kind::AT)) {
				attr = parse_attribute(p);
				if (attr == NULL as type ast::attribute*)
					return NULL as type ast::stmt*;
			}

			ptok = parser_peek(p);
			unsigned int amspos = parser_pos(p);

			type lex::token* initial_ident = ptok;
			if (!token_accept(ptok, lex::token_kind::IDENT)
				&& !token_accept(ptok, lex::token_kind::UNDERSCORE)) {
				util::report_token(util::error_kind::ERROR,
					"Expected an identifier or '_' to start a member declaration here.", ptok);
				return NULL as type ast::stmt*;
			}
			parser_pop(p);

			type util::vector* idents = util::vector_init(sizeof{type lex::token*}, util::no_free);
			util::vector_append(idents, initial_ident$ as byte*);

			if (!token_accept(initial_ident, lex::token_kind::UNDERSCORE)) {
				ptok = parser_peek(p);
				while (token_accept(ptok, lex::token_kind::COMMA)) {
					parser_pop(p);

					type lex::token* ident = parser_peek(p);
					if (!token_accept(ident, lex::token_kind::IDENT)) {
						util::report_token(util::error_kind::ERROR,
							"Expected an identifier here to continue a member list.", ident);
						return NULL as type ast::stmt*;
					}
					parser_pop(p);

					util::vector_append(idents, ident$ as byte*);

					ptok = parser_peek(p);
				}
			}

			ptok = parser_peek(p);
			if (token_accept(ptok, lex::token_kind::SEMICOLON)) {
				parser_pop(p);
				type ast::aggregate_member* am = ast::aggregate_member_none_init(attr, idents,
					metadata_parser_init(amspos, p));
				util::vector_append(members, am$ as byte*);
			}
			else {
				if (!token_accept(ptok, lex::token_kind::COLON)) {
					util::report_token(util::error_kind::ERROR,
						"Expected a colon (':') after a member list to start a member specification.", initial_ident);
					return NULL as type ast::stmt*;
				}
				parser_pop(p);

				ptok = parser_peek(p);
				type ast::aggregate_member* am = NULL as type ast::aggregate_member*;
				if (token_accept(ptok, lex::token_kind::AT)
					|| token_accept(ptok, lex::token_kind::STRUCT)
					|| token_accept(ptok, lex::token_kind::UNION)
					|| token_accept(ptok, lex::token_kind::ENUM)
					|| token_accept(ptok, lex::token_kind::VARIANT)) {
					type ast::attribute* iattr = NULL as type ast::attribute*;
					if (token_accept(ptok, lex::token_kind::AT)) {
						iattr = parse_attribute(p);
						if (iattr == NULL as type ast::attribute*)
							return NULL as type ast::stmt*;
					}

					type ast::stmt* a = parse_aggregate(p);
					if (a == NULL as type ast::stmt*)
						return NULL as type ast::stmt*;
					a->which.aggregate->attribute = iattr;

					am = ast::aggregate_member_aggregate_init(attr, idents, a, NULL as type ast::metadata*);
				}
				else {
					type ast::typ* t = parse_typ(p);
					if (t == NULL as type ast::typ*)
						return NULL as type ast::stmt*;

					ptok = parser_peek(p);
					if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
						util::report_token(util::error_kind::ERROR,
							"Expected a semicolon (';') here to end a variant member type specification.", ptok);
						return NULL as type ast::stmt*;
					}
					parser_pop(p);

					am = ast::aggregate_member_typ_init(attr, idents, t, NULL as type ast::metadata*);
				}

				am->metadata = metadata_parser_init(amspos, p);

				util::vector_append(members, am$ as byte*);
			}

			ptok = parser_peek(p);
		}

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::EOF)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a close brace ('}') to end this struct/union/variant declaration.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match an open brace ('{') here.", obrace);
			return NULL as type ast::stmt*;
		}

		util::maybe_ice(token_accept(ptok, lex::token_kind::CLOSE_BRACE),
			"parse_aggregate", "Expected a close brace here to end a struct/union/variant!");
		parser_pop(p);

		return ast::stmt_aggregate_init(ast::aggregate_init(NULL as type ast::attribute*,
			k, name, members, metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::ENUM)) {
		type lex::token* start = ptok;
		parser_pop(p);

		ptok = parser_peek(p);
		type lex::token* name = ptok;
		if (!token_accept(ptok, lex::token_kind::IDENT)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a name for an enum after here.", start);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::SEMICOLON)) {
			parser_pop(p);
			if (name == NULL as type lex::token*) {
				util::report_token(util::error_kind::ERROR,
					"Cannot forward-declare an anonymous enum.", start);
				return NULL as type ast::stmt*;
			}

			return ast::stmt_aggregate_init(ast::aggregate_init(NULL as type ast::attribute*,
				ast::aggregate_kind::ENUM, name, NULL as type util::vector*, metadata_parser_init(spos, p)));
		}

		
		type lex::token* obrace = ptok;
		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::OPEN_BRACE)) {
			util::report_token(util::error_kind::ERROR,
				"Expected an open brace ('{') to start an enum.", start);
			return NULL as type ast::stmt*;
		}

		type util::vector* members = util::vector_init(sizeof{type lex::token*},
			util::no_free);

		ptok = parser_pop(p);
		while (!token_accept(ptok, lex::token_kind::CLOSE_BRACE)
			&& !token_accept(ptok, lex::token_kind::EOF)) {
			ptok = parser_peek(p);
			type lex::token* ident = ptok;
			if (!token_accept(ptok, lex::token_kind::IDENT)) {
				util::report_token(util::error_kind::ERROR,
					"Expected an identifier for an enum member here.", ptok);
				return NULL as type ast::stmt*;
			}
			parser_pop(p);

			util::vector_append(members, ident$ as byte*);

			ptok = parser_peek(p);
			if (token_accept(ptok, lex::token_kind::COMMA))
				parser_pop(p);
			else
				break;
			ptok = parser_peek(p);
		}

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::EOF)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a close brace ('}') here to end an enum statement.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match an open brace ('{') here.", obrace);
			return NULL as type ast::stmt*;
		}

		util::maybe_ice(token_accept(ptok, lex::token_kind::CLOSE_BRACE),
			"parse_aggregate", "Expected a close brace here while parsing an enum statement!");
		parser_pop(p);

		return ast::stmt_aggregate_init(ast::aggregate_init(NULL as type ast::attribute*,
			ast::aggregate_kind::ENUM, name, members, metadata_parser_init(spos, p)));
	}

	util::ice("parse_aggregate", "This should be unreachable!");
}

func[static] type ast::stmt* parse_iteration(type parser* p) {
	unsigned int spos = parser_pos(p);
	type lex::token* ptok = parser_peek(p);

	if (token_accept(ptok, lex::token_kind::FOR)) {
		type lex::token* start = ptok;
		parser_pop(p);

		ptok = parser_peek(p);
		type lex::token* opar = ptok;
		if (!token_accept(ptok, lex::token_kind::OPEN_PAR)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a open parenthesis ('(') after here to start a for statement.", start);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		type ast::stmt* d_or_e = NULL as type ast::stmt*;
		type ast::exp* cond = NULL as type ast::exp*;
		type ast::exp* update = NULL as type ast::exp*;

		{
			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
				d_or_e = parse_decl_or_exp(p);
				if (d_or_e == NULL as type ast::stmt*)
					return NULL as type ast::stmt*;
			}

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a semicolon (';') here to end the update part of a for statement header.", ptok);
				return NULL as type ast::stmt*;
			}
			parser_pop(p);
		}

		{
			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
				cond = parse_exp(p);
				if (cond == NULL as type ast::exp*)
					return NULL as type ast::stmt*;
			}

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a semicolon (';') here to end the condition part of a for statement header.", ptok);
				return NULL as type ast::stmt*;
			}
			parser_pop(p);
		}

		{
			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
				update = parse_exp(p);
				if (update == NULL as type ast::exp*)
					return NULL as type ast::stmt*;
			}


			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a close parenthesis (')') here.", ptok);
				util::report_token(util::error_kind::NOTE,
					"To match an open parenthesis ('(') here.", opar);
				return NULL as type ast::stmt*;
			}
			parser_pop(p);
		}

		type ast::stmt* s = parse_stmt(p);
		if (s == NULL as type ast::stmt*)
			return NULL as type ast::stmt*;

		if (d_or_e == NULL as type ast::stmt*) {
			return ast::stmt_iteration_init(ast::iteration_for_exp_init(
				NULL as type ast::exp*, cond, update, s,
				metadata_parser_init(spos, p)));
		}
		else {
			util::maybe_ice(d_or_e->kind == ast::stmt_kind::DECL
				|| d_or_e->kind == ast::stmt_kind::EXP,
				"parse_iteration",
				"Expected a declaration of an expression for a for header initialization!");

			bool is_decl = d_or_e->kind == ast::stmt_kind::DECL;
			if (is_decl) {
				return ast::stmt_iteration_init(ast::iteration_for_decl_init(
					d_or_e->which.decl, cond, update, s,
					metadata_parser_init(spos, p)));
			}
			else {
				return ast::stmt_iteration_init(ast::iteration_for_exp_init(
					d_or_e->which.exp, cond, update, s,
					metadata_parser_init(spos, p)));
			}
		}
	}
	else if (token_accept(ptok, lex::token_kind::WHILE)) {
		type lex::token* start = ptok;
		parser_pop(p);

		ptok = parser_peek(p);
		type lex::token* opar = ptok;
		if (!token_accept(ptok, lex::token_kind::OPEN_PAR)) {
			util::report_token(util::error_kind::ERROR,
				"Expected an open parenthesis ('(') after here to start a while statement.", start);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		type ast::exp* e = parse_exp(p);
		if (e == NULL as type ast::exp*)
			return NULL as type ast::stmt*;

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::CLOSE_BRACE)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a close parenthesis (')') here to finish a while statement header.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match an open parenthesis ('(') here.", opar);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		type ast::stmt* s = parse_stmt(p);
		if (s == NULL as type ast::stmt*)
			return NULL as type ast::stmt*;

		return ast::stmt_iteration_init(ast::iteration_while_init(e, s,
			metadata_parser_init(spos, p)));
	}
	else if (token_accept(ptok, lex::token_kind::DO)) {
		parser_pop(p);

		type ast::stmt* s = parse_stmt(p);
		if (s == NULL as type ast::stmt*)
			return NULL as type ast::stmt*;

		ptok = parser_peek(p);
		type lex::token* wtok = ptok;
		if (!token_accept(ptok, lex::token_kind::WHILE)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a 'while' condition for a do-while statement.", ptok);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		ptok = parser_peek(p);
		type lex::token* opar = ptok;
		if (!token_accept(ptok, lex::token_kind::OPEN_PAR)) {
			util::report_token(util::error_kind::ERROR,
				"Expected an open parenthesis ('(') after here to start a condition for a do-while statement.", wtok);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		type ast::exp* e = parse_exp(p);
		if (e == NULL as type ast::exp*)
			return NULL as type ast::stmt*;

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a close parenthesis (')') here.", ptok);
			util::report_token(util::error_kind::NOTE,
				"To match an open parenthesis ('(') here.", opar);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a semicolon (';') here to end a do-while statement.", ptok);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		return ast::stmt_iteration_init(ast::iteration_do_init(s, e,
			metadata_parser_init(spos, p)));
	}

	util::ice("parse_iteration", "This should be unreachable!");
}

func type ast::stmt* parse_using(type parser* p) {
	unsigned int spos = parser_pos(p);
	type lex::token* ptok = parser_peek(p);
	type lex::token* start = ptok;

	if (!token_accept(ptok, lex::token_kind::USING))
		util::ice("parse_using", "This should be unreachable!");

	bool has_namespace = false;
	ptok = parser_pop(p);
	if (token_accept(ptok, lex::token_kind::NAMESPACE)) {
		parser_pop(p);
		has_namespace = true;
	}

	ptok = parser_peek(p);
	type ast::qualified_identifier* v = parse_qualified_identifier(p);
	if (v == NULL as type ast::qualified_identifier*) {
		util::report_token(util::error_kind::ERROR,
			"Expected a valid identifier for a using statement after here.", start);
		return NULL as type ast::stmt*;
	}

	ptok = parser_peek(p);
	if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
		util::report_token(util::error_kind::ERROR,
			"Expected a semicolon (';') to end a using statement after here.", start);
		return NULL as type ast::stmt*;
	}
	parser_pop(p);

	type ast::using_data* ud = ast::using_data_init(NULL as type ast::attribute*,
		v, metadata_parser_init(spos, p));

	unsigned int k = has_namespace ? ast::stmt_kind::USING_NAMESPACE : ast::stmt_kind::USING;
	return ast::stmt_using_init(k, ud);
}

func[static] type ast::stmt* parse_namespace_alias(type parser* p) {
	type lex::token* ptok = parser_peek(p);
	unsigned int spos = parser_pos(p);
	type lex::token* nstok = ptok;

	if (!token_accept(ptok, lex::token_kind::NAMESPACE)) {
		util::ice("parse_namespace_alias",
			"This should be unreachable!");
	}
	parser_pop(p);

	ptok = parser_peek(p);
	type lex::token* name = ptok;
	if (!token_accept(ptok, lex::token_kind::IDENT)) {
		util::report_token(util::error_kind::ERROR,
			"Expected an identifier for a namespace alias after here.", nstok);
		return NULL as type ast::stmt*;
	}
	parser_pop(p);

	ptok = parser_peek(p);
	type lex::token* eq = ptok;
	if (!token_accept(ptok, lex::token_kind::EQ)) {
		util::report_token(util::error_kind::ERROR,
			"Expected an equals ('=') for a namespace alias here.", ptok);
		return NULL as type ast::stmt*;
	}
	parser_pop(p);

	type ast::qualified_identifier* fns = parse_qualified_identifier(p);
	if (fns == NULL as type ast::qualified_identifier*) {
		util::report_token(util::error_kind::ERROR,
			"Expected a valid namespace name to alias from after here.", eq);
		return NULL as type ast::stmt*;
	}

	type ast::namespace_alias* na = ast::namespace_alias_init(NULL as type ast::attribute*,
		name, fns, metadata_parser_init(spos, p));
		
	ptok = parser_peek(p);
	if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
		util::report_token(util::error_kind::ERROR,
			"Expected a semicolon (';') here to end a namespace alias.", ptok);
		return NULL as type ast::stmt*;
	}
	parser_pop(p);

	return ast::stmt_namespace_alias_init(na, metadata_parser_init(spos, p));
}

func[static] type ast::stmt* parse_asm(type parser* p) {
	type lex::token* ptok = parser_peek(p);
	type lex::token* start = ptok;
	unsigned int spos = parser_pos(p);

	util::maybe_ice(token_accept(ptok, lex::token_kind::ASM),
		"parse_asm",
		"This should be unreachable!");

	parser_pop(p);

	ptok = parser_peek(p);
	type lex::token* opar = ptok;
	if (!token_accept(ptok, lex::token_kind::OPEN_PAR)) {
		util::report_token(util::error_kind::ERROR,
			"Expected an open parenthesis ('(') after here to start an 'asm' statement.", start);
		return NULL as type ast::stmt*;
	}
	parser_pop(p);

	type util::vector* ac_vec = util::vector_init(sizeof{type ast::asm_component*},
		deref_asm_component_free);
	ptok = parser_peek(p);
	if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
		while (true) {
			unsigned int ac_spos = parser_pos(p);
			type lex::token* string = ptok;
			if (!token_accept(ptok, lex::token_kind::STRING)) {
				util::report_token(util::error_kind::ERROR,
					"Expected a string here for an assembly instruction inside of an 'asm' statement.", ptok);
				return NULL as type ast::stmt*;
			}
			parser_pop(p);

			type ast::qualified_identifier* qi = NULL as type ast::qualified_identifier*;
			ptok = parser_peek(p);
			if (token_accept(ptok, lex::token_kind::COLON_COLON)
				|| token_accept(ptok, lex::token_kind::IDENT)) {
				qi = parse_qualified_identifier(p);
				if (qi == NULL as type ast::qualified_identifier*) {
					util::report_token(util::error_kind::ERROR,
						"Expected a colon (':') to delimit an assembly instruction or a qualified identifier to refer to here.", ptok);
					return NULL as type ast::stmt*;
				}
			}

			type ast::asm_component* ac = ast::asm_component_init(string, qi,
				metadata_parser_init(ac_spos, p));
			util::vector_append(ac_vec, ac$ as byte*);

			ptok = parser_peek(p);
			if (!token_accept(ptok, lex::token_kind::COLON))
				break;
			ptok = parser_pop(p);
		}
	}

	ptok = parser_peek(p);
	type lex::token* cpar = ptok;
	if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
		util::report_token(util::error_kind::ERROR,
			"Expected a close parenthesis (')') here to end an 'asm' statement.", ptok);
		util::report_token(util::error_kind::NOTE,
			"To match an open parenthesis ('(') here.", opar);
		return NULL as type ast::stmt*;
	}
	parser_pop(p);

	ptok = parser_peek(p);
	if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
		util::report_token(util::error_kind::ERROR,
			"Expected a semicolon (';') to end an 'asm' statement after here.", cpar);
		return NULL as type ast::stmt*;
	}
	parser_pop(p);

	return ast::stmt_asm_init(ac_vec, metadata_parser_init(spos, p));
}

func type ast::stmt* parse_include(type parser* p) {
	type lex::token* ptok = parser_peek(p);
	unsigned int spos = parser_pos(p);

	type lex::token* start = ptok;

	util::maybe_ice(token_accept(start, lex::token_kind::INCLUDE),
		"parse_include",
		"This should be unreachable!");

	parser_pop(p);

	type util::vector* includes = util::vector_init(sizeof{type lex::token*}, util::no_free);
	type util::vector* lexers = util::vector_init(sizeof{type lex::lexer*}, util::no_free);
	type util::vector* inodes = util::vector_init(sizeof{unsigned int}, util::no_free);

	while (true) {
		ptok = parser_peek(p);
		type lex::token* curr = ptok;
		if (!token_accept(ptok, lex::token_kind::IDENT)) {
			util::report_token(util::error_kind::ERROR,
				"Expected an identifier representing a local file name to include here.", ptok);
		}
		parser_pop(p);

		util::vector_append(includes, curr$ as byte*);

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::COMMA))
			parser_pop(p), ptok = parser_peek(p);
		else
			break;
	}

	ptok = parser_peek(p);
	if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
		util::report_token(util::error_kind::ERROR,
			"Expected a semicolon (';') here to end an include statement.", start);
		return NULL as type ast::stmt*;
	}
	parser_pop(p);

	type ast::stmt* ret = ast::stmt_include_init(includes, metadata_parser_init(spos, p));
		
	for (unsigned int i = util::vector_size(includes); i > 0; i--) {
		type lex::token* curr = util::vector_at(includes, i - 1) as type lex::token** @;

		type util::string* resolved = resolve_include(p, curr);
		if (!util::file_exists(util::string_data(resolved))) {
			type util::string* err_str = util::string_init("Could not resolve include: \"");
			util::string_catc(err_str, curr->text);
			util::string_catc(err_str, "\" specified here to a file.");

			util::report_token(util::error_kind::ERROR,
				util::string_data(err_str), curr);
			util::string_delete(err_str);
			return NULL as type ast::stmt*;
		}

		unsigned int file_inode = util::get_inode(util::string_data(resolved));
		char* file_text = util::read_file(util::string_data(resolved));
		if (file_inode == -1 || file_text == NULL as char*) {
			type util::string* err_str = util::string_init("Could not read include: \"");
			util::string_catc(err_str, curr->text);
			util::string_catc(err_str, "\" specified here.");

			util::report_token(util::error_kind::ERROR,
				util::string_data(err_str), curr);
			util::string_delete(err_str);
			return NULL as type ast::stmt*;
		}

		type lex::lexer* lexer = lex::lexer_init(
			resolved, file_inode,
			util::string_init_move(file_text));
		parser_add_lexer(p, lexer);
	}

	return ret;
}

func type ast::stmt* parse_import(type parser* p) {
	type lex::token* ptok = parser_peek(p);
	unsigned int spos = parser_pos(p);

	util::maybe_ice(token_accept(ptok, lex::token_kind::IMPORT),
		"parse_import",
		"This should be unreachable!");

	parser_pop(p);

	ptok = parser_peek(p);
	// TODO
}

func type ast::stmt* parse_type_alias(type parser* p) {
	type lex::token* ptok = parser_peek(p), start = ptok;
	unsigned int spos = parser_pos(p);

	util::maybe_ice(token_accept(ptok, lex::token_kind::TYPE),
		"parse_type_alias",
		"This should be unreachable!");
	parser_pop(p);

	ptok = parser_peek(p);
	if (token_accept(ptok, lex::token_kind::SEMICOLON)) {
		util::report_token(util::error_kind::ERROR,
			"Cannot have an empty type alias list starting here.", start);
		return NULL as type ast::stmt*;
	}

	type util::vector* aliases = util::vector_init(sizeof{type ast::type_alias_value*},
		deref_type_alias_value_free);

	ptok = parser_peek(p);
	while (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
		type lex::token* ident = ptok;
		unsigned int tavspos = parser_pos(p);
		if (!token_accept(ptok, lex::token_kind::IDENT)) {
			util::report_token(util::error_kind::ERROR,
				"Expected an identifier after here to name a type alias.", start);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		ptok = parser_peek(p);
		type lex::token* eq = ptok;
		if (!token_accept(ptok, lex::token_kind::EQ)) {
			util::report_token(util::error_kind::ERROR,
				"Expected an equals ('=') after here to assign to a type alias.", ident);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		type ast::typ* underlying = parse_typ(p);
		if (underlying == NULL as type ast::typ*)
			return NULL as type ast::stmt*;

		type ast::type_alias_value* tav = ast::type_alias_value_init(ident, underlying,
			metadata_parser_init(tavspos, p));
		util::vector_append(aliases, tav$ as byte*);

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::COMMA))
			parser_pop(p), ptok = parser_peek(p);
		else
			break;
	}

	ptok = parser_peek(p);
	if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
		util::report_token(util::error_kind::ERROR,
			"Expected a semicolon (';') here to end a type alias.", ptok);
		return NULL as type ast::stmt*;
	}
	parser_pop(p);

	return ast::stmt_type_alias_init(ast::type_alias_init(NULL as type ast::attribute*, aliases,
		metadata_parser_init(spos, p)));
}

func[static] type ast::stmt* parse_match(type parser* p) {
	type lex::token* ptok = parser_peek(p), start = ptok;
	unsigned int spos = parser_pos(p);

	util::maybe_ice(token_accept(ptok, lex::token_kind::MATCH),
		"parse_match",
		"This should be unreachable!");
	parser_pop(p);

	ptok = parser_peek(p);
	type lex::token* opar = ptok;
	if (!token_accept(ptok, lex::token_kind::OPEN_PAR)) {
		util::report_token(util::error_kind::ERROR,
			"Expected an open parenthesis ('(') after here to start a match statement's expression.", start);
		return NULL as type ast::stmt*;
	}
	parser_pop(p);

	type ast::exp* parent = parse_exp(p);
	if (parent == NULL as type ast::exp*)
		return NULL as type ast::stmt*;

	ptok = parser_peek(p);
	type lex::token* cpar = ptok;
	if (!token_accept(ptok, lex::token_kind::CLOSE_PAR)) {
		util::report_token(util::error_kind::ERROR,
			"Expected a close parenthesis (')') here to end a match statement's expression.", ptok);
		util::report_token(util::error_kind::NOTE,
			"To match an open parenthesis ('(') here.", opar);
		return NULL as type ast::stmt*;
	}
	parser_pop(p);

	ptok = parser_peek(p);
	type lex::token* obrace = parser_peek(p);
	if (!token_accept(ptok, lex::token_kind::OPEN_BRACE)) {
		util::report_token(util::error_kind::ERROR,
			"Expected an open brace ('{') after here to start a match statement's body.", cpar);
		return NULL as type ast::stmt*;
	}
	parser_pop(p);

	type util::vector* match_branches = util::vector_init(sizeof{type ast::match_branch*}, deref_match_branch_free);

	ptok = parser_peek(p);
	while (!token_accept(ptok, lex::token_kind::CLOSE_BRACE)) {
		unsigned int mbspos = parser_pos(p);

		type util::vector* pats = util::vector_init(sizeof{type ast::typ*}, deref_pat_free);

		type ast::pat* initial = parse_pat(p);
		if (initial == NULL as type ast::pat*)
			return NULL as type ast::stmt*;

		util::vector_append(pats, initial$ as byte*);

		ptok = parser_peek(p);
		while (token_accept(ptok, lex::token_kind::BAR)) {
			parser_pop(p);

			type ast::pat* curr_pat = parse_pat(p);
			if (curr_pat == NULL as type ast::pat*)
				return NULL as type ast::stmt*;

			util::vector_append(pats, curr_pat$ as byte*);

			ptok = parser_peek(p);
		}

		type ast::exp* guard = NULL as type ast::exp*;

		ptok = parser_peek(p);
		if (token_accept(ptok, lex::token_kind::IF)) {
			parser_pop(p);

			guard = parse_exp(p);
			if (guard == NULL as type ast::exp*)
				return NULL as type ast::stmt*;
		}
		else
			guard = NULL as type ast::exp*;

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::COLON)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a colon (':') here to start a match body.", ptok);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		type ast::stmt* body = parse_stmt(p);
		if (body == NULL as type ast::stmt*)
			return NULL as type ast::stmt*;

		type ast::match_branch* mb = ast::match_branch_init(pats, guard, body,
			metadata_parser_init(mbspos, p));
		util::vector_append(match_branches, mb$ as byte*);

		ptok = parser_peek(p);
	}

	ptok = parser_peek(p);
	if (!token_accept(ptok, lex::token_kind::CLOSE_BRACE)) {
		util::report_token(util::error_kind::ERROR,
			"Expected a close brace ('}') here to end a match statement.", ptok);
		util::report_token(util::error_kind::NOTE,
			"To match an open brace ('{') here.", obrace);
		return NULL as type ast::stmt*;
	}
	parser_pop(p);

	return ast::stmt_match_data_init(ast::match_data_init(parent, match_branches,
		metadata_parser_init(spos, p)));
}

func type ast::stmt* parse_stmt(type parser* p) {
	type lex::token* ptok = parser_peek(p);
	unsigned int spos = parser_pos(p);

	type ast::attribute* attr = NULL as type ast::attribute*;
	if (token_accept(ptok, lex::token_kind::AT)) {
		attr = parse_attribute(p);
		if (attr == NULL as type ast::attribute*)
			return NULL as type ast::stmt*;
	}
	bool has_attr = attr != NULL as type ast::attribute*;

	ptok = parser_peek(p);
	if (!has_attr
		&& (token_accept(ptok, lex::token_kind::LABEL)
			|| token_accept(ptok, lex::token_kind::CASE)
			|| token_accept(ptok, lex::token_kind::DEFAULT))) {
		return parse_labeled(p);
	}
	else if (!has_attr
		&& (token_accept(ptok, lex::token_kind::IF)
			|| token_accept(ptok, lex::token_kind::SWITCH))) {
		return parse_selection(p);
	}
	else if (!has_attr
		&& (token_accept(ptok, lex::token_kind::GOTO)
			|| token_accept(ptok, lex::token_kind::BREAK)
			|| token_accept(ptok, lex::token_kind::CONTINUE)
			|| token_accept(ptok, lex::token_kind::RETURN)
			|| token_accept(ptok, lex::token_kind::ERR_RETURN)
			|| token_accept(ptok, lex::token_kind::DEFER)
			|| token_accept(ptok, lex::token_kind::ERR_DEFER))) {
		return parse_jump(p);
	}
	else if (!has_attr
		&& token_accept(ptok, lex::token_kind::OPEN_BRACE)) {
		return parse_compound(p);
	}
	else if (token_accept(ptok, lex::token_kind::STRUCT)
		|| token_accept(ptok, lex::token_kind::UNION)
		|| token_accept(ptok, lex::token_kind::VARIANT)
		|| token_accept(ptok, lex::token_kind::ENUM)) {
		type ast::stmt* ret = parse_aggregate(p);
		if (ret != NULL as type ast::stmt* && has_attr) {
			type ast::aggregate* a = ret->which.aggregate;
			a->attribute = attr;
		}
		return ret;
	}
	else if (!has_attr
		&& (token_accept(ptok, lex::token_kind::FOR)
			|| token_accept(ptok, lex::token_kind::WHILE)
			|| token_accept(ptok, lex::token_kind::DO))) {
		return parse_iteration(p);
	}
	else if (!has_attr
		&& token_accept(ptok, lex::token_kind::MATCH)) {
		return parse_match(p);
	}
	else if (!has_attr
		&& (token_accept(ptok, lex::token_kind::ASM))) {
		return parse_asm(p);
	}
	else if (token_accept(ptok, lex::token_kind::USING)) {
		type ast::stmt* ret = parse_using(p);
		if (ret != NULL as type ast::stmt* && has_attr) {
			type ast::using_data* ud = ret->which.using_data;
			ud->attribute = attr;
		}
		return ret;
	}
	else if (token_accept(ptok, lex::token_kind::NAMESPACE)) {
		type ast::stmt* ret = parse_namespace_alias(p);
		if (has_attr && ret != NULL as type ast::stmt*) {
			type ast::namespace_alias* na = ret->which.namespace_alias;
			na->attribute = attr;
		}
		return ret;
	}
	else if (!has_attr
		&& token_accept(ptok, lex::token_kind::SEMICOLON)) {
		parser_pop(p);
		return ast::stmt_empty_init(metadata_parser_init(spos, p));
	}
	else if (token_accept(ptok, lex::token_kind::IMPORT)) {
		type ast::stmt* ret = parse_import(p);
		if (has_attr && ret != NULL as type ast::stmt*) {
			type ast::import_data* id = ret->which.import_data;
			id->attribute = attr;
		}
		return ret;
	}
	else if (token_accept(ptok, lex::token_kind::TYPE)) {
		type ast::stmt* ret = parse_type_alias(p);
		if (has_attr && ret != NULL as type ast::stmt*) {
			type ast::type_alias* ta = ret->which.type_alias;
			ta->attribute = attr;
		}
		return ret;
	}
	else if (!has_attr
		&& token_accept(ptok, lex::token_kind::INCLUDE)) {
		return parse_include(p);
	}
	else {
		if (has_attr)
			ast::attribute_delete(attr);

		parser_set_pos(p, spos);
		type ast::stmt* r = parse_decl_or_exp(p);
		if (r == NULL as type ast::stmt*)
			return NULL as type ast::stmt*;

		ptok = parser_peek(p);
		if (!token_accept(ptok, lex::token_kind::SEMICOLON)) {
			util::report_token(util::error_kind::ERROR,
				"Expected a semicolon (';') here to end a statement.", ptok);
			return NULL as type ast::stmt*;
		}
		parser_pop(p);

		return r;
	}

	util::report_token(util::error_kind::ERROR,
		"Invalid statement starting here.", ptok);
	return NULL as type ast::stmt*;
}

} } // namespace neutrino::parse
