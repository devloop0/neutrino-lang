import "tck/tck.hsp"

import <"std/io">
import <"std/lib">

import "tck/symtab.hsp"
import "util/error.hsp"
import "ast/ast.hsp"
import "util/vector.hsp"
import "lex/token.hsp"
import "util/lib.hsp"
import "util/base.hsp"
import "util/hash_table.hsp"
import "tck/util.hsp"

using std::lib::NULL;
using std::io::printf;
using neutrino::util::n_strdup;

namespace neutrino { namespace tck {

func[static] bool tck_selection(type symtab* ctx, type ast::selection* s) {
	switch (s->kind) {
	case ast::selection_kind::IF_ELSE:
	case ast::selection_kind::IF: {
		type symtab* if_ctx = symtab_init(symtab_kind::LOCAL, ctx->name,
			ctx->global, ctx);

		if (s->decl != NULL as type ast::decl*) {
			if (!tck_decl(if_ctx, s->decl, false))
				return false;
		}

		if (!tck_exp(if_ctx, s->exp))
			return false;

		if (!is_primitive(s->exp->typ, ast::primitive_kind::BOOL)) {
			type util::string* err_str = util::string_init("Expected a boolean type for an if condition; found '");
			typ_human_readable(s->exp->typ, err_str);
			util::string_catc(err_str, "' instead.");

			type ast::metadata m;
			m.token_stream = s->metadata->token_stream;
			m.start = s->metadata->start;
			m.end = s->exp->metadata->end + 1;

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				m$, (s->exp->metadata->start - m.start),
				(m.end - m.start) - 2, true);
			util::string_delete(err_str);
			return false;
		}

		if (!tck_stmt(if_ctx, s->stmt1))
			return false;
		
		s->ctx1 = if_ctx;
		util::vector_append(ctx->unnamed_children, if_ctx$ as byte*);

		if (s->stmt2 != NULL as type ast::stmt*) {
			type symtab* else_ctx = symtab_init(symtab_kind::LOCAL, ctx->name,
				ctx->global, ctx);

			if (!tck_stmt(else_ctx, s->stmt2))
				return false;

			s->ctx2 = else_ctx;
			util::vector_append(ctx->unnamed_children, else_ctx$ as byte*);
		}

		return true;
	}
		break;
	case ast::selection_kind::SWITCH: {
		type symtab* switch_ctx = symtab_init(symtab_kind::SWITCH, ctx->name,
			ctx->global, ctx);

		if (s->decl != NULL as type ast::decl*) {
			if (!tck_decl(switch_ctx, s->decl, false))
				return false;
		}

		if (!tck_exp(switch_ctx, s->exp))
			return false;

		if (is_primitive(s->exp->typ, ast::primitive_kind::VOID)) {
			type ast::metadata m;
			m.token_stream = s->metadata->token_stream;
			m.start = s->metadata->start;
			m.end = s->exp->metadata->end + 1;

			util::report_ast_metadata(util::error_kind::ERROR,
				"Expected a non-'void' type for a switch statement's expression.",
				m$, s->exp->metadata->start - m.start, (m.end - m.start) - 2, true);
			return false;
		}

		switch_ctx->symtab_info = symtab_info_switch_init(s, typ_copy(s->exp->typ));

		if (!tck_stmt(switch_ctx, s->stmt1))
			return false;

		s->ctx1 = switch_ctx;
		util::vector_append(ctx->unnamed_children, switch_ctx$ as byte*);
		return true;
	}
		break;
	default:
		util::ice("tck_selection",
			"Unrecognized selection_kind while tck'ing!");
	}

	return false;
}

func[static] bool tck_labeled(type symtab* ctx, type ast::labeled* l) {
	switch (l->kind) {
	case ast::labeled_kind::LABEL:
		return tck_stmt(ctx, l->stmt);
	case ast::labeled_kind::DEFAULT:
	case ast::labeled_kind::CASE: {
		char* op_name = NULL as char*;
		if (l->kind == ast::labeled_kind::CASE)
			op_name = "case";
		else
			op_name = "default";

		type symtab* iter = ctx;
		bool hit = false;
		while (iter != NULL as type symtab*) {
			if (iter->kind == symtab_kind::SWITCH) {
				hit = true;
				break;
			}

			iter = iter->parent;
		}

		if (l->kind == ast::labeled_kind::CASE) {
			if (!tck_exp(ctx, l->exp))
				return false;
		}

		type ast::metadata m;
		m.token_stream = l->metadata->token_stream;
		m.start = l->metadata->start;
		if (l->kind == ast::labeled_kind::CASE)
			m.end = l->exp->metadata->end + 1;
		else
			m.end = m.start + 2;

		if (!hit) {
			type util::string* err_str = util::string_init("Expected a '");
			util::string_catc(err_str, op_name);
			util::string_catc(err_str, "' statement to be nested inside a switch statement.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str), m$, 0, 0, true);
			util::string_delete(err_str);
			return false;
		}

		if (l->kind == ast::labeled_kind::CASE) {
			util::maybe_ice(iter->symtab_info != NULL as type symtab_info*
				&& iter->symtab_info->kind == symtab_info_kind::SWITCH
				&& iter->symtab_info->which.switch_stmt != NULL as type ast::selection*,
				"tck_labeled",
				"Expected a non-NULL switch_stmt for a switch ctx!");

			type ast::selection* sw = iter->symtab_info->which.switch_stmt;
			type ast::typ* sw_typ = iter->symtab_info->resolved_parent_typ;

			if (!typ_compare(ctx, l->exp->typ, sw_typ, false, true)) {
				type util::string* err_str = util::string_init("Incompatible types between a 'case' statement's expression and the parent switch statement's expression; found '");
				typ_human_readable(l->exp->typ, err_str);
				util::string_catc(err_str, "' and '");
				typ_human_readable(sw_typ, err_str);
				util::string_catc(err_str, "' respectively.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str), m$, 1, (m.end - m.start) - 2, true);
				util::string_delete(err_str);

				type ast::metadata m_ref;
				m_ref.token_stream = sw->metadata->token_stream;
				m_ref.start = sw->metadata->start;
				m_ref.end = sw->exp->metadata->end + 1;
				util::report_ast_metadata(util::error_kind::NOTE,
					"Switch statement's expression is here.",
					m_ref$, sw->exp->metadata->start - m_ref.start,
					(m_ref.end - m_ref.start) - 2, true);

				return false;
			}
		}

		type symtab* cd_ctx = symtab_init(symtab_kind::LOCAL, ctx->name,
			ctx->global, ctx);
		if (!tck_stmt(cd_ctx, l->stmt))
			return false;

		l->ctx = cd_ctx;
		util::vector_append(ctx->unnamed_children, cd_ctx$ as byte*);
		return true;
	}
		break;
	default:
		util::ice("tck_labeled",
			"Unrecognized labeled_kind while tck'ing!");
	}

	return false;
}

func[static] bool tck_jump(type symtab* ctx, type ast::jump* j) {
	switch (j->kind) {
	case ast::jump_kind::RETURN:
	case ast::jump_kind::ERR_RETURN:
	case ast::jump_kind::DEFER:
	case ast::jump_kind::ERR_DEFER: {
		char* op_name = NULL as char*;
		if (j->kind == ast::jump_kind::RETURN)
			op_name = "return";
		else if (j->kind == ast::jump_kind::ERR_RETURN)
			op_name = "err_return";
		else if (j->kind == ast::jump_kind::DEFER)
			op_name = "defer";
		else
			op_name = "err_defer";

		type symtab* iter = ctx;
		bool hit = false;
		while (iter != NULL as type symtab*) {
			if (iter->kind == symtab_kind::FUN
				|| iter->kind == symtab_kind::LAMBDA) {
				hit = true;
				break;
			}

			iter = iter->parent;
		}

		util::maybe_ice(hit, "tck_jump",
			"Expected a defer/err_defer/return/err_return statement to be nested inside of a function at this point!");

		if (j->kind == ast::jump_kind::RETURN || j->kind == ast::jump_kind::ERR_RETURN) {
			type ast::typ* rtyp = NULL as type ast::typ*;

			if (j->exp == NULL as type ast::exp*) {
				rtyp = ast::typ_primitive_init(mut_non_pointer_typ_qualifiers_init(),
					ast::primitive_kind::VOID, NULL as type ast::metadata*);
			}
			else {
				if (!tck_exp(ctx, j->exp))
					return false;
				rtyp = typ_copy(j->exp->typ);
			}

			util::maybe_ice(iter->symtab_info != NULL as type symtab_info*
				&& iter->symtab_info->kind == symtab_info_kind::FUN
				&& iter->symtab_info->which.function != NULL as type ast::function*,
				"tck_jump",
				"Expected a function symtab_info at this point!");

			type ast::function* f = iter->symtab_info->which.function;
			type ast::typ* ftyp = iter->symtab_info->resolved_parent_typ;

			if (!typ_compare(ctx, rtyp, ftyp, false, true)) {
				type ast::metadata m_ref;
				m_ref.token_stream = f->metadata->token_stream;
				m_ref.start = f->metadata->start;
				m_ref.end = ftyp->metadata->end + 1;

				type util::string* err_str = util::string_init("Incompatible types between a");
				if (j->kind == ast::jump_kind::ERR_RETURN)
					util::string_catc(err_str, "n");
				util::string_catc(err_str, " '");
				util::string_catc(err_str, op_name);
				util::string_catc(err_str, "' statement and a function's return type; found '");
				typ_human_readable(rtyp, err_str);
				util::string_catc(err_str, "' and '");
				typ_human_readable(ftyp, err_str);
				util::string_catc(err_str, "' respectively.");

				if (j->exp == NULL as type ast::exp*) {
					util::report_ast_metadata(util::error_kind::ERROR,
						util::string_data(err_str),
						j->metadata, 0, (j->metadata->end - j->metadata->start) - 1, false);
				}
				else {
					util::report_ast_metadata(util::error_kind::ERROR,
						util::string_data(err_str),
						j->metadata, j->exp->metadata->start - j->metadata->start,
						(j->exp->metadata->end - j->metadata->start) - 1, true);
				}
				util::string_delete(err_str);

				util::report_ast_metadata(util::error_kind::NOTE,
					"Function's return type declared here.",
					m_ref$, ftyp->metadata->start - m_ref.start,
					(m_ref.end - m_ref.start) - 2, true);

				return false;
			}
		}
		else {
			util::maybe_ice(j->exp != NULL as type ast::exp*,
				"tck_jump",
				"Expected a non-NULL expression for an err_defer/defer statement at this point!");

			if (!tck_exp(ctx, j->exp))
				return false;
		}

		return true;
	}
		break;
	case ast::jump_kind::BREAK:
	case ast::jump_kind::CONTINUE: {
		char* op_name = NULL as char*, scope_name = NULL as char*;
		if (j->kind == ast::jump_kind::BREAK)
			op_name = "break", scope_name = "switch or loop";
		else
			op_name = "continue", scope_name = "loop";

		bool err = true;
		type symtab* iter = ctx;
		while (iter != NULL as type symtab*) {
			if (j->kind == ast::jump_kind::BREAK) {
				if (iter->kind == symtab_kind::SWITCH
					|| iter->kind == symtab_kind::LOOP) {
					err = false;
					break;
				}
			}
			else {
				if (iter->kind == symtab_kind::LOOP) {
					err = false;
					break;
				}
			}

			iter = iter->parent;
		}

		if (err) {
			type util::string* err_str = util::string_init("Expected a '");
			util::string_catc(err_str, op_name);
			util::string_catc(err_str, "' statement inside of a ");
			util::string_catc(err_str, scope_name);
			util::string_catc(err_str, " scope.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				j->metadata, 0, (j->metadata->end - j->metadata->start) - 1, false);
			util::string_delete(err_str);
			return false;
		}

		return true;
	}
		break;
	case ast::jump_kind::GOTO: {
		bool hit = false;
		type symtab* iter = ctx;
		while (iter != NULL as type symtab*) {
			if (iter->kind == symtab_kind::FUN
				|| iter->kind == symtab_kind::LAMBDA) {
				hit = true;
				break;
			}

			iter = iter->parent;
		}

		util::maybe_ice(hit,
			"tck_jump",
			"Expected a label to be nested inside of a function at this point!");

		type symtab_value* sv = symtab_lookup_unqualified(iter, j->ident, true, true);

		if (sv == NULL as type symtab_value*) {
			type util::string* err_str = util::string_init("Label '");
			util::string_catc(err_str, j->ident->text);
			util::string_catc(err_str, "' not found.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				j->metadata, 1, 1, true);
			util::string_delete(err_str);

			return false;
		}

		if (sv->kind != symtab_value_kind::SYMBOL
			|| sv->ast_node_kind != symtab_value_ast_node_kind::LABEL) {
			type util::string* err_str = util::string_init("Symbol '");
			util::string_catc(err_str, j->ident->text);
			util::string_catc(err_str, "' does not correspond to a label (all labels are at the function-scope level).");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				j->metadata, 1, 1, true);
			util::string_delete(err_str);

			util::report_token(util::error_kind::NOTE,
				"Originally declared here.", sv->name);
				
			return false;
		}

		j->symtab_value = sv;

		return true;
	}
		break;
	default:
		util::ice("tck_jump",
			"Unrecognized jump_kind while tck'ing!");
	}

	return false;
}

func[static] bool tck_iteration(type symtab* ctx, type ast::iteration* i) {
	switch (i->kind) {
	case ast::iteration_kind::WHILE: {
		if (!tck_exp(ctx, i->exp1))
			return false;

		if (!is_primitive(i->exp1->typ, ast::primitive_kind::BOOL)) {
			type util::string* err_str = util::string_init("Expected a boolean type for a condition in a while loop; found '");
			typ_human_readable(i->exp1->typ, err_str);
			util::string_catc(err_str, "' instead.");

			type ast::metadata m;
			m.token_stream = i->metadata->token_stream;
			m.start = i->metadata->start;
			m.end = i->exp1->metadata->end + 1;

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				m$, i->exp1->metadata->start - m.start,
				(i->exp1->metadata->end - m.start) - 1, true);
			util::string_delete(err_str);

			return false;
		}

		type symtab* w_ctx = symtab_init(symtab_kind::LOOP, ctx->name,
			ctx->global, ctx);

		if (!tck_stmt(w_ctx, i->stmt))
			return false;

		i->ctx = w_ctx;
		util::vector_append(ctx->unnamed_children, w_ctx$ as byte*);
		return true;
	}
		break;
	case ast::iteration_kind::DO: {
		type symtab* d_ctx = symtab_init(symtab_kind::LOOP, ctx->name,
			ctx->global, ctx);

		if (!tck_stmt(d_ctx, i->stmt))
			return false;

		if (!tck_exp(ctx, i->exp1))
			return false;

		if (!is_primitive(i->exp1->typ, ast::primitive_kind::BOOL)) {
			type util::string* err_str = util::string_init("Expected a boolean type for a condition in a do-while loop; found '");
			typ_human_readable(i->exp1->typ, err_str);
			util::string_catc(err_str, "' instead.");

			type ast::metadata m;
			m.token_stream = i->metadata->token_stream;
			m.start = i->exp1->metadata->start - 2;
			m.end = i->exp1->metadata->end + 1;

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				m$, i->exp1->metadata->start - m.start,
				(i->exp1->metadata->end - m.start) - 1, true);
			util::string_delete(err_str);

			return false;
		}

		i->ctx = d_ctx;
		util::vector_append(ctx->unnamed_children, d_ctx$ as byte*);
		return true;
	}
		break;
	case ast::iteration_kind::FOR_DECL: {
		type symtab* f_ctx = symtab_init(symtab_kind::LOOP, ctx->name,
			ctx->global, ctx);

		if (i->decl != NULL as type ast::decl*) {
			if (!tck_decl(f_ctx, i->decl, false))
				return false;
		}

		if (i->exp1 != NULL as type ast::exp*) {
			if (!tck_exp(f_ctx, i->exp1))
				return false;

			if (!is_primitive(i->exp1->typ, ast::primitive_kind::BOOL)) {
				type util::string* err_str = util::string_init("Expected a boolean type for a condition of a for-loop; found '");
				typ_human_readable(i->exp1->typ, err_str);
				util::string_catc(err_str, "' instead.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					i->exp1->metadata, 0, 0, false);
				util::string_delete(err_str);
				return false;
			}
		}

		if (i->exp2 != NULL as type ast::exp*) {
			if (!tck_exp(f_ctx, i->exp2))
				return false;
		}

		if (!tck_stmt(f_ctx, i->stmt))
			return false;

		i->ctx = f_ctx;
		util::vector_append(ctx->unnamed_children, f_ctx$ as byte*);
		return true;
	}
		break;
	case ast::iteration_kind::FOR_EXP: {
		type symtab* f_ctx = symtab_init(symtab_kind::LOOP, ctx->name,
			ctx->global, ctx);

		if (i->exp1 != NULL as type ast::exp*) {
			if (!tck_exp(f_ctx, i->exp1))
				return false;
		}

		if (i->exp2 != NULL as type ast::exp*) {
			if (!tck_exp(f_ctx, i->exp2))
				return false;

			if (!is_primitive(i->exp2->typ, ast::primitive_kind::BOOL)) {
				type util::string* err_str = util::string_init("Expected a boolean type for a condition of a for-loop; found '");
				typ_human_readable(i->exp2->typ, err_str);
				util::string_catc(err_str, "' instead.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					i->exp2->metadata, 0, 0, false);
				util::string_delete(err_str);
				return false;
			}
		}

		if (i->exp3 != NULL as type ast::exp*) {
			if (!tck_exp(f_ctx, i->exp3))
				return false;
		}

		if (!tck_stmt(f_ctx, i->stmt))
			return false;

		i->ctx = f_ctx;
		util::vector_append(ctx->unnamed_children, f_ctx$ as byte*);
		return true;
	}
		break;
	default:
		util::ice("tck_iteration",
			"Unrecognized iteration_kind while tck'ing!");
	}

	return false;
}

func bool tck_alias(type symtab* ctx, unsigned int k,
	type lex::token* to_name, type ast::qualified_identifier* from_name,
	type ast::attribute* attr, type ast::metadata* m) {
	util::maybe_ice(k == ast::stmt_kind::USING
		|| k == ast::stmt_kind::USING_NAMESPACE
		|| k == ast::stmt_kind::NAMESPACE_ALIAS,
		"tck_using",
		"Expected a USING, USING_NAMESPACE, or NAMESPACE_ALIAS statement here!");

	type util::hash_table* attrs = NULL as type util::hash_table*;
	if (attr != NULL as type ast::attribute*) {
		switch (k) {
		case ast::stmt_kind::USING:
			attrs = tck_attribute(ctx, ast::attribute_kind::USING, attr);
			break;
		case ast::stmt_kind::USING_NAMESPACE:
			attrs = tck_attribute(ctx, ast::attribute_kind::USING_NAMESPACE, attr);
			break;
		case ast::stmt_kind::NAMESPACE_ALIAS:
			attrs = tck_attribute(ctx, ast::attribute_kind::NAMESPACE_ALIAS, attr);
			break;
		default:
			util::ice("tck_alias",
				"Unrecognized stmt_kind while tck'ing an alias!");
		}

		if (attrs == NULL as type util::hash_table*)
			return false;
	}

	bool is_namespace = k == ast::stmt_kind::USING_NAMESPACE
		|| k == ast::stmt_kind::NAMESPACE_ALIAS;

	type symtab_value* sv = symtab_lookup_qualified(ctx, from_name->name, from_name->metadata, true);
	if (sv == NULL as type symtab_value*)
		return false;

	type symtab* sv_ctx = symtab_lookup_qualified_ctx(ctx, from_name->name, from_name->metadata, true);
	if (sv_ctx == NULL as type symtab*)
		return false;

	to_fully_qualified(sv_ctx, from_name);
	from_name->symtab_value = sv; 

	if (k != ast::stmt_kind::USING_NAMESPACE) { 
		type symtab_value* check_curr = symtab_lookup_unqualified(ctx, to_name, true, true);
		if (check_curr != NULL as type symtab_value*) {
			type util::string* err_str = util::string_init("Duplicate symbol '");
			util::string_catc(err_str, to_name->text);
			util::string_catc(err_str, "' found in the current scope.");

			unsigned int token_index = k == ast::stmt_kind::NAMESPACE_ALIAS
				? 1 : qident2token_index(from_name->name,
					util::vector_size(from_name->name) - 1);
			type ast::metadata* mtmp = k == ast::stmt_kind::NAMESPACE_ALIAS
				? m : from_name->metadata;
			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				mtmp, token_index, token_index, true);
			util::string_delete(err_str);

			util::report_token(util::error_kind::NOTE,
				"Originally declared here.", check_curr->name);
			return false;
		}
	}

	unsigned int offset = k == ast::stmt_kind::USING ? 1
		: (k == ast::stmt_kind::USING_NAMESPACE ? 2 : 3);

	if (is_namespace && sv->kind != symtab_value_kind::NAMESPACE) {
		util::report_ast_metadata(util::error_kind::ERROR,
			"Expected a namespace symbol for a 'using namespace ...' or namespace alias directive.",
			m, offset, (m->end - m->start) - 2, true);
		return false;
	}
	else if (!is_namespace && sv->kind == symtab_value_kind::NAMESPACE) {
		util::report_ast_metadata(util::error_kind::ERROR,
			"Expected a non-namespace symbol for a 'using ...' directive (consider a 'using namespace ...' directive instead).",
			m, offset, (m->end - m->start) - 2, true);
		return false;
	}

	if (sv->kind != symtab_value_kind::STRUCT
		&& sv->kind != symtab_value_kind::UNION
		&& sv->kind != symtab_value_kind::VARIANT
		&& sv->kind != symtab_value_kind::ENUM
		&& sv->kind != symtab_value_kind::FUN
		&& sv->kind != symtab_value_kind::TYPE_ALIAS
		&& sv->ast_node_kind != symtab_value_ast_node_kind::VARIANT_CONSTRUCTOR
		&& sv->kind != symtab_value_kind::NAMESPACE) {
		util::report_ast_metadata(util::error_kind::ERROR,
			"A 'using [namespace] ...' directive must refer to one of a: struct/union/enum, namespace, function, or type alias.",
			m, offset, (m->end - m->start) - 2, true);
		return false;
	}

	if (k != ast::stmt_kind::USING_NAMESPACE) {
		type symtab_value* cr = symtab_value_cross_ref_init(sv, to_name);
		cr->attrs = attrs;
		char* cname = n_strdup(to_name->text);

		util::ht_set(ctx->symbols, cname as byte*, cr as byte*);
	}
	else {
		if (sv->fwd) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot have a 'using namespace ...' directive on a forward-declared namespace.",
				m, offset, (m->end - m->start) - 2, true);
			return false;
		}

		for (type util::hash_table_iterator* ht_it = util::ht_iter_init(sv->members->symbols);
			!util::ht_iter_done(ht_it);
			util::ht_iter_advance(ht_it)) {
			type util::hash_table_entry* entry = util::ht_iter_curr(ht_it);

			type symtab_value* sv = entry->value as type symtab_value*;

			if (sv->kind == symtab_value_kind::STRUCT
				|| sv->kind == symtab_value_kind::UNION
				|| sv->kind == symtab_value_kind::ENUM
				|| sv->kind == symtab_value_kind::VARIANT
				|| sv->kind == symtab_value_kind::NAMESPACE
				|| sv->kind == symtab_value_kind::FUN
				|| sv->ast_node_kind == symtab_value_ast_node_kind::VARIANT_CONSTRUCTOR
				|| sv->kind == symtab_value_kind::TYPE_ALIAS) {
				type symtab_value* check = symtab_lookup_unqualified(ctx, sv->name, true, true);
				if (check != NULL as type symtab_value*) {
					type util::string* err_str = util::string_init("While processing a 'using namespace ...' directive, symbol '");
					util::string_catc(err_str, sv->name->text);
					util::string_catc(err_str, "' conflicts with an existing symbol.");

					util::report_ast_metadata(util::error_kind::ERROR,
						util::string_data(err_str),
						m, offset, (m->end - m->start) - 2, true);
					util::string_delete(err_str);

					util::report_token(util::error_kind::NOTE,
						"Conflicting symbol from 'using namespace ...' directive here.",
						sv->name);

					util::report_token(util::error_kind::NOTE,
						"Originally declared here.", check->name);
					return false;
				}

				char* cname = n_strdup(sv->name->text);
				type symtab_value* cr = symtab_value_cross_ref_init(sv, sv->name);
				cr->attrs = attrs;

				util::ht_set(ctx->symbols, cname as byte*, cr as byte*);
			}
		}
	}

	return true;
}

func[static] bool tck_asm(type symtab* ctx, type util::vector* asm) {
	for (unsigned int i = 0; i < util::vector_size(asm); i++) {
		type ast::asm_component* ac = util::vector_at(asm, i)
			as type ast::asm_component** @;

		if (ac->qualified_identifier != NULL as type ast::qualified_identifier*) {
			type symtab_value* sv = symtab_lookup_qualified(ctx, ac->qualified_identifier->name,
				ac->qualified_identifier->metadata, true);
			if (sv == NULL as type symtab_value*)
				return false;

			/* type symtab* sv_ctx = symtab_lookup_qualified_ctx(ctx, ac->qualified_identifier->name,
				ac->qualified_identifier->metadata, true);
			if (sv_ctx == NULL as type symtab*)
				return false; */

			if ((sv->kind != symtab_value_kind::SYMBOL
				&& sv->kind != symtab_value_kind::FUN)
				|| sv->ast_node_kind == symtab_value_ast_node_kind::ENUM_SYMBOL
				|| (sv->ast_node_kind == symtab_value_ast_node_kind::DECL
					&& sv->is_constant)) {
				util::report_ast_metadata(util::error_kind::ERROR,
					"Expected an lvalue symbol to take the address of in an 'asm' statement.",
					ac->metadata, 1, (ac->metadata->end - ac->metadata->start) - 1, true);
				return false;
			}

			/* to_fully_qualified(sv_ctx, ac->qualified_identifier);
			ac->qualified_identifier->symtab_value = sv; */
		}
	}

	return true;
}

func bool tck_type_alias(type symtab* ctx, type ast::type_alias* ta) {
	for (unsigned int i = 0; i < util::vector_size(ta->aliases); i++) {
		type ast::type_alias_value* tav = util::vector_at(ta->aliases, i)
			as type ast::type_alias_value** @;

		type symtab_value* check = symtab_lookup_unqualified(ctx, tav->ident, true, true);
		if (check != NULL as type symtab_value*) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Duplicate symbol declared here for a type alias.",
				tav->metadata, 0, 0, true);
			util::report_token(util::error_kind::NOTE,
				"Originally declared here.", check->name);
			return false;
		}

		type ast::typ* resolved = tck_typ(ctx, tav->underlying);
		if (resolved == NULL as type ast::typ*)
			return false;

		type symtab_value* sv = symtab_value_type_alias_init(ctx, false,
			false, tav->ident, resolved, tav);
		char* cname = n_strdup(tav->ident->text);
		util::ht_set(ctx->symbols, cname as byte*, sv as byte*);

		tav->symtab_value = sv;
	}

	return true;
}

func bool tck_match(type symtab* ctx, type ast::match_data* md) {
	type symtab* subctx = symtab_init(symtab_kind::LOCAL, NULL as type lex::token*,
		ctx->global, ctx);

	if (!tck_exp(subctx, md->parent))
		return false;

	for (unsigned int i = 0; i < util::vector_size(md->match_branches); i++) {
		type ast::match_branch* mb = util::vector_at(md->match_branches, i)
			as type ast::match_branch** @;

		type symtab* branchctx = symtab_init(symtab_kind::LOCAL, NULL as type lex::token*,
			subctx->global, subctx);

		for (unsigned int j = 0; j < util::vector_size(mb->pats); j++) {
			type ast::pat* curr_pat = util::vector_at(mb->pats, j)
				as type ast::pat** @;

			if (!tck_pat(branchctx, pat_ctx_kind::MATCH, curr_pat, md->parent->typ,
				NULL as type ast::attribute*)) {
				return false;
			}
		}

		if (mb->guard != NULL as type ast::exp*) {
			if (!tck_exp(branchctx, mb->guard))
				return false;

			if (!is_primitive(mb->guard->typ, ast::primitive_kind::BOOL)) {
				type util::string* err_str = util::string_init("Expected a boolean condition for a match branch guard; found '");
				typ_human_readable(mb->guard->typ, err_str);
				util::string_catc(err_str, "' instead.");

				type ast::metadata temp;
				temp.token_stream = mb->metadata->token_stream;
				temp.start = mb->metadata->start;
				temp.end = mb->guard->metadata->end + 1;

				unsigned int start_index = mb->guard->metadata->start - mb->metadata->start,
					end_index = (mb->guard->metadata->end - mb->metadata->start) - 1;
				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					temp$, start_index, end_index, true);
				util::string_delete(err_str);

				return false;
			}
		}

		if (!tck_stmt(branchctx, mb->body))
			return false;

		mb->ctx = branchctx;
	}

	md->ctx = subctx;

	return true;
}

func bool tck_stmt(type symtab* ctx, type ast::stmt* s) {
	switch (s->kind) {
	case ast::stmt_kind::EMPTY:
		break;
	case ast::stmt_kind::EXP:
		return tck_exp(ctx, s->which.exp);
	case ast::stmt_kind::DECL:
		return tck_decl(ctx, s->which.decl, false);
	case ast::stmt_kind::COMPOUND: {
		type symtab* c_ctx = symtab_init(symtab_kind::LOCAL, ctx->name,
			ctx->global, ctx);

		for (unsigned int i = 0; i < util::vector_size(s->which.compound->stmts); i++) {
			type ast::stmt* cs = util::vector_at(s->which.compound->stmts, i)
				as type ast::stmt** @;

			if (!tck_stmt(c_ctx, cs))
				return false;
		} 

		s->which.compound->ctx = c_ctx;
		util::vector_append(ctx->unnamed_children, c_ctx$ as byte*);
	}
		break;
	case ast::stmt_kind::AGGREGATE:
		return tck_aggregate(ctx, s->which.aggregate);
	case ast::stmt_kind::SELECTION:
		return tck_selection(ctx, s->which.selection);
	case ast::stmt_kind::LABELED:
		return tck_labeled(ctx, s->which.labeled);
	case ast::stmt_kind::JUMP:
		return tck_jump(ctx, s->which.jump);
	case ast::stmt_kind::ITERATION:
		return tck_iteration(ctx, s->which.iteration);
	case ast::stmt_kind::USING:
	case ast::stmt_kind::USING_NAMESPACE: {
		type ast::using_data* ud = s->which.using_data;
		type ast::qualified_identifier* from_name = ud->qualified_identifier;
		util::maybe_ice(!util::vector_empty(from_name->name),
			"tck_stmt",
			"Expected a valid qualified identifier for a using-directive here!");

		type lex::token* to_name = util::vector_at(from_name->name, util::vector_size(from_name->name) - 1)
			as type lex::token** @;

		return tck_alias(ctx, s->kind, to_name, from_name, ud->attribute, s->metadata);
	}
		break;
	case ast::stmt_kind::NAMESPACE_ALIAS: {
		type ast::namespace_alias* na = s->which.namespace_alias;

		if (!tck_alias(ctx, s->kind, na->to_name, na->from_name, na->attribute, s->metadata))
			return false;
	}
		break;
	case ast::stmt_kind::ASM: {
		type util::vector* asm = s->which.asm;

		if (!tck_asm(ctx, asm))
			return false;
	}
		break;
	case ast::stmt_kind::INCLUDE:
		break;
	case ast::stmt_kind::TYPE_ALIAS: {
		type ast::type_alias* ta = s->which.type_alias;

		if (!tck_type_alias(ctx, ta))
			return false;
	}
		break;
	case ast::stmt_kind::MATCH: {
		type ast::match_data* md = s->which.match_data;

		if (!tck_match(ctx, md))
			return false;
	}
		break;
	// TODO
	default:
		util::ice("tck_stmt",
			"Unrecognized stmt_kind while tck'ing!");
	}

	return true;
}

} } // namespace neutrino::tck
