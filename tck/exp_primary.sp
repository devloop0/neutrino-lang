import "tck/exp_helpers.hsp"

import <"std/io">
import <"std/lib">

import "tck/tck.hsp"
import "ast/ast.hsp"
import "tck/symtab.hsp"
import "util/error.hsp"
import "lex/token.hsp"
import "tck/util.hsp"
import "util/lib.hsp"
import "util/hash_table.hsp"
import "util/generic_funcs.hsp"
import "util/base.hsp"
import "parse/util.hsp"

using std::io::printf;
using std::lib::NULL;
using neutrino::util::n_strdup;

namespace neutrino { namespace tck {

func bool tck_exp_primary(type symtab* ctx, type ast::primary* p) {
	switch (p->kind) {
	case ast::primary_kind::LITERAL: {
		p->typ = literal2typ(p->which.literal);
		p->value_kind = ast::value_kind::RVALUE;
		p->is_constant = true;
		return true;
	}
		break;
	case ast::primary_kind::TUPLE: {
		type util::vector* tu = p->which.tuple;

		type util::vector* tu_typ = util::vector_init(sizeof{type ast::typ*},
			parse::deref_typ_free);

		for (unsigned int i = 0; i < util::vector_size(tu); i++) {
			type ast::exp* e = util::vector_at(tu, i) as type ast::exp** @;

			if (!tck_exp(ctx, e))
				return false;

			type ast::typ* curr_typ = typ_copy(e->typ);
			util::vector_append(tu_typ, curr_typ$ as byte*);
		}

		p->value_kind = ast::value_kind::RVALUE;
		p->typ = ast::typ_tup_init(mut_non_pointer_typ_qualifiers_init(),
			tu_typ, NULL as type ast::metadata*);
		p->is_constant = true;
		return true;
	}
		break;
	case ast::primary_kind::LAMBDA: {
		type ast::typ* check = tck_function(ctx, p->which.lambda);
		if (check == NULL as type ast::typ*)
			return false;

		p->value_kind = ast::value_kind::RVALUE;
		p->typ = check;
		p->is_constant = false;
		return true;
	}
		break;
	case ast::primary_kind::QUALIFIED_IDENTIFIER: {
		type ast::qualified_identifier* qi = p->which.qualified_identifier;
		type util::vector* v = qi->name;
		type symtab_value* sv = symtab_lookup_qualified(ctx, v, p->metadata, true);
		if (sv == NULL as type symtab_value*)
			return false;

		type symtab* sv_ctx = symtab_lookup_qualified_ctx(ctx, v, p->metadata, true);
		if (sv_ctx == NULL as type symtab*)
			return false;

		if (sv->kind == symtab_value_kind::NAMESPACE
			|| sv->kind == symtab_value_kind::STRUCT
			|| sv->kind == symtab_value_kind::UNION
			|| sv->kind == symtab_value_kind::VARIANT
			|| sv->kind == symtab_value_kind::TYPE_ALIAS
			|| sv->kind == symtab_value_kind::ENUM) {
			unsigned int token_index = qident2token_index(v, util::vector_size(v) - 1);
			util::report_ast_metadata(util::error_kind::ERROR,
				"Expected a non-namespace, non-aggregate, non-alias identifier here.", p->metadata,
				token_index, token_index, true);
			return false;
		}

		to_fully_qualified(sv_ctx, qi);
		qi->symtab_value = sv;

		util::maybe_ice(sv->typ != NULL as type ast::typ*, "tck_exp_primary",
			"Expected a non-NULL typ for an identifier here!");

		p->typ = typ_copy(sv->typ);
		p->is_constant = (sv->kind == symtab_value_kind::SYMBOL
			&& sv->is_constant)
			|| (sv->kind == symtab_value_kind::SYMBOL
				&& sv->ast_node_kind == symtab_value_ast_node_kind::ENUM_SYMBOL);
		p->value_kind = p->is_constant
			|| sv->kind == symtab_value_kind::FUN
			|| (sv->kind == symtab_value_kind::SYMBOL
				&& sv->ast_node_kind == symtab_value_ast_node_kind::LABEL)
			|| (sv->kind == symtab_value_kind::SYMBOL
				&& sv->ast_node_kind == symtab_value_ast_node_kind::VARIANT_CONSTRUCTOR)
			? ast::value_kind::RVALUE : ast::value_kind::LVALUE;
		if (p->is_constant)
			set_mut(p->typ->typ_qualifiers, false);
		return true;
	}
		break;
	case ast::primary_kind::STRUCT_INIT: {
		type ast::struct_init* si = p->which.struct_init;
		type ast::qualified_identifier* qi = si->qualified_identifier;
		type util::vector* sname = qi->name;

		type symtab_value* sv = symtab_lookup_qualified(ctx, sname,
			si->qualified_identifier->metadata, false);
		if (sv == NULL as type symtab_value*)
			return false;

		type symtab* sv_ctx = symtab_lookup_qualified_ctx(ctx, sname,
			si->qualified_identifier->metadata, false);
		if (sv_ctx == NULL as type symtab*)
			return false;

		if (sv->kind != symtab_value_kind::STRUCT
			&& sv->kind != symtab_value_kind::UNION
			&& sv->kind != symtab_value_kind::TYPE_ALIAS) {
			unsigned int token_index = qident2token_index(sname, util::vector_size(sname) - 1);
			util::report_ast_metadata(util::error_kind::ERROR,
				"Expected a 'struct' or 'union' here for struct initialization.",
				si->qualified_identifier->metadata,
				token_index, token_index, true);
			return false;
		}

		if (sv->kind == symtab_value_kind::TYPE_ALIAS) {
			type ast::typ* to_check = sv->typ;

			if (to_check->kind != ast::typ_kind::AGGREGATE_NAME
				|| !to_check->typ_qualifiers->none_specified
				|| is_pointer(to_check)) {
				type util::string* err_str = util::string_init("Expected a type alias to refer to a struct/union with no type qualifiers in order to be used for struct initialization; found '");
				typ_human_readable(to_check, err_str);
				util::string_catc(err_str, "' instead.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					si->qualified_identifier->metadata,
					0, 0, false);
				util::string_delete(err_str);
				return false;
			}

			type ast::qualified_identifier* qi = sv->typ->which.aggregate_name;
			type symtab_value* check = symtab_lookup_qualified(qi->ref_ctx, qi->full_name, NULL as type ast::metadata*, false);
			util::maybe_ice(check != NULL as type symtab_value*,
				"tck_exp_primary",
				"Expected a valid type name from a type alias for a struct initialization!");

			if (check->kind != symtab_value_kind::STRUCT
				&& check->kind != symtab_value_kind::UNION) {
				type util::string* err_str = util::string_init("Expected a type alias to refer to a struct/union with no type qualifiers in order to be used for struct initialization; found '");
				typ_human_readable(to_check, err_str);
				util::string_catc(err_str, "' instead.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					si->qualified_identifier->metadata,
					0, 0, false);
				util::string_delete(err_str);
				return false;
			}

			sv = check;
			sv_ctx = qi->ref_ctx;
		}

		to_fully_qualified(sv_ctx, qi);
		qi->symtab_value = sv;

		if (sv->kind == symtab_value_kind::UNION
			&& util::vector_size(si->member_inits) > 1) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Can only initialize (at most) a single member of a union.",
				si->metadata,
				(si->qualified_identifier->metadata->start - si->metadata->start),
				(si->qualified_identifier->metadata->end - si->metadata->start) - 1, true);
			util::report_token(util::error_kind::NOTE,
				"Union in question requested here.", sv->name);
			return false;
		}

		type util::hash_table* mem_ht = util::ht_init(
			util::str_hash, util::str_eq,
			util::no_free, util::no_free);
		for (unsigned int i = 0; i < util::vector_size(si->member_inits); i++) {
			type ast::member_init* mi = util::vector_at(si->member_inits, i)
				as type ast::member_init** @;

			type symtab_value* mem_lookup = symtab_lookup_unqualified(sv->members, mi->ident, true, true);
			if (mem_lookup == NULL as type symtab_value*
				|| mem_lookup->ast_node_kind != symtab_value_ast_node_kind::MEMBER) {
				type util::string* err_str = util::string_init("Could not find member '");
				util::string_catc(err_str, mi->ident->text);
				util::string_catc(err_str, "' in '");

				type ast::typ* tmp = typ_copy(sv->typ);
				set_mut(tmp->typ_qualifiers, false);
				typ_human_readable(tmp, err_str);
				ast::typ_delete(tmp);

				util::string_catc(err_str, "'.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					mi->metadata, 1, 1, true);
				util::string_delete(err_str);

				util::report_token(util::error_kind::NOTE,
					"Struct/union in question requested in this context.", sv->name);
				return false;
			}

			mi->symtab_value = mem_lookup;

			type ast::member_init* prev_mi;
			if (util::ht_get(mem_ht, mi->ident->text as byte*, prev_mi$ as byte**)) {
				type util::string* err_str = util::string_init("Member '");
				util::string_catc(err_str, mi->ident->text);
				util::string_catc(err_str, "' reinitalized here.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					mi->metadata, 0, 1, true);
				util::report_ast_metadata(util::error_kind::NOTE,
					"Originally initialized here.",
					prev_mi->metadata, 0, 1, true);
				util::string_delete(err_str);
				return false;
			}

			if (!tck_exp(ctx, mi->init))
				return false;

			if (!typ_compare(ctx, mi->init->typ, mem_lookup->typ, false, true)) {
				type util::string* err_str = util::string_init("Incompatible types '");
				typ_human_readable(mem_lookup->typ, err_str);
				util::string_catc(err_str, "' and '");
				typ_human_readable(mi->init->typ, err_str);
				util::string_catc(err_str, "' for member initialization.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					mi->metadata, 2, 2, true);
				util::string_delete(err_str);

				util::report_token(util::error_kind::NOTE,
					"Member in question requested here.", mem_lookup->name);
				return false;
			}

			util::ht_set(mem_ht, mi->ident->text as byte*, mi as byte*);
		}
		util::ht_delete(mem_ht);

		p->typ = typ_copy(sv->typ);
		p->value_kind = ast::value_kind::RVALUE;
		p->is_constant = true;
		return true;
	}
		break;
	case ast::primary_kind::PARENTHESIZED: {
		type ast::exp* par = p->which.parenthesized;
		if (!tck_exp(ctx, par))
			return false;

		p->typ = typ_copy(par->typ);
		p->value_kind = par->value_kind;
		p->is_constant = par->is_constant;
		return true;
	}
		break;
	case ast::primary_kind::ARRAY: {
		type util::vector* arr = p->which.array;

		if (util::vector_empty(arr)) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Expected at least one element in an array.",
				p->metadata, 0, 0, false);
			return false;
		}

		type ast::exp* initial = util::vector_at(p->which.array, 0)
			as type ast::exp** @;

		if (!tck_exp(ctx, initial))
			return false;

		type ast::typ* ret = typ_copy(initial->typ);
		if (is_primitive(ret, ast::primitive_kind::VOID)) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot have a void type for an array element.",
				initial->metadata, 0, 0, false);
			return false;
		}

		for (unsigned int i = 1; i < util::vector_size(arr); i++) {
			type ast::exp* curr = util::vector_at(arr, i)
				as type ast::exp** @;

			if (!tck_exp(ctx, curr))
				return false;

			if (!typ_compare(ctx, initial->typ, curr->typ, true, false)) {
				type util::string* err_str = util::string_init(
					"Expected consistent types for elements in an array; first element had type '");
				typ_human_readable(initial->typ, err_str);
				util::string_catc(err_str, "' but element #");
				util::utoustr(i + 1, err_str);
				util::string_catc(err_str, " has type '");
				typ_human_readable(curr->typ, err_str);
				util::string_catc(err_str, "'.");
				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					curr->metadata, 0, 0, false);
				util::string_delete(err_str);

				util::report_ast_metadata(util::error_kind::NOTE,
					"First element of the array is here.",
					initial->metadata, 0, 0, false);
				return false;
			}
		}

		p->typ = ret;
		{
			bool is_mut = true;
			util::vector_insert(p->typ->typ_qualifiers->qualifiers,
				0, is_mut$ as byte*);
		}
		p->value_kind = ast::value_kind::RVALUE;
		p->is_constant = true;
		return true;
	}
		break;
	case ast::primary_kind::STMT: {
		type ast::stmt* ns = p->which.stmt;

		if (!tck_stmt(ctx, ns))
			return false;

		util::maybe_ice(ns->kind == ast::stmt_kind::COMPOUND,
			"tck_exp",
			"Expected a compound statement for an expression statement!");

		type util::vector* cs = ns->which.compound->stmts;

		if (util::vector_empty(cs)) {
			p->typ = ast::typ_primitive_init(
				mut_non_pointer_typ_qualifiers_init(),
				ast::primitive_kind::VOID,
				NULL as type ast::metadata*);
			p->value_kind = ast::value_kind::RVALUE;
			return true;
		}

		type ast::stmt* ls = util::vector_at(cs, util::vector_size(cs) - 1)
			as type ast::stmt** @;

		if (ls->kind != ast::stmt_kind::EXP) {
			p->typ = ast::typ_primitive_init(
				mut_non_pointer_typ_qualifiers_init(),
				ast::primitive_kind::VOID,
				NULL as type ast::metadata*);
			p->value_kind = ast::value_kind::RVALUE;
			return true;
		}

		p->typ = typ_copy(ls->which.exp->typ);
		p->value_kind = ast::value_kind::RVALUE;
		p->is_constant = false;
		return true;
	}
		break;
	default:
		util::ice("tck_exp_primary",
			"Unrecognized primary_kind while tck'ing!");
	}

	return false;
}

} } // namespace neutrino::tck
