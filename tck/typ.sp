import "tck/tck.hsp"

import <"std/io">
import <"std/lib">

import "ast/ast.hsp"
import "util/error.hsp"
import "tck/symtab.hsp"
import "util/generic_funcs.hsp"
import "tck/util.hsp"
import "parse/util.hsp"

using std::io::printf;
using std::lib::NULL;

namespace neutrino { namespace tck {

func type ast::typ* tck_typ(type symtab* ctx, type ast::typ* t) {
	switch (t->kind) {
	case ast::typ_kind::PRIMITIVE: {
		unsigned int prim_kind = t->which.primitive;

		if (prim_kind == ast::primitive_kind::VOID
			&& is_pointer(t)) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot have a pointer-to-void type.", t->metadata, 0, 0, false);
			return NULL as type ast::typ*;
		}

		return typ_copy(t);
	}
		break;
	case ast::typ_kind::FN_TYP: {
		type ast::fn_typ* ft = t->which.fn_typ;

		type ast::typ* nrt = tck_typ(ctx, ft->return_typ);
		if (nrt == NULL as type ast::typ*)
			return NULL as type ast::typ*;

		type util::vector* npars = util::vector_init(sizeof{type ast::typ*}, parse::deref_typ_free);

		for (unsigned int i = 0; i < util::vector_size(ft->parameter_typs); i++) {
			type ast::typ* cpt = util::vector_at(ft->parameter_typs, i)
				as type ast::typ** @;

			type ast::typ* ncpt = tck_typ(ctx, cpt);
			if (ncpt == NULL as type ast::typ*)
				return NULL as type ast::typ*;

			util::vector_append(npars, ncpt$ as byte*);
		}

		return ast::typ_fn_typ_init(typ_qualifiers_copy(t->typ_qualifiers),
			ast::fn_typ_init(npars, ft->is_variadic, nrt),
			ast::metadata_copy(t->metadata));
	}
		break;
	case ast::typ_kind::TUP: {
		type util::vector* tt = t->which.tup_typ;

		type util::vector* ntt = util::vector_init(sizeof{type ast::typ*},
			parse::deref_typ_free);

		for (unsigned int i = 0; i < util::vector_size(tt); i++) {
			type ast::typ* curr = util::vector_at(tt, i) as type ast::typ** @;

			type ast::typ* ncurr = tck_typ(ctx, curr);
			if (ncurr == NULL as type ast::typ*)
				return NULL as type ast::typ*;
			util::vector_append(ntt, ncurr$ as byte*);
		}

		return ast::typ_tup_init(typ_qualifiers_copy(t->typ_qualifiers),
			ntt, ast::metadata_copy(t->metadata));
	}
		break;
	case ast::typ_kind::AGGREGATE_NAME: {
		type ast::qualified_identifier* an = t->which.aggregate_name;
		type symtab* parent_ctx = symtab_lookup_qualified_ctx(ctx,
			an->name, an->metadata, false);
		if (parent_ctx == NULL as type symtab*)
			return NULL as type ast::typ*;

		type symtab_value* val = symtab_lookup_qualified(ctx, an->name, an->metadata, false);
		if (val == NULL as type symtab_value*)
			return NULL as type ast::typ*;

		if (val->kind != symtab_value_kind::STRUCT
			&& val->kind != symtab_value_kind::UNION
			&& val->kind != symtab_value_kind::VARIANT
			&& val->kind != symtab_value_kind::ENUM
			&& val->kind != symtab_value_kind::TYPE_ALIAS) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Found a non-type symbol for a type name starting here.", t->metadata, 0, 0, false);

			util::report_token(util::error_kind::NOTE,
				"Symbol originally declared here.", val->name);
			return NULL as type ast::typ*;
		}

		if (val->fwd && !is_pointer(t)) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot have a non-pointer type to a struct/union/enum that is forward-declared (or being defined).",
				t->metadata, 0, 0, false);

			util::report_token(util::error_kind::NOTE,
				"Symbol originally declared here.", val->name);
			return NULL as type ast::typ*;
		}

		if (val->kind == symtab_value_kind::TYPE_ALIAS) {
			type ast::typ* ret = typ_copy(val->typ);
			ast::metadata_delete(ret->metadata);
			ret->metadata = ast::metadata_copy(t->metadata);

			util::maybe_ice(!util::vector_empty(t->typ_qualifiers->qualifiers)
				&& !util::vector_empty(ret->typ_qualifiers->qualifiers),
				"tck_typ",
				"Expected non-empty type qualifiers here!");

			type util::vector* nq = util::vector_init(sizeof{bool}, util::no_free);
			for (unsigned int i = 0;
				i < util::vector_size(t->typ_qualifiers->qualifiers) - 1;
				i++) {
				bool curr = util::vector_at(t->typ_qualifiers->qualifiers, i) as bool* @;

				util::vector_append(nq, curr$ as byte*);
			}

			bool* prev_final = util::vector_at(t->typ_qualifiers->qualifiers,
				util::vector_size(t->typ_qualifiers->qualifiers) - 1) as bool*,
				curr_first = util::vector_at(ret->typ_qualifiers->qualifiers, 0) as bool*;
			bool join = prev_final@ || curr_first@;

			util::vector_append(nq, join$ as byte*);

			for (unsigned int i = 1; i < util::vector_size(ret->typ_qualifiers->qualifiers); i++) {
				bool curr = util::vector_at(ret->typ_qualifiers->qualifiers, i) as bool* @;

				util::vector_append(nq, curr$ as byte*);
			}

			ret->typ_qualifiers->none_specified = ret->typ_qualifiers->none_specified
				&& t->typ_qualifiers->none_specified;
			util::vector_delete(ret->typ_qualifiers->qualifiers);
			ret->typ_qualifiers->qualifiers = nq;

			type ast::typ* recheck = tck_typ(ctx, ret);
			if (recheck == NULL as type ast::typ*)
				return NULL as type ast::typ*;
			ast::typ_delete(ret);
			return recheck;
		}

		to_fully_qualified(parent_ctx, an);
		an->symtab_value = val;

		return typ_copy(t);
	}
		break;
	default:
		util::ice("tck_typ", "Unrecognized typ_kind while tck'ing!");
	}

	util::ice("tck_typ",
		"This should be unreachable!");
	return NULL as type ast::typ*;
}

} } // namespace neutrino::tck
