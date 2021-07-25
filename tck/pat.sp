import "tck/tck.hsp"

import <"std/io">
import <"std/lib">

import "ast/ast.hsp"
import "lex/token.hsp"
import "tck/symtab.hsp"
import "tck/util.hsp"
import "util/error.hsp"
import "util/base.hsp"
import "util/lib.hsp"
import "parse/util.hsp"
import "util/hash_table.hsp"
import "util/generic_funcs.hsp"

using std::io::printf;
using std::lib::NULL;
using neutrino::util::n_strdup;

namespace neutrino { namespace tck {

func[static] type ast::typ* tck_range_bound(type symtab* ctx, type ast::range_bound* rb) {
	switch (rb->kind) {
	case ast::range_bound_kind::LITERAL: {
		type ast::typ* lit_typ = literal2typ(rb->which.literal);
		util::maybe_ice(is_integral(lit_typ),
			"tck_pat",
			"Expected an integral type for a start bound of a range pattern here!");
		return lit_typ;
	}
		break;
	case ast::range_bound_kind::QUALIFIED_IDENTIFIER: {
		type ast::qualified_identifier* qi = rb->which.qualified_identifier;

		type symtab_value* sv = symtab_lookup_qualified(ctx, qi->name, qi->metadata, true);
		if (sv == NULL as type symtab_value*)
			return NULL as type ast::typ*;

		if (!sv->is_constant || !is_integral(sv->typ)) {
			type util::string* err_str = util::string_init("Expected an integral, constant type for a range bound; found '");
			typ_human_readable(sv->typ, err_str);
			util::string_catc(err_str, "' instead.");

			unsigned int token_index = qident2token_index(qi->name,
				util::vector_size(qi->name) - 1);
			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				qi->metadata, token_index, token_index, true);
			return NULL as type ast::typ*;
		}

		type symtab* parent_ctx = symtab_lookup_qualified_ctx(ctx,
			qi->name, qi->metadata, true);
		if (parent_ctx == NULL as type symtab*)
			return NULL as type ast::typ*;

		to_fully_qualified(parent_ctx, qi);
		qi->symtab_value = sv;

		return typ_copy(sv->typ);
	}
		break;
	default:
		util::ice("tck_pat",
			"Unrecognized range_bound_kind while tck'ing the starting bound of a range pattern!");
	}

	util::ice("tck_range_bound",
		"This should be unreachable!");
	return NULL as type ast::typ*;
}

func bool tck_pat(type symtab* ctx, unsigned int pck, type ast::pat* p, type ast::typ* t,
	type ast::attribute* attr) {
	switch (p->kind) {
	case ast::pat_kind::WILDCARD: {
		p->typ = typ_copy(t);
		return true;
	}
		break;
	case ast::pat_kind::NESTED: {
		if (!tck_pat(ctx, pck, p->which.nested, t, attr))
			return false;
		p->typ = typ_copy(p->which.nested->typ);
		return true;
	}
		break;
	case ast::pat_kind::IDENT: {
		type ast::ident_pat* ip = p->which.ident_pat;

		if (pck == pat_ctx_kind::EXP) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot introduce a new binding or match an nullary variant constructor with an assignment expression pattern; consider using braces ('{', '}') to refer to an existing expression.",
				ip->metadata, 0, 0, false);
			return false;
		}

		type ast::typ* to_typ = NULL as type ast::typ*;
		if (ip->to_typ != NULL as type ast::typ*) {
			to_typ = tck_typ(ctx, ip->to_typ);
			if (to_typ == NULL as type ast::typ*)
				return false;
		}

		if ((ip->is_mut || to_typ != NULL as type ast::typ*)
			&& pck == pat_ctx_kind::CONST) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot have a 'mut' identifier or a cast for a constant.",
				ip->metadata, 0, 0, true);
			return false;
		}


		type ast::typ* variant_typ = NULL as type ast::typ*;
		type lex::token* vc_name = NULL as type lex::token*;
		type symtab_value* vc_sv = NULL as type symtab_value*;

		type symtab_value* check = symtab_lookup_unqualified(ctx, ip->ident, true, true);
		if (check != NULL as type symtab_value*) {
			bool is_vc = check->kind == symtab_value_kind::SYMBOL
				&& check->ast_node_kind == symtab_value_ast_node_kind::VARIANT_CONSTRUCTOR;
			if (ip->is_mut || !is_vc) {
				unsigned int token_index = ip->is_mut ? 1 : 0;
				util::report_ast_metadata(util::error_kind::ERROR,
					"Duplicate symbol found here.",
					ip->metadata, token_index, token_index, true);
				util::report_token(util::error_kind::NOTE,
					"Originally declared here.", check->name);
				return false;
			}

			variant_typ = typ_copy(check->typ);
			set_mut(variant_typ->typ_qualifiers, false);
			vc_name = check->name;
			vc_sv = check;
		}
		else {
			type symtab_value* check_all = symtab_lookup_unqualified(ctx, ip->ident, false, true);
			if (check_all != NULL as type symtab_value*
				&& check_all->kind == symtab_value_kind::SYMBOL
				&& check_all->ast_node_kind == symtab_value_ast_node_kind::VARIANT_CONSTRUCTOR) {
				variant_typ = typ_copy(check_all->typ);
				set_mut(variant_typ->typ_qualifiers, false);
				vc_name = check_all->name;
				vc_sv = check_all;
			}
		}

		if (vc_name != NULL as type lex::token*
			&& variant_typ != NULL as type ast::typ*
			&& vc_sv != NULL as type symtab_value*) {
			if (to_typ != NULL as type ast::typ*) {
				type util::string* err_str = util::string_init("This symbol refers to a nullary variant constructor of type '");
				typ_human_readable(variant_typ, err_str);
				util::string_catc(err_str, "; cannot cast a nullary variant constructor inside of a pattern.");

				unsigned int start_token_index = ip->is_mut ? 1 : 0,
					end_token_index = ip->is_mut ? 2 : 1;
				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, start_token_index, end_token_index, true);
				util::string_delete(err_str);
				return false;
			}

			if (pck == pat_ctx_kind::DECL
				|| pck == pat_ctx_kind::EXP
				|| pck == pat_ctx_kind::CONST) {
				type util::string* err_str = util::string_init("This symbol refers to a nullary variant constructor of type '");
				typ_human_readable(variant_typ, err_str);
				util::string_catc(err_str, "'; cannot assign to a nullary variant constructor.");

				unsigned int token_index = ip->is_mut ? 1 : 0;
				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, token_index, token_index, true);
				util::report_token(util::error_kind::NOTE,
					"Variant constructor originally declared here.", vc_name);
				util::string_delete(err_str);
				return false;
			}

			if (!typ_compare(ctx, variant_typ, t, false, false)) {
				type util::string* err_str = util::string_init("Incompatible types between a nullary variant constructor and the corresponding type being matched; found '");
				typ_human_readable(variant_typ, err_str);
				util::string_catc(err_str, "' and '");
				typ_human_readable(t, err_str);
				util::string_catc(err_str, "' respectively.");

				unsigned int token_index = ip->is_mut ? 1 : 0;
				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, token_index, token_index, true);
				util::report_token(util::error_kind::NOTE,
					"Variant constructor originally declared here.", vc_name);
				util::string_delete(err_str);
				return false;
			}

			p->typ = variant_typ;
			ip->symtab_value = vc_sv;
			return true;
		}

		if (ip->is_mut && !is_mut(t->typ_qualifiers)) {
			type ast::typ* t1 = typ_copy(t), t2 = typ_copy(t);
			set_mut(t1->typ_qualifiers, true);

			if (!typ_compare(ctx, t2, t1, false, true)) {
				type util::string* err_str = util::string_init("Cannot assign a 'mut' identifier to a non-'mut' type ('");
				typ_human_readable(t, err_str);
				util::string_catc(err_str, "').");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					ip->metadata, 0, 0, true);
				util::string_delete(err_str);
				ast::typ_delete(t1), ast::typ_delete(t2);
				return false;
			}

			ast::typ_delete(t1), ast::typ_delete(t2);
		}

		type ast::typ* final_typ = NULL as type ast::typ*;
		if (to_typ != NULL as type ast::typ*) {
			if (is_primitive(t, ast::primitive_kind::VOID)
				&& !is_primitive(to_typ, ast::primitive_kind::VOID)) {
				unsigned int start_index = (to_typ->metadata->start - ip->metadata->start),
					end_index = (to_typ->metadata->end - ip->metadata->start) - 1;
				util::report_ast_metadata(util::error_kind::ERROR,
					"Cannot cast from a 'void' type to a non-'void' type.",
					ip->metadata, start_index, end_index,  true);
				return false;
			}

			final_typ = to_typ;
			set_mut(final_typ->typ_qualifiers, ip->is_mut);
		}
		else {
			final_typ = typ_copy(t);
			set_mut(final_typ->typ_qualifiers, ip->is_mut);
		}

		util::maybe_ice(final_typ != NULL as type ast::typ*,
			"tck_pat",
			"Expected final_typ to be non-NULL here while tck'ing an identifier pattern!");

		printf("%s: ", ip->ident->text), ast::typ_print(final_typ, 0), printf("\n");

		type symtab_value* new_decl = symtab_value_decl_init(ctx, false,
			false, ip->ident, final_typ, ip->ident);
		new_decl->is_constant = pck == pat_ctx_kind::CONST;
		char* idcname = n_strdup(ip->ident->text);
		if (attr != NULL as type ast::attribute*
			&& attr->attrs != NULL as type util::hash_table*) {
			new_decl->attrs = attr->attrs;
		}
		util::ht_set(ctx->symbols, idcname as byte*, new_decl as byte*);

		p->typ = typ_copy(final_typ);
		ip->symtab_value = new_decl;
		return true;
	}
		break;
	case ast::pat_kind::TUPLE: {
		type ast::tuple_pat* tp = p->which.tuple_pat;

		if (t->kind != ast::typ_kind::TUP) {
			type util::string* err_str = util::string_init("Cannot assign a non-tuple type ('");
			typ_human_readable(t, err_str);
			util::string_catc(err_str, "') to a tuple pattern.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, 0, 0, false);
			util::string_delete(err_str);
			return false;
		}

		type util::vector* tup_typs = t->which.tup_typ;

		switch (tp->ignore_kind) {
		case ast::tuple_ignore_kind::NONE: {
			if (util::vector_size(tup_typs) != util::vector_size(tp->first)) {
				type util::string* err_str = util::string_init("Expected ");
				util::utoustr(util::vector_size(tup_typs), err_str);
				util::string_catc(err_str, " element(s) to assign to this tuple pattern; found ");
				util::utoustr(util::vector_size(tp->first), err_str);
				util::string_catc(err_str, " element(s) instead (from type '");
				typ_human_readable(t, err_str);
				util::string_catc(err_str, "').");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, 0, 0, false);
				util::string_delete(err_str);
				return false;
			}

			tp->first_typs = util::vector_init(sizeof{type ast::typ*}, parse::deref_typ_free);
			for (unsigned int i = 0; i < util::vector_size(tup_typs); i++) {
				type ast::typ* curr_typ = util::vector_at(tup_typs, i)
					as type ast::typ** @;
				type ast::pat* curr_pat = util::vector_at(tp->first, i)
					as type ast::pat** @;

				if (!tck_pat(ctx, pck, curr_pat, curr_typ, attr))
					return false;

				type ast::typ* tcopy = typ_copy(curr_pat->typ);
				util::vector_append(tp->first_typs, tcopy$ as byte*);
			}
		}
			break;
		case ast::tuple_ignore_kind::BEGINNING: {
			if (util::vector_size(tp->first) >= util::vector_size(tup_typs)) {
				type util::string* err_str = util::string_init("Expected at least ");
				util::utoustr(util::vector_size(tp->first) + 1, err_str);
				util::string_catc(err_str, " element(s) when eliding beginning tuple elements; found ");
				util::utoustr(util::vector_size(tup_typs), err_str);
				util::string_catc(err_str, " element(s) instead (from type '");
				typ_human_readable(t, err_str);
				util::string_catc(err_str, "').");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, 0, 0, false);
				util::string_delete(err_str);
				return false;
			}

			tp->first_typs = util::vector_init(sizeof{type ast::typ*}, parse::deref_typ_free);
			for (unsigned int i = util::vector_size(tup_typs) - util::vector_size(tp->first), j = 0;
				i < util::vector_size(tup_typs); i++, j++) {
				type ast::typ* curr_typ = util::vector_at(tup_typs, i) as type ast::typ** @;
				type ast::pat* curr_pat = util::vector_at(tp->first, j) as type ast::pat** @;

				if (!tck_pat(ctx, pck, curr_pat, curr_typ, attr))
					return false;

				type ast::typ* tcopy = typ_copy(curr_pat->typ);
				util::vector_append(tp->first_typs, tcopy$ as byte*);
			}
		}
			break;
		case ast::tuple_ignore_kind::END: {
			if (util::vector_size(tp->first) >= util::vector_size(tup_typs)) {
				type util::string* err_str = util::string_init("Expected at least ");
				util::utoustr(util::vector_size(tp->first) + 1, err_str);
				util::string_catc(err_str, " element(s) when eliding ending tuple elements; found ");
				util::utoustr(util::vector_size(tup_typs), err_str);
				util::string_catc(err_str, " element(s) instead (from type '");
				typ_human_readable(t, err_str);
				util::string_catc(err_str, "').");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, 0, 0, false);
				util::string_delete(err_str);
				return false;
			}

			tp->first_typs = util::vector_init(sizeof{type ast::typ*}, parse::deref_typ_free);
			for (unsigned int i = 0; i < util::vector_size(tp->first); i++) {
				type ast::typ* curr_typ = util::vector_at(tup_typs, i) as type ast::typ** @;
				type ast::pat* curr_pat = util::vector_at(tp->first, i) as type ast::pat** @;

				if (!tck_pat(ctx, pck, curr_pat, curr_typ, attr))
					return false;

				type ast::typ* tcopy = typ_copy(curr_pat->typ);
				util::vector_append(tp->first_typs, tcopy$ as byte*);
			}
		}
			break;
		case ast::tuple_ignore_kind::MIDDLE: {
			if (util::vector_size(tp->first) + util::vector_size(tp->last) >= util::vector_size(tup_typs)) {
				type util::string* err_str = util::string_init("Expected at least ");
				util::utoustr(util::vector_size(tp->first) + util::vector_size(tp->last) + 1, err_str);
				util::string_catc(err_str, " element(s) when eliding middle tuple elements; found ");
				util::utoustr(util::vector_size(tup_typs), err_str);
				util::string_catc(err_str, " element(s) instead (from type '");
				typ_human_readable(t, err_str);
				util::string_catc(err_str, "').");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, 0, 0, false);
				util::string_delete(err_str);
				return false;
			}

			tp->first_typs = util::vector_init(sizeof{type ast::typ*}, parse::deref_typ_free);
			for (unsigned int i = 0; i < util::vector_size(tp->first); i++) {
				type ast::typ* curr_typ = util::vector_at(tup_typs, i) as type ast::typ** @;
				type ast::pat* curr_pat = util::vector_at(tp->first, i) as type ast::pat** @;

				if (!tck_pat(ctx, pck, curr_pat, curr_typ, attr))
					return false;

				type ast::typ* tcopy = typ_copy(curr_pat->typ);
				util::vector_append(tp->first_typs, tcopy$ as byte*);
			}

			tp->last_typs = util::vector_init(sizeof{type ast::typ*}, parse::deref_typ_free);
			for (unsigned int i = util::vector_size(tup_typs) - util::vector_size(tp->last), j = 0;
				i < util::vector_size(tup_typs); i++, j++) {
				type ast::typ* curr_typ = util::vector_at(tup_typs, i) as type ast::typ** @;
				type ast::pat* curr_pat = util::vector_at(tp->last, j) as type ast::pat** @;

				if (!tck_pat(ctx, pck, curr_pat, curr_typ, attr))
					return false;

				type ast::typ* tcopy = typ_copy(curr_pat->typ);
				util::vector_append(tp->last_typs, tcopy$ as byte*);
			}
		}
			break;
		default:
			util::ice("tck_pat",
				"Unrecognized tuple_ignore_kind while tck'ing!");
		}

		p->typ = typ_copy(t);
		return true;
	}
		break;
	case ast::pat_kind::LITERAL: {
		if (pck == pat_ctx_kind::DECL
			|| pck == pat_ctx_kind::CONST
			|| pck == pat_ctx_kind::EXP) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot assign to a literal.",
				p->metadata, 0, 0, false);
			return false;
		}

		type ast::typ* final_typ = literal2typ(p->which.literal);

		if (!typ_compare(ctx, final_typ, t, false, false)) {
			type util::string* err_str = util::string_init("Incompatible types between a literal and the type being matched against; found '");
			typ_human_readable(final_typ, err_str);
			util::string_catc(err_str, "' and '");
			typ_human_readable(t, err_str);
			util::string_catc(err_str, "' respectively.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, 0, 0, false);
			util::string_delete(err_str);
			return false;
		}

		p->typ = final_typ;
		return true;
	}
		break;
	case ast::pat_kind::RANGE: {
		if (pck == pat_ctx_kind::DECL
			|| pck == pat_ctx_kind::CONST
			|| pck == pat_ctx_kind::EXP) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot assign to a range.",
				p->metadata, 0, 0, false);
			return false;
		}

		type ast::range_pat* rp = p->which.range_pat;

		type ast::range_bound* srb = rp->start, erb = rp->end;
		type ast::typ* styp = NULL as type ast::typ*,
			etyp = NULL as type ast::typ*;

		if (srb != NULL as type ast::range_bound*) {
			styp = tck_range_bound(ctx, srb);
			if (styp == NULL as type ast::typ*)
				return false;
		}

		if (erb != NULL as type ast::range_bound*) {
			etyp = tck_range_bound(ctx, erb);
			if (etyp == NULL as type ast::typ*)
				return false;
		}

		type ast::typ* final_typ = NULL as type ast::typ*;
		if (styp == NULL as type ast::typ* && etyp == NULL as type ast::typ*) {
			final_typ = ast::typ_primitive_init(mut_non_pointer_typ_qualifiers_init(),
				ast::primitive_kind::SIGNED_INT, NULL as type ast::metadata*);
		}
		else if (styp == NULL as type ast::typ* && etyp != NULL as type ast::typ*)
			final_typ = typ_copy(etyp);
		else if (styp != NULL as type ast::typ* && etyp == NULL as type ast::typ*)
			final_typ = typ_copy(styp);
		else {
			type util::hash_table* typ_prom_ht = prim_typ_promotion_table();

			unsigned int prec1, prec2;
			util::maybe_ice(util::ht_get(typ_prom_ht, styp->which.primitive as byte*, prec1$ as byte**)
				&& util::ht_get(typ_prom_ht, etyp->which.primitive as byte*, prec2$ as byte**),
				"tck_pat", "Expected to find primitive types in the type promotion hierarchy here!");

			final_typ = ast::typ_primitive_init(mut_non_pointer_typ_qualifiers_init(),
				prec1 < prec2 ? styp->which.primitive : etyp->which.primitive,
				NULL as type ast::metadata*);
			util::ht_delete(typ_prom_ht);
		}

		util::maybe_ice(final_typ != NULL as type ast::typ*
			&& is_integral(final_typ),
			"tck_pat",
			"Expected 'final_typ' to be non-NULL here!");

		if (!typ_compare(ctx, final_typ, t, false, false)) {
			type util::string* err_str = util::string_init("Incompatible types while matching a range (type '");
			typ_human_readable(final_typ, err_str);
			util::string_catc(err_str, "') with type '");
			typ_human_readable(t, err_str);
			util::string_catc(err_str, "'.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, 0, 0, false);
			return false;
		}

		p->typ = final_typ;
		return true;
	}
		break;
	case ast::pat_kind::EXP: {
		type ast::exp* e = p->which.exp;

		if (!tck_exp(ctx, e))
			return false;

		switch (pck) {
		case pat_ctx_kind::DECL:
		case pat_ctx_kind::CONST: {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot assign to an expression pattern inside of a 'let' or 'const'.",
				p->metadata, 0, 0, false);
			return false;
		}
			break;
		case pat_ctx_kind::EXP: {
			if (e->value_kind != ast::value_kind::LVALUE
				|| e->is_constant
				|| !is_mut(e->typ->typ_qualifiers)) {
				util::report_ast_metadata(util::error_kind::ERROR,
					"Expected a non-'mut', non-constant, lvalue to assign to here.",
					p->metadata, 1, (p->metadata->end - p->metadata->start) - 2,
					true);
				return false;
			}

			if (!typ_compare(ctx, t, e->typ, false, true)) {
				type util::string* err_str = util::string_init("Incompatible types between an expression pattern (type '");
				typ_human_readable(e->typ, err_str);
				util::string_catc(err_str, "') and its initializer (type '");
				typ_human_readable(t, err_str);
				util::string_catc(err_str, "').");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, 1, (p->metadata->end - p->metadata->start) - 2, true);
				util::string_delete(err_str);
				return false;
			}
		}
			break;
		case pat_ctx_kind::MATCH: {
			if (!typ_compare(ctx, t, e->typ, false, false)) {
				type util::string* err_str = util::string_init("Incompatible types between an expression pattern (type '");
				typ_human_readable(e->typ, err_str);
				util::string_catc(err_str, "') and the type being matched against (type '");
				typ_human_readable(t, err_str);
				util::string_catc(err_str, "').");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, 1, (p->metadata->end - p->metadata->start) - 2, true);
				return false;
			}
		}
			break;
		default:
			util::ice("tck_pat",
				"Unrecognized pat_ctx_kind while tck'ing an expression pattern.");
		}

		p->typ = typ_copy(e->typ);
		return true;
	}
		break;
	case ast::pat_kind::CONSTRUCTOR: {
		type ast::constructor_pat* cp = p->which.constructor_pat;
		type ast::qualified_identifier* qi = cp->constructor;

		type symtab_value* check = symtab_lookup_qualified(ctx, qi->name, qi->metadata, true);
		if (check == NULL as type symtab_value*)
			return false;

		type symtab* check_ctx = symtab_lookup_qualified_ctx(ctx, qi->name, qi->metadata, true);
		if (check_ctx == NULL as type symtab*)
			return false;

		to_fully_qualified(check_ctx, qi);
		qi->symtab_value = check;

		bool is_vcn = check->kind == symtab_value_kind::SYMBOL
			&& check->ast_node_kind == symtab_value_ast_node_kind::VARIANT_CONSTRUCTOR,
			is_vc = check->kind == symtab_value_kind::FUN
				&& check->ast_node_kind == symtab_value_ast_node_kind::VARIANT_CONSTRUCTOR;

		unsigned int start_qi_index = qi->metadata->start - p->metadata->start,
			end_qi_index = (qi->metadata->end - p->metadata->start) - 1;

		if (!is_vcn && !is_vc) {
			type util::string* err_str = util::string_init("Expected a (nullary) variant constructor here; found type '");
			typ_human_readable(check->typ, err_str);
			util::string_catc(err_str, "' instead.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, start_qi_index, end_qi_index, true);
			util::report_token(util::error_kind::NOTE,
				"Symbol declared here.", check->name);
			util::string_delete(err_str);
			return false;
		}

		type ast::typ* final_typ = NULL as type ast::typ*;
		type util::vector* nested_typs = NULL as type util::vector*;

		if (is_vcn) {
			if (pck == pat_ctx_kind::DECL
				|| pck == pat_ctx_kind::EXP
				|| pck == pat_ctx_kind::CONST) {
				type util::string* err_str = util::string_init("This symbol refers to a nullary variant constructor of type '");
				typ_human_readable(check->typ, err_str);
				util::string_catc(err_str, "'; cannot assign to a nullary variant constructor.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, start_qi_index, end_qi_index, true);
				util::report_token(util::error_kind::NOTE,
					"Variant constructor originally declared here.", check->name);
				util::string_delete(err_str);
				return false;
			}

			if (cp->nested != NULL as type util::vector*) {
				type util::string* err_str = util::string_init("Expected no (absent) arguments for a nullary variant constructor; found ");
				util::utoustr(util::vector_size(cp->nested), err_str);
				util::string_catc(err_str, " argument(s) instead.");

				type util::string* err_str2 = util::string_init("Symbol declared here with type '");
				typ_human_readable(check->typ, err_str2);
				util::string_catc(err_str2, "'.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, start_qi_index, end_qi_index, true);
				util::report_token(util::error_kind::NOTE,
					util::string_data(err_str2), check->name);
				util::string_delete(err_str);
				util::string_delete(err_str2);
				return false;
			}

			final_typ = typ_copy(check->typ);
			set_mut(final_typ->typ_qualifiers, false);
		}
		else {
			util::maybe_ice(is_vc,
				"tck_pat",
				"Expected a variant constructor here while tck'ing a constructor pattern!");

			util::maybe_ice(check->typ->kind == ast::typ_kind::FN_TYP,
				"tck_pat",
				"Expected a function type for a variant constructor!");

			type ast::fn_typ* ft = check->typ->which.fn_typ;

			if (cp->nested == NULL as type util::vector*
				|| util::vector_size(ft->parameter_typs) != util::vector_size(cp->nested)) {
				type util::string* err_str = util::string_init("Expected ");
				util::utoustr(util::vector_size(ft->parameter_typs), err_str);
				util::string_catc(err_str, " argument(s) for this variant constructor; found ");
				if (cp->nested == NULL as type util::vector*)
					util::string_catc(err_str, "no");
				else
					util::utoustr(util::vector_size(cp->nested), err_str);
				util::string_catc(err_str, " argument(s) instead.");

				type util::string* err_str2 = util::string_init("Variant constructor originally declared here with type '");
				typ_human_readable(check->typ, err_str2);
				util::string_catc(err_str2, "'.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, start_qi_index, end_qi_index, true);
				util::report_token(util::error_kind::NOTE,
					util::string_data(err_str2), check->name);
				util::string_delete(err_str);
				util::string_delete(err_str2);
				return false;
			}

			final_typ = typ_copy(ft->return_typ);
			set_mut(final_typ->typ_qualifiers, false);
			nested_typs = ft->parameter_typs;
		}

		util::maybe_ice(final_typ != NULL as type ast::typ*,
			"tck_pat",
			"Expected final_typ to be a non-NULL here while tck'ing a constructor pattern!");


		if (!typ_compare(ctx, final_typ, t, false, true)) {
			type util::string* err_str = util::string_init("Incompatible types between a variant constructor and the type being matched; found '");
			typ_human_readable(final_typ, err_str);
			util::string_catc(err_str, "' and '");
			typ_human_readable(t, err_str);
			util::string_catc(err_str, "' respectively.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, start_qi_index, end_qi_index, true);
			return false;
		}

		if (cp->nested != NULL as type util::vector*) {
			util::maybe_ice(nested_typs != NULL as type util::vector*
				&& util::vector_size(nested_typs) == util::vector_size(cp->nested),
				"tck_pat",
				"Expected an equivalent and valid number of types and patterns to check against while tck'ing a constructor pattern!");

			for (unsigned int i = 0; i < util::vector_size(cp->nested); i++) {
				type ast::pat* curr_pat = util::vector_at(cp->nested, i)
					as type ast::pat** @;
				type ast::typ* curr_typ = util::vector_at(nested_typs, i)
					as type ast::typ** @;

				if (!tck_pat(ctx, pck, curr_pat, curr_typ, attr))
					return false;
			}
		}

		p->typ = final_typ;
		return true;
	}
		break;
	case ast::pat_kind::STRUCT: {
		type ast::struct_pat* sp = p->which.struct_pat;
		type ast::qualified_identifier* qi = sp->qualified_identifier;

		type symtab_value* sv = symtab_lookup_qualified(ctx, qi->name, qi->metadata, false);
		if (sv == NULL as type symtab_value*)
			return false;

		type symtab* sv_ctx = symtab_lookup_qualified_ctx(ctx, qi->name, qi->metadata, false);
		if (sv_ctx == NULL as type symtab*)
			return false;

		unsigned int start_qi_index = (qi->metadata->start - p->metadata->start),
			end_qi_index = (qi->metadata->end - p->metadata->start) - 1;

		if (sv->kind != symtab_value_kind::STRUCT
			&& sv->kind != symtab_value_kind::UNION
			&& sv->kind != symtab_value_kind::TYPE_ALIAS) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Expected a struct/union for a struct pattern.",
				p->metadata, start_qi_index, end_qi_index, true);
			return false;
		}

		if (sv->kind == symtab_value_kind::TYPE_ALIAS) {
			type ast::typ* check = sv->typ;

			if (check->kind != ast::typ_kind::AGGREGATE_NAME
				|| !check->typ_qualifiers->none_specified
				|| is_pointer(check)) {
				type util::string* err_str = util::string_init("Expected a struct/union type alias with no type qualifiers to be used in a struct pattern; found '");
				typ_human_readable(check, err_str);
				util::string_catc(err_str, "' instead.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					qi->metadata, 0, 0, false);
				util::string_delete(err_str);
				return false;
			}

			type ast::qualified_identifier* curr_qi = check->which.aggregate_name;
			type symtab_value* check_sv = symtab_lookup_qualified(curr_qi->ref_ctx, curr_qi->name, NULL as type ast::metadata*, false);

			util::maybe_ice(check_sv != NULL as type symtab_value*,
				"tck_pat",
				"Expected a valid type name from a type alias for a struct pattern!");

			if (check_sv->kind != symtab_value_kind::STRUCT
				&& check_sv->kind != symtab_value_kind::UNION) {
				type util::string* err_str = util::string_init("Expected a struct/union type alias with no type qualifiers to be used in a struct pattern; found '");
				typ_human_readable(check, err_str);
				util::string_catc(err_str, "' instead.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					qi->metadata, 0, 0, true);
				util::string_delete(err_str);
				return false;
			}

			sv = check_sv;
		}

		to_fully_qualified(sv_ctx, qi);
		qi->symtab_value = sv;

		if (sv->kind == symtab_value_kind::UNION
			&& util::vector_size(sp->member_pats) > 1) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Can only match against (at most) a single member of a union.",
				p->metadata, start_qi_index, end_qi_index, true);

			util::report_token(util::error_kind::NOTE,
				"Union in question declared here.", sv->name);
			return false;
		}

		type util::hash_table* mem_ht = util::ht_init(
			util::str_hash, util::str_eq,
			util::no_free, util::no_free);
		for (unsigned int i = 0; i < util::vector_size(sp->member_pats); i++) {
			type ast::member_pat* mp = util::vector_at(sp->member_pats, i)
				as type ast::member_pat** @;

			unsigned int mem_index = mp->is_mut ? 2 : 1;

			type symtab_value* mem_sv = symtab_lookup_unqualified(sv->members, mp->member, true, true);
			if (mem_sv == NULL as type symtab_value*
				|| mem_sv->ast_node_kind != symtab_value_ast_node_kind::MEMBER) {
				type util::string* err_str = util::string_init("Could not find member '");
				util::string_catc(err_str, mp->member->text);
				util::string_catc(err_str, "' in '");

				type ast::typ* tmp = typ_copy(sv->typ);
				set_mut(tmp->typ_qualifiers, false);
				typ_human_readable(tmp, err_str);
				ast::typ_delete(tmp);

				util::string_catc(err_str, "'.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					mp->metadata, mem_index, mem_index, true);
				util::report_token(util::error_kind::NOTE,
					"Member requested from this context.", sv->name);
				util::string_delete(err_str);
				return false;
			}

			mp->symtab_value = mem_sv;

			type ast::member_pat* prev_mp = NULL as type ast::member_pat*;
			if (util::ht_get(mem_ht, mp->member->text as byte*, prev_mp$ as byte**)) {
				type util::string* err_str = util::string_init("Multiple patterns for member '");
				util::string_catc(err_str, mp->member->text);
				util::string_catc(err_str, "'.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					mp->metadata, mem_index - 1, mem_index, true);
				unsigned int prev_mem_index = prev_mp->is_mut ? 2 : 1;
				util::report_ast_metadata(util::error_kind::NOTE,
					"Original pattern here.",
					prev_mp->metadata, prev_mem_index - 1, prev_mem_index, true);
				return false;
			}

			if (mp->nested == NULL as type ast::pat*) {
				if (pck == pat_ctx_kind::EXP) {
					util::report_ast_metadata(util::error_kind::ERROR,
						"Cannot introduce a new symbol during an expression assignment.",
						mp->metadata, mem_index, mem_index, true);
					return false;
				}

				type symtab_value* check = symtab_lookup_unqualified(ctx, mp->member, true, true);

				if (check != NULL as type symtab_value*) {
					util::report_ast_metadata(util::error_kind::ERROR,
						"Duplicate symbol found here.",
						mp->metadata, mem_index, mem_index, true);
					util::report_token(util::error_kind::NOTE,
						"Originally declared here.", check->name);
					return false;
				}

				type ast::typ* new_typ = typ_copy(mem_sv->typ);
				set_mut(new_typ->typ_qualifiers, mp->is_mut);

				type symtab_value* new_sv = symtab_value_decl_init(ctx, false,
					false, mp->member, new_typ, mp->member);
				char* cname = n_strdup(mp->member->text);
				util::ht_set(ctx->symbols, cname as byte*, new_sv as byte*);
			}
			else {
				util::maybe_ice(!mp->is_mut,
					"tck_pat",
					"For a non-NULL nested pattern with a member, expected no 'mut' qualifier!");

				if (!tck_pat(ctx, pck, mp->nested, mem_sv->typ, attr))
					return false;
			}

			util::ht_set(mem_ht, mp->member->text as byte*, mp as byte*);
		}
		util::ht_delete(mem_ht);

		type ast::typ* final_typ = typ_copy(sv->typ);
		set_mut(final_typ->typ_qualifiers, false);

		if (!typ_compare(ctx, t, final_typ, false, true)) {
			type util::string* err_str = util::string_init("Incompatible types between a 'struct' pattern and the type being matched; found '");
			typ_human_readable(final_typ, err_str);
			util::string_catc(err_str, "' and '");
			typ_human_readable(t, err_str);
			util::string_catc(err_str, "' respectively.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, start_qi_index, end_qi_index, true);
			util::string_delete(err_str);
			return false;
		}

		p->typ = final_typ;
		return true;
	}
		break;
	default:
		util::ice("tck_pat",
			"Unrecognized pat_kind while tck'ing!");
	}

	return false;
}

} } // namespace neutrino::tck
