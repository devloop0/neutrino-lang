import "tck/util.hsp"

import <"std/io">
import <"std/lib">
import <"std/string">

import "ast/ast.hsp"
import "lex/token.hsp"
import "util/vector.hsp"
import "util/error.hsp"
import "tck/symtab.hsp"
import "parse/util.hsp"
import "util/generic_funcs.hsp"
import "util/base.hsp"
import "util/lib.hsp"

using std::lib::NULL;
using std::io::printf;
using std::string::strcmp;
using neutrino::util::n_strdup;

namespace neutrino { namespace tck {

func[static] bool typ_compare_helper(type symtab* ctx,
	type ast::typ* t1, type ast::typ* t2,
	bool force_equality, bool directionality,
	bool force_base_equality) {
	type ast::typ_qualifiers* tq1 = t1->typ_qualifiers,
		tq2 = t2->typ_qualifiers;

	if (util::vector_size(tq1->qualifiers)
		!= util::vector_size(tq2->qualifiers)) {
		return false;
	}

	if (force_equality) {
		if (t1->kind != t2->kind)
			return false;

		for (unsigned int i = 0; i < util::vector_size(tq1->qualifiers); i++) {
			bool b1 = util::vector_at(tq1->qualifiers, i) as bool* @,
				b2 = util::vector_at(tq2->qualifiers, i) as bool* @;
			if (b1 != b2)
				return false;
		}
	}
	else {
		if (directionality) {
			for (unsigned int i = 1; i < util::vector_size(tq1->qualifiers); i++) {
				bool b1 = util::vector_at(tq1->qualifiers, i) as bool* @,
					b2 = util::vector_at(tq2->qualifiers, i) as bool* @;
				if (b1 != b2) {
					if (!b1 && b2)
						return false;
				}
			}
		}

		if (t1->kind != t2->kind)
			return false;
	}

	switch (t1->kind) {
	case ast::typ_kind::PRIMITIVE: {
		unsigned int pk1 = t1->which.primitive,
			pk2 = t2->which.primitive;
		if (force_equality || force_base_equality)
			return pk1 == pk2;

		if ((pk1 == ast::primitive_kind::VOID
			|| pk2 == ast::primitive_kind::VOID)
			|| (pk1 == ast::primitive_kind::BOOL
				|| pk2 == ast::primitive_kind::BOOL)) {
			return pk1 == pk2;
		}
		else
			return util::vector_size(tq1->qualifiers) > 1 ? pk1 == pk2 : true;
	}
		break;
	case ast::typ_kind::FN_TYP: {
		type ast::fn_typ* ft1 = t1->which.fn_typ,
			ft2 = t2->which.fn_typ;

		if (!typ_compare_helper(ctx, ft1->return_typ, ft2->return_typ, true, false, true))
			return false;

		if (ft1->is_variadic != ft2->is_variadic)
			return false;

		if (util::vector_size(ft1->parameter_typs) != util::vector_size(ft2->parameter_typs))
			return false;

		unsigned int sz = util::vector_size(ft1->parameter_typs);
		for (unsigned int i = 0; i < sz; i++) {
			type ast::typ* cpt1 = util::vector_at(ft1->parameter_typs, i)
				as type ast::typ** @,
				cpt2 = util::vector_at(ft2->parameter_typs, i)
					as type ast::typ** @;

			if (!typ_compare_helper(ctx, cpt1, cpt2, true, false, true))
				return false;
		}

		return true;
	}
		break;
	case ast::typ_kind::LAMBDA: {
		type ast::lambda_typ* lt1 = t1->which.lambda_typ,
			lt2 = t2->which.lambda_typ;

		if (lt1->unique_identifier != lt2->unique_identifier)
			return false;

		return typ_compare_helper(ctx, lt1->signature, lt2->signature, true, false, true);
	}
		break;
	case ast::typ_kind::TUP: {
		type util::vector* tt1 = t1->which.tup_typ, tt2 = t2->which.tup_typ;

		if (util::vector_size(tt1) != util::vector_size(tt2))
			return false;

		for (unsigned int i = 0; i < util::vector_size(tt1); i++) {
			type ast::typ* curr1 = util::vector_at(tt1, i) as type ast::typ** @,
				curr2 = util::vector_at(tt2, i) as type ast::typ** @;

			if (!typ_compare_helper(ctx, curr1, curr2, false, directionality,
				true)) {
				return false;
			}
		}

		return true;
	}
		break;
	case ast::typ_kind::AGGREGATE_NAME: {
		type ast::qualified_identifier* an1 = t1->which.aggregate_name,
			an2 = t2->which.aggregate_name;

		type util::vector* fn1 = an1->full_name,
			fn2 = an2->full_name;

		util::maybe_ice(fn1 != NULL as type util::vector*
			&& fn2 != NULL as type util::vector*,
			"typ_compare_helper",
			"Expected non-NULL fully qualified type names here!");

		type symtab* ref_ctx1 = an1->ref_ctx,
			ref_ctx2 = an2->ref_ctx;

		util::maybe_ice(ref_ctx1 != NULL as type symtab*
			&& ref_ctx2 != NULL as type symtab*,
			"typ_compare_helper",
			"Expected non-NULL reference contexts to begin searching from for fully qualified names!");

		type symtab_value* sv1 = symtab_lookup_qualified(ref_ctx1, fn1, NULL as type ast::metadata*, false),
			sv2 = symtab_lookup_qualified(ref_ctx2, fn2, NULL as type ast::metadata*, false);

		util::maybe_ice(sv1 != NULL as type symtab_value*
			&& sv2 != NULL as type symtab_value*,
			"typ_compare_helper",
			"Expected valid fully-qualified names here!");

		return sv1 == sv2;
	}
		break;
	default:
		util::ice("typ_compare_helper", "Unrecognized ast::typ_kind while comparing!");
	}

	util::ice("typ_compare_helper", "This should be unreachable!");
}

func bool typ_compare(type symtab* ctx,
	type ast::typ* t1, type ast::typ* t2,
	bool force_equality, bool directionality) {
	return typ_compare_helper(ctx, t1, t2, force_equality, directionality,
		force_equality);
}

func type util::hash_table* prim_typ_promotion_table() {
	type util::hash_table* typ_prom_ht = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);
	util::ht_set(typ_prom_ht, ast::primitive_kind::DOUBLE as byte*, 0 as byte*);
	util::ht_set(typ_prom_ht, ast::primitive_kind::UNSIGNED_LONG as byte*, 1 as byte*);
	util::ht_set(typ_prom_ht, ast::primitive_kind::SIGNED_LONG as byte*, 2 as byte*);
	util::ht_set(typ_prom_ht, ast::primitive_kind::FLOAT as byte*, 3 as byte*);
	util::ht_set(typ_prom_ht, ast::primitive_kind::UNSIGNED_INT as byte*, 4 as byte*);
	util::ht_set(typ_prom_ht, ast::primitive_kind::SIGNED_INT as byte*, 5 as byte*);
	util::ht_set(typ_prom_ht, ast::primitive_kind::UNSIGNED_SHORT as byte*, 6 as byte*);
	util::ht_set(typ_prom_ht, ast::primitive_kind::SIGNED_SHORT as byte*, 7 as byte*);
	util::ht_set(typ_prom_ht, ast::primitive_kind::UNSIGNED_BYTE as byte*, 8 as byte*);
	util::ht_set(typ_prom_ht, ast::primitive_kind::SIGNED_BYTE as byte*, 9 as byte*);
	util::ht_set(typ_prom_ht, ast::primitive_kind::UNSIGNED_CHAR as byte*, 10 as byte*);
	util::ht_set(typ_prom_ht, ast::primitive_kind::SIGNED_CHAR as byte*, 11 as byte*);

	return typ_prom_ht;
}

func type ast::typ_qualifiers* typ_qualifiers_copy(
	type ast::typ_qualifiers* tqs) {
	type util::vector* tmp = util::vector_init(sizeof{bool}, util::no_free);
	for (unsigned int i = 0; i < util::vector_size(tqs->qualifiers); i++) {
		bool b = util::vector_at(tqs->qualifiers, i) as bool* @;
		util::vector_append(tmp, b$ as byte*);
	}
	type ast::typ_qualifiers* ret = ast::typ_qualifiers_init(tmp);
	ret->none_specified = tqs->none_specified;
	return ret;
}

func type ast::typ* typ_copy(type ast::typ* t) {
	type ast::typ_qualifiers* tqs = typ_qualifiers_copy(t->typ_qualifiers);

	switch (t->kind) {
	case ast::typ_kind::PRIMITIVE:
		return ast::typ_primitive_init(tqs, t->which.primitive,
			ast::metadata_copy(t->metadata));
	case ast::typ_kind::FN_TYP: {
		type ast::fn_typ* ft = t->which.fn_typ;

		type ast::typ* rt = typ_copy(ft->return_typ);
		bool iv = ft->is_variadic;

		type util::vector* pts = util::vector_init(sizeof{type ast::typ*},
			parse::deref_typ_free);
		for (unsigned int i = 0; i < util::vector_size(ft->parameter_typs); i++) {
			type ast::typ* pt = util::vector_at(ft->parameter_typs, i)
				as type ast::typ** @;
			type ast::typ* cpt = typ_copy(pt);
			util::vector_append(pts, cpt$ as byte*);
		}

		return ast::typ_fn_typ_init(tqs,
			ast::fn_typ_init(pts, iv, rt),
			ast::metadata_copy(t->metadata));
	}
		break;
	case ast::typ_kind::LAMBDA: {
		type ast::lambda_typ* lt = t->which.lambda_typ;

		type ast::typ* cs = typ_copy(lt->signature);
		return ast::typ_lambda_typ_init(tqs,
			ast::lambda_typ_init(cs, lt->unique_identifier, lt->has_capture_list),
			ast::metadata_copy(t->metadata));
	}
		break;
	case ast::typ_kind::TUP: {
		type util::vector* tt = t->which.tup_typ;

		type util::vector* ret = util::vector_init(sizeof{type ast::typ*},
			parse::deref_typ_free);
		for (unsigned int i = 0; i < util::vector_size(tt); i++) {
			type ast::typ* curr = util::vector_at(tt, i) as type ast::typ** @;
			type ast::typ* temp = typ_copy(curr);

			util::vector_append(ret, temp$ as byte*);
		}

		return ast::typ_tup_init(tqs, ret,
			ast::metadata_copy(t->metadata));
	}
		break;
	case ast::typ_kind::AGGREGATE_NAME: {
		type util::vector* copy = util::vector_init(sizeof{type lex::token*},
			util::no_free);
		type util::vector* orig = t->which.aggregate_name->name;
		type util::vector* orig_full = t->which.aggregate_name->full_name;

		for (unsigned int i = 0; i < util::vector_size(orig); i++) {
			type lex::token* tok = util::vector_at(orig, i)
				as type lex::token** @;
			util::vector_append(copy, tok$ as byte*);
		}

		type ast::typ* ret = ast::typ_aggregate_name_init(tqs,
			ast::qualified_identifier_init(copy,
				ast::metadata_copy(
					t->which.aggregate_name->metadata)),
			ast::metadata_copy(t->metadata));

		if (orig_full != NULL as type util::vector*) {
			type util::vector* copy_full = util::vector_init(
				sizeof{type lex::token*}, util::no_free);

			for (unsigned int i = 0; i < util::vector_size(orig_full); i++) {
				type lex::token* curr = util::vector_at(orig_full, i)
					as type lex::token** @;

				util::vector_append(copy_full, curr$ as byte*);
			}

			ret->which.aggregate_name->full_name = copy_full;
		}

		ret->which.aggregate_name->ref_ctx = t->which.aggregate_name->ref_ctx;
		ret->which.aggregate_name->symtab_value = t->which.aggregate_name->symtab_value;

		return ret;
	}
		break;
	default:
		util::ice("typ_copy", "Unrecognized ast::typ_kind while copying!");
	}

	util::ice("typ_copy", "This should be unreachable!");
}

func bool is_primitive(type ast::typ* t, unsigned int k) {
	return t->kind == ast::typ_kind::PRIMITIVE
		&& util::vector_size(t->typ_qualifiers->qualifiers) == 1
		&& t->which.primitive == k;
}

func bool is_pointer(type ast::typ* t) {
	return util::vector_size(t->typ_qualifiers->qualifiers) > 1;
}

func bool is_integral(type ast::typ* t) {
	return t->kind == ast::typ_kind::PRIMITIVE
		&& util::vector_size(t->typ_qualifiers->qualifiers) == 1
		&& t->which.primitive != ast::primitive_kind::BOOL
		&& t->which.primitive != ast::primitive_kind::VOID
		&& t->which.primitive != ast::primitive_kind::FLOAT
		&& t->which.primitive != ast::primitive_kind::DOUBLE;
}

func bool is_floating_point(type ast::typ* t) {
	return t->kind == ast::typ_kind::PRIMITIVE
		&& util::vector_size(t->typ_qualifiers->qualifiers) == 1
		&& (t->which.primitive == ast::primitive_kind::FLOAT
			|| t->which.primitive == ast::primitive_kind::DOUBLE);
}

func bool is_numeric(type ast::typ* t) {
	return is_integral(t) || is_floating_point(t);
}

func bool is_mut(type ast::typ_qualifiers* tqs) {
	util::maybe_ice(!util::vector_empty(tqs->qualifiers),
		"is_mut",
		"Expected a non-zero number of type qualifiers here!");
	return util::vector_at(tqs->qualifiers, 0) as bool* @;
}

func void set_mut(type ast::typ_qualifiers* tqs, bool b) {
	util::maybe_ice(!util::vector_empty(tqs->qualifiers),
		"set_mut",
		"Expected a non-zero number of type qualifiers here!");
	util::vector_at(tqs->qualifiers, 0) as bool* @ = b;
}

func type ast::typ_qualifiers* mut_non_pointer_typ_qualifiers_init() {
	type util::vector* tmp = util::vector_init(sizeof{bool}, util::no_free);
	bool is_mut = true;
	util::vector_append(tmp, is_mut$ as byte*);
	return ast::typ_qualifiers_init(tmp);
}

func void typ_human_readable(type ast::typ* t, type util::string* s) {
	type ast::typ_qualifiers* tq = t->typ_qualifiers;

	bool added = false;
	for (unsigned int i = 0; i < util::vector_size(tq->qualifiers); i++) {
		bool b = util::vector_at(tq->qualifiers, i) as bool* @;
		if (b) {
			if (i != 0)
				util::string_catc(s, " ");
			util::string_catc(s, "mut");
		}

		if (i != util::vector_size(tq->qualifiers) - 1)
			util::string_catc(s, "*");
		else {
			if (i != 0 || b)
				util::string_catc(s, " ");
		}
	}

	if (added)
		util::string_catc(s, " ");
	switch (t->kind) {
	case ast::typ_kind::PRIMITIVE: {
		switch (t->which.primitive) {
		case ast::primitive_kind::BOOL:
			util::string_catc(s, "bool");
			break;
		case ast::primitive_kind::SIGNED_CHAR:
			util::string_catc(s, "signed char");
			break;
		case ast::primitive_kind::UNSIGNED_CHAR:
			util::string_catc(s, "char");
			break;
		case ast::primitive_kind::SIGNED_BYTE:
			util::string_catc(s, "byte");
			break;
		case ast::primitive_kind::UNSIGNED_BYTE:
			util::string_catc(s, "unsigned byte");
			break;
		case ast::primitive_kind::SIGNED_SHORT:
			util::string_catc(s, "short");
			break;
		case ast::primitive_kind::UNSIGNED_SHORT:
			util::string_catc(s, "unsigned short");
			break;
		case ast::primitive_kind::SIGNED_INT:
			util::string_catc(s, "int");
			break;
		case ast::primitive_kind::UNSIGNED_INT:
			util::string_catc(s, "unsigned int");
			break;
		case ast::primitive_kind::SIGNED_LONG:
			util::string_catc(s, "long");
			break;
		case ast::primitive_kind::UNSIGNED_LONG:
			util::string_catc(s, "unsigned long");
			break;
		case ast::primitive_kind::FLOAT:
			util::string_catc(s, "float");
			break;
		case ast::primitive_kind::DOUBLE:
			util::string_catc(s, "double");
			break;
		case ast::primitive_kind::VOID:
			util::string_catc(s, "void");
			break;
		default:
			util::ice("typ_human_readable", "Unrecognized primitive_kind here!");
		}
	}
		break;
	case ast::typ_kind::AGGREGATE_NAME: {
		type util::vector* v = t->which.aggregate_name->name,
			v2 = t->which.aggregate_name->full_name;

		for (unsigned int i = 0; i < util::vector_size(v); i++) {
			type lex::token* curr = util::vector_at(v, i)
				as type lex::token** @;

			if (curr != NULL as type lex::token*)
				util::string_catc(s, curr->text);

			if (i != util::vector_size(v) - 1)
				util::string_catc(s, "::");
		}

		if (v2 != NULL as type util::vector*) {
			util::string_catc(s, " [");
			for (unsigned int i = 0; i < util::vector_size(v2); i++) {
				type lex::token* curr = util::vector_at(v2, i)
					as type lex::token** @;

				if (curr != NULL as type lex::token*)
					util::string_catc(s, curr->text);

				if (i != util::vector_size(v2) - 1)
					util::string_catc(s, "::");
			}
			util::string_catc(s, "]");
		}
	}
		break;
	case ast::typ_kind::FN_TYP: {
		type ast::fn_typ* ft = t->which.fn_typ;

		util::string_catc(s, "fn (");
		for (unsigned int i = 0; i < util::vector_size(ft->parameter_typs); i++) {
			type ast::typ* cpt = util::vector_at(ft->parameter_typs, i)
				as type ast::typ** @;

			typ_human_readable(cpt, s);

			if (i != util::vector_size(ft->parameter_typs) - 1
				|| ft->is_variadic) {
				util::string_catc(s, ", ");
			}
		}
		if (ft->is_variadic) util::string_catc(s, "...");
		util::string_catc(s, ") : ");

		typ_human_readable(ft->return_typ, s);
	}
		break;
	case ast::typ_kind::LAMBDA: {
		type ast::lambda_typ* lt = t->which.lambda_typ;
		util::string_catc(s, "<lambda ");
		util::utoustr(lt->unique_identifier, s);
		util::string_catc(s, ">");

		if (lt->has_capture_list)
			util::string_catc(s, "(CL)");

		util::string_catc(s, " [");
		typ_human_readable(lt->signature, s);
		util::string_catc(s, "]");
	}
		break;
	case ast::typ_kind::TUP: {
		type util::vector* tt = t->which.tup_typ;

		util::string_catc(s, "(");
		for (unsigned int i = 0; i < util::vector_size(tt); i++) {
			type ast::typ* curr = util::vector_at(tt, i) as type ast::typ** @;

			typ_human_readable(curr, s);

			if (i != util::vector_size(tt) - 1)
				util::string_catc(s, ", ");
		}
		util::string_catc(s, ")");
	}
		break;
	default:
		util::ice("typ_human_readable", "Unrecognized typ_kind here!");
	}
}

func unsigned int qident2token_index(type util::vector* n,
	unsigned int index) {
	type lex::token* initial = util::vector_at(n, 0)
		as type lex::token** @;
	bool global_start = initial == NULL as type lex::token*;

	if (global_start) {
		if (index == 0)
			return 0;
		else if (index == 1)
			return 1;
		else
			return 1 + 2 * (index - 1);
	}
	else
		return 2 * index;
}

func bool prepopulate_exp_labels(type symtab* ctx, type ast::exp* e) {
	switch (e->kind) {
	case ast::exp_kind::PRIMARY: {
		type ast::primary* p = e->which.primary;
		switch (p->kind) {
		case ast::primary_kind::QUALIFIED_IDENTIFIER:
		case ast::primary_kind::LITERAL:
		case ast::primary_kind::LAMBDA:
			return true;
		case ast::primary_kind::PARENTHESIZED:
			return prepopulate_exp_labels(ctx, p->which.parenthesized);
		case ast::primary_kind::TUPLE: {
			type util::vector* tu = p->which.tuple;

			for (unsigned int i = 0; i < util::vector_size(tu); i++) {
				type ast::exp* ce = util::vector_at(tu, i)
					as type ast::exp** @;

				if (!prepopulate_exp_labels(ctx, ce))
					return false;
			}
			return true;
		}
			break;
		case ast::primary_kind::STRUCT_INIT: {
			type ast::struct_init* si = p->which.struct_init;

			for (unsigned int i = 0; i < util::vector_size(si->member_inits); i++) {
				type ast::member_init* mi = util::vector_at(si->member_inits, i)
					as type ast::member_init** @;

				if (!prepopulate_exp_labels(ctx, mi->init))
					return false;
			}

			return true;
		}
			break;
		case ast::primary_kind::ARRAY: {
			type util::vector* arr = p->which.array;

			for (unsigned int i = 0; i < util::vector_size(arr); i++) {
				type ast::exp* ce = util::vector_at(arr, i)
					as type ast::exp** @;

				if (!prepopulate_exp_labels(ctx, ce))
					return false;
			}

			return true;
		}
			break;
		case ast::primary_kind::STMT:
			return prepopulate_stmt_labels(ctx, p->which.stmt);
		default:
			util::ice("prepopulate_exp_Labels",
				"Unrecognized primary_kind while pre-populating labels!");
		}
	}
		break;
	case ast::exp_kind::POSTFIX: {
		type ast::postfix* p = e->which.postfix;

		if (!prepopulate_exp_labels(ctx, p->base))
			return false;

		switch (p->kind) {
		case ast::postfix_kind::INCREMENT:
		case ast::postfix_kind::DECREMENT:
		case ast::postfix_kind::DOT_INDEX:
		case ast::postfix_kind::ARROW_INDEX:
		case ast::postfix_kind::DOT:
		case ast::postfix_kind::ARROW:
		case ast::postfix_kind::AS:
		case ast::postfix_kind::ADDRESS:
		case ast::postfix_kind::AT:
			return true;
		case ast::postfix_kind::INDEX:
			return prepopulate_exp_labels(ctx, p->which.index);
		case ast::postfix_kind::FUNCTION_CALL: {
			type util::vector* args = p->which.arguments;

			for (unsigned int i = 0; i < util::vector_size(args); i++) {
				type ast::exp* a = util::vector_at(args, i)
					as type ast::exp** @;

				if (!prepopulate_exp_labels(ctx, a))
					return false;
			}

			return true;
		}
			break;
		default:
			util::ice("prepopulate_exp_labels",
				"Unrecognized postfix_kind while pre-populating labels!");
		}
	}
		break;
	case ast::exp_kind::UNARY: {
		type ast::unary* u = e->which.unary;

		switch (u->kind) {
		case ast::unary_kind::INCREMENT:
		case ast::unary_kind::DECREMENT:
		case ast::unary_kind::PLUS:
		case ast::unary_kind::MINUS:
		case ast::unary_kind::NOT:
		case ast::unary_kind::CMPL:
			return prepopulate_exp_labels(ctx, u->which.base);
		case ast::unary_kind::SIZEOF_TYP:
		case ast::unary_kind::SIZEOF_EXP:
		case ast::unary_kind::ALIGNOF_TYP:
		case ast::unary_kind::ALIGNOF_EXP:
			return true;
		case ast::unary_kind::DELETE:
			return prepopulate_exp_labels(ctx, u->which.delete_exp);
		case ast::unary_kind::NEW:
		case ast::unary_kind::RESV:
		case ast::unary_kind::STK: {
			type ast::alloc_info* ai = u->which.alloc_info;
			if (ai->exp != NULL as type ast::exp*) {
				if (!prepopulate_exp_labels(ctx, ai->exp))
					return false;
			}

			return true;
		}
			break;
		default:
			util::ice("prepopulate_exp_labels",
				"Unrecognized unary_kind while pre-populating labels!");
		}
	}
		break;
	case ast::exp_kind::BINARY: {
		type ast::binary* b = e->which.binary;

		if (!prepopulate_exp_labels(ctx, b->lhs))
			return false;
		if (!prepopulate_exp_labels(ctx, b->rhs))
			return false;

		return true;
	}
		break;
	case ast::exp_kind::TERNARY: {
		type ast::ternary* t = e->which.ternary;

		if (!prepopulate_exp_labels(ctx, t->cond))
			return false;
		if (!prepopulate_exp_labels(ctx, t->lhs))
			return false;
		if (!prepopulate_exp_labels(ctx, t->rhs))
			return false;

		return true;
	}
		break;
	case ast::exp_kind::PAT_ASSIGN: {
		type ast::pat_assign* pa = e->which.pat_assign;

		if (!prepopulate_pat_labels(ctx, pa->lhs))
			return false;

		if (!prepopulate_exp_labels(ctx, pa->rhs))
			return false;

		return true;
	}
		break;
	default:
		util::ice("prepopulate_exp_labels",
			"Unrecognized exp_kind while pre-populating labels!");
	}

	return false;
}

func bool prepopulate_decl_labels(type symtab* ctx, type ast::decl* d) {
	for (unsigned int i = 0; i < util::vector_size(d->decl_components); i++) {
		type ast::decl_component* dc = util::vector_at(d->decl_components, i)
			as type ast::decl_component** @;

		if (dc->init != NULL as type ast::exp*) {
			if (!prepopulate_exp_labels(ctx, dc->init))
				return false;
		}
	}

	return true;
}

func bool prepopulate_pat_labels(type symtab* ctx, type ast::pat* p) {
	switch (p->kind) {
	case ast::pat_kind::WILDCARD:
	case ast::pat_kind::IDENT:
	case ast::pat_kind::RANGE:
	case ast::pat_kind::LITERAL:
		return true;
	case ast::pat_kind::NESTED:
		return prepopulate_pat_labels(ctx, p->which.nested);
	case ast::pat_kind::TUPLE: {
		type ast::tuple_pat* tp = p->which.tuple_pat;

		if (tp->first != NULL as type util::vector*) {
			for (unsigned int i = 0; i < util::vector_size(tp->first); i++) {
				type ast::pat* curr_pat = util::vector_at(tp->first, i)
					as type ast::pat** @;

				if (!prepopulate_pat_labels(ctx, curr_pat))
					return false;
			}
		}

		if (tp->last != NULL as type util::vector*) {
			for (unsigned int i = 0; i < util::vector_size(tp->last); i++) {
				type ast::pat* curr_pat = util::vector_at(tp->last, i)
					as type ast::pat** @;

				if (!prepopulate_pat_labels(ctx, curr_pat))
					return false;
			}
		}

		return true;
	}
		break;
	case ast::pat_kind::CONSTRUCTOR: {
		type ast::constructor_pat* cp = p->which.constructor_pat;

		if (cp->nested != NULL as type util::vector*) {
			for (unsigned int i = 0; i < util::vector_size(cp->nested); i++) {
				type ast::pat* curr_pat = util::vector_at(cp->nested, i)
					as type ast::pat** @;

				if (!prepopulate_pat_labels(ctx, curr_pat))
					return false;
			}
		}

		return true;
	}
		break;
	case ast::pat_kind::STRUCT: {
		type ast::struct_pat* sp = p->which.struct_pat;

		for (unsigned int i = 0; i < util::vector_size(sp->member_pats); i++) {
			type ast::member_pat* mp = util::vector_at(sp->member_pats, i)
				as type ast::member_pat** @;

			if (mp->nested != NULL as type ast::pat*) {
				if (!prepopulate_pat_labels(ctx, mp->nested))
					return false;
			}
		}

		return true;
	}
		break;
	case ast::pat_kind::EXP:
		return prepopulate_exp_labels(ctx, p->which.exp);
	}
}

func bool prepopulate_stmt_labels(type symtab* ctx, type ast::stmt* s) {
	switch (s->kind) {
	case ast::stmt_kind::EXP:
		return prepopulate_exp_labels(ctx, s->which.exp);
	case ast::stmt_kind::USING_NAMESPACE:
	case ast::stmt_kind::USING:
	case ast::stmt_kind::NAMESPACE_ALIAS:
	case ast::stmt_kind::EMPTY:
	case ast::stmt_kind::AGGREGATE:
	case ast::stmt_kind::ASM:
	case ast::stmt_kind::INCLUDE:
	case ast::stmt_kind::IMPORT:
	case ast::stmt_kind::TYPE_ALIAS:
		return true;
	case ast::stmt_kind::MATCH: {
		type ast::match_data* md = s->which.match_data;

		for (unsigned int i = 0; i < util::vector_size(md->match_branches); i++) {
			type ast::match_branch* mb = util::vector_at(md->match_branches, i)
				as type ast::match_branch** @;

			for (unsigned int j = 0; j < util::vector_size(mb->pats); j++) {
				type ast::pat* curr_pat = util::vector_at(mb->pats, j)
					as type ast::pat** @;

				if (!prepopulate_pat_labels(ctx, curr_pat))
					return false;
			}

			if (mb->guard != NULL as type ast::exp*) {
				if (!prepopulate_exp_labels(ctx, mb->guard))
					return false;
			}

			if (!prepopulate_stmt_labels(ctx, mb->body))
				return false;
		}

		return true;
	}
		break;
	case ast::stmt_kind::DECL:
		return prepopulate_decl_labels(ctx, s->which.decl);
	case ast::stmt_kind::COMPOUND: {
		type ast::compound* c = s->which.compound;

		for (unsigned int i = 0; i < util::vector_size(c->stmts); i++) {
			type ast::stmt* cs = util::vector_at(c->stmts, i)
				as type ast::stmt** @;

			if (!prepopulate_stmt_labels(ctx, cs))
				return false;
		}

		return true;
	}
		break;
	case ast::stmt_kind::LABELED: {
		type ast::labeled* l = s->which.labeled;

		switch (l->kind) {
		case ast::labeled_kind::LABEL: {
			type symtab_value* check = NULL as type symtab_value*;
			if (util::ht_get(ctx->symbols, l->name->text as byte*, check$ as byte**)) {
				type ast::metadata m;
				m.token_stream = l->metadata->token_stream;
				m.start = l->metadata->start;
				m.end = m.start + 3;

				util::report_ast_metadata(util::error_kind::ERROR,
					"Duplicate symbol name found here for a label (all labels are at the function-scope level).",
					m$, 1, 1, true);

				util::report_token(util::error_kind::NOTE,
					"Originally declared here.", check->name);
				return false;
			}

			type util::vector* tmp = util::vector_init(sizeof{bool}, util::no_free);
			bool base = false, pointer = true;
			util::vector_append(tmp, pointer$ as byte*);
			util::vector_append(tmp, base$ as byte*);
			type symtab_value* sv = symtab_value_label_init(ctx, false,
				false, l->name,
				ast::typ_primitive_init(
					ast::typ_qualifiers_init(tmp),
					ast::primitive_kind::SIGNED_BYTE, NULL as type ast::metadata*),
				l);

			char* cname = n_strdup(l->name->text);
			util::ht_set(ctx->symbols, cname as byte*, sv as byte*);

			l->symtab_value = sv;
		}
			break;
		case ast::labeled_kind::DEFAULT:
			break;
		case ast::labeled_kind::CASE: {
			if (!prepopulate_exp_labels(ctx, l->exp))
				return false;
		}
			break;
		default:
			util::ice("prepopulate_stmt_labels",
				"Unrecognized labeled_kind while pre-populating labels!");
		}

		return prepopulate_stmt_labels(ctx, l->stmt);
	}
		break;
	case ast::stmt_kind::SELECTION: {
		type ast::selection* sel = s->which.selection;

		if (sel->decl != NULL as type ast::decl*) {
			if (!prepopulate_decl_labels(ctx, sel->decl))
				return false;
		}

		if (sel->exp != NULL as type ast::exp*) {
			if (!prepopulate_exp_labels(ctx, sel->exp))
				return false;
		}

		if (sel->stmt1 != NULL as type ast::stmt*) {
			if (!prepopulate_stmt_labels(ctx, sel->stmt1))
				return false;
		}

		if (sel->stmt2 != NULL as type ast::stmt*) {
			if (!prepopulate_stmt_labels(ctx, sel->stmt2))
				return false;
		}

		return true;
	}
		break;
	case ast::stmt_kind::ITERATION: {
		type ast::iteration* i = s->which.iteration;

		if (i->decl != NULL as type ast::decl*) {
			if (!prepopulate_decl_labels(ctx, i->decl))
				return false;
		}

		if (i->exp1 != NULL as type ast::exp*) {
			if (!prepopulate_exp_labels(ctx, i->exp1))
				return false;
		}

		if (i->exp2 != NULL as type ast::exp*) {
			if (!prepopulate_exp_labels(ctx, i->exp2))
				return false;
		}

		if (i->exp3 != NULL as type ast::exp*) {
			if (!prepopulate_exp_labels(ctx, i->exp3))
				return false;
		}

		if (i->stmt != NULL as type ast::stmt*) {
			if (!prepopulate_stmt_labels(ctx, i->stmt))
				return false;
		}

		return true;
	}
		break;
	case ast::stmt_kind::JUMP: {
		type ast::jump* j = s->which.jump;

		if (j->exp != NULL as type ast::exp*) {
			if (!prepopulate_exp_labels(ctx, j->exp))
			return false;
		}

		return true;
	}
		break;
	default:
		util::ice("prepopulate_stmt_labels",
			"Unrecognized stmt_kind while pre-populating labels!");
	}

	return false;
}

func bool attribute_compare(type ast::attribute* a1, type ast::attribute* a2) {
	if (a1 == NULL as type ast::attribute*
		&& a2 == NULL as type ast::attribute*) {
		return true;
	}
	else if (a1 != NULL as type ast::attribute*
		&& a2 == NULL as type ast::attribute*) {
		return false;
	}
	else if (a1 == NULL as type ast::attribute*
		&& a2 != NULL as type ast::attribute*) {
		return false;
	}

	util::maybe_ice(a1->attrs != NULL as type util::hash_table*
		&& a2->attrs != NULL as type util::hash_table*,
		"attribute_compare",
		"Expected a non-NULL attribute key-value mappings at this point!");

	if (util::ht_size(a1->attrs) != util::ht_size(a2->attrs))
		return false;

	type util::hash_table_iterator* ht_iter = util::ht_iter_init(a1->attrs);
	for (; !util::ht_iter_done(ht_iter); util::ht_iter_advance(ht_iter)) {
		type util::hash_table_entry* hte = util::ht_iter_curr(ht_iter);

		unsigned int attr_key = hte->key as unsigned int;
		type lex::token* attr_val = hte->value as type lex::token*;

		type lex::token* check_attr_val;
		if (!util::ht_get(a2->attrs, attr_key as byte*, check_attr_val$ as byte**))
			return false;

		if (check_attr_val != NULL as type lex::token*
			&& attr_val != NULL as type lex::token*) {
			if (check_attr_val->kind != attr_val->kind)
				return false;

			switch (attr_val->kind) {
			case lex::token_kind::STRING: {
				if (strcmp(attr_val->value.string_value,
					check_attr_val->value.string_value) != 0) {
					return false;
				}
			}
				break;
			case lex::token_kind::INTEGER: {
				if (attr_val->value.integral_value
					!= check_attr_val->value.integral_value) {
					return false;
				}
			}
				break;
			default:
				util::ice("attribute_compare",
					"Unrecognized value token_kind while comparing attributes!");
			}
		}
		else if (check_attr_val == NULL as type lex::token*
			&& attr_val != NULL as type lex::token*) {
			return false;
		}
		else if (check_attr_val != NULL as type lex::token*
			&& attr_val == NULL as type lex::token*) {
			return false;
		}
	}
	util::ht_iter_delete(ht_iter);

	return true;
}

func type ast::typ* literal2typ(type lex::token* lit) {
	switch (lit->kind) {
	case lex::token_kind::INTEGER: {
		unsigned int k;
		switch (lit->suffix_kind) {
		case lex::suffix_kind::NONE:
			k = ast::primitive_kind::SIGNED_INT;
			break;
		case lex::suffix_kind::UNSIGNED_INT:
			k = ast::primitive_kind::UNSIGNED_INT;
			break;
		case lex::suffix_kind::UNSIGNED_LONG:
			k = ast::primitive_kind::UNSIGNED_LONG;
			break;
		case lex::suffix_kind::SIGNED_LONG:
			k = ast::primitive_kind::SIGNED_LONG;
			break;
		case lex::suffix_kind::UNSIGNED_SHORT:
			k = ast::primitive_kind::UNSIGNED_SHORT;
			break;
		case lex::suffix_kind::SIGNED_SHORT:
			k = ast::primitive_kind::SIGNED_SHORT;
			break;
		default:
			util::ice("tck_exp_primary",
				"Unrecognized/invalid integer suffix_kind for literal while tck'ing!");
		}
		return ast::typ_primitive_init(mut_non_pointer_typ_qualifiers_init(),
			k, NULL as type ast::metadata*);
	}
		break;
	case lex::token_kind::DECIMAL: {
		unsigned int k;
		switch (lit->suffix_kind) {
		case lex::suffix_kind::NONE:
			k = ast::primitive_kind::DOUBLE;
			break;
		case lex::suffix_kind::FLOAT:
			k = ast::primitive_kind::FLOAT;
			break;
		case lex::suffix_kind::DOUBLE:
			k = ast::primitive_kind::DOUBLE;
			break;
		default:
			util::ice("tck_exp_primary",
				"Unrecognized/invalid decimal suffix_kind for literal while tck'ing!");
		}

		return ast::typ_primitive_init(mut_non_pointer_typ_qualifiers_init(),
			k, NULL as type ast::metadata*);
	}
		break;
	case lex::token_kind::STRING: {
		type util::vector* tmp = util::vector_init(sizeof{bool}, util::no_free);
		bool pointer = true, base = true;
		util::vector_append(tmp, pointer$ as byte*);
		util::vector_append(tmp, base$ as byte*);

		return ast::typ_primitive_init(ast::typ_qualifiers_init(tmp),
			ast::primitive_kind::UNSIGNED_CHAR, NULL as type ast::metadata*);
	}
		break;
	case lex::token_kind::CHARACTER:
		return ast::typ_primitive_init(mut_non_pointer_typ_qualifiers_init(),
			ast::primitive_kind::UNSIGNED_CHAR, NULL as type ast::metadata*);
	case lex::token_kind::TRUE:
	case lex::token_kind::FALSE:
		return ast::typ_primitive_init(mut_non_pointer_typ_qualifiers_init(),
			ast::primitive_kind::BOOL, NULL as type ast::metadata*);
		break;
	default:
		util::ice("literal2typ",
			"Unrecognized token_kind while tck'ing literal!");
	}

	util::ice("literal2typ",
		"This should be unreachable!");
	return NULL as type ast::typ*;
}

} } // namespace neutrino::tck
