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

using std::io::printf;
using std::lib::NULL;

namespace neutrino { namespace tck {

func bool tck_exp_ternary(type symtab* ctx, type ast::ternary* t) {
	if (!tck_exp(ctx, t->cond))
		return false;

	if (!is_primitive(t->cond->typ, ast::primitive_kind::BOOL)) {
		type util::string* err_str = util::string_init("Expected a boolean condition type for a ternary expression; found '");
		typ_human_readable(t->cond->typ, err_str);
		util::string_catc(err_str, "' instead.");

		util::report_ast_metadata(util::error_kind::ERROR,
			util::string_data(err_str),
			t->metadata, 0, (t->cond->metadata->end - t->metadata->start) - 1, true);
		util::string_delete(err_str);
		return false;
	}

	if (!tck_exp(ctx, t->lhs))
		return false;
	if (!tck_exp(ctx, t->rhs))
		return false;

	type ast::typ* lt = t->lhs->typ, ltc = typ_copy(lt), rt = t->rhs->typ, rtc = typ_copy(rt);
	type ast::typ_qualifiers* ltqs = lt->typ_qualifiers, rtqs = rt->typ_qualifiers;

	bool final_mut = is_mut(ltqs) && is_mut(rtqs);

	set_mut(ltc->typ_qualifiers, true);
	set_mut(rtc->typ_qualifiers, true);

	bool err = false;
	if (typ_compare(ctx, ltc, rtc, true, false)) {
		ast::typ_delete(rtc);
		t->value_kind = t->lhs->value_kind == ast::value_kind::LVALUE
			&& t->rhs->value_kind == ast::value_kind::LVALUE
			? ast::value_kind::LVALUE : ast::value_kind::RVALUE;
		t->typ = ltc;
		set_mut(t->typ->typ_qualifiers, final_mut);
		t->is_constant = t->cond->is_constant && t->lhs->is_constant && t->rhs->is_constant;
		return true;
	}
	else if (!typ_compare(ctx, rt, lt, false, false))
		err = true;

	if (err) {
		type util::string* err_str = util::string_init(
			"Expected compatible operand types between branches of a ternary expression; found '");
		typ_human_readable(t->lhs->typ, err_str);
		util::string_catc(err_str, "' and '");
		typ_human_readable(t->rhs->typ, err_str);
		util::string_catc(err_str, "' instead.");

		type ast::metadata tmp;
		tmp.token_stream = t->metadata->token_stream;
		tmp.start = t->lhs->metadata->start;
		tmp.end = t->rhs->metadata->end;

		unsigned int token_index = (t->lhs->metadata->end - tmp.start);
		util::report_ast_metadata(util::error_kind::ERROR,
			util::string_data(err_str),
			tmp$, token_index, token_index, true);
		util::string_delete(err_str);
		return false;
	}

	ast::typ_delete(rtc);

	util::maybe_ice(rt->kind == lt->kind && util::vector_size(ltqs->qualifiers)
		== util::vector_size(rtqs->qualifiers),
		"tck_exp_ternary",
		"Also expected matching typ_kind's and pointer_level's at this point!");

	t->value_kind = ast::value_kind::RVALUE;
	if (lt->kind == ast::typ_kind::PRIMITIVE && util::vector_size(ltqs->qualifiers) == 1) {
		ast::typ_delete(ltc);
		type util::hash_table* typ_prom_ht = prim_typ_promotion_table();

		unsigned int prec1, prec2;
		util::maybe_ice(util::ht_get(typ_prom_ht, lt->which.primitive as byte*, prec1$ as byte**)
				&& util::ht_get(typ_prom_ht, rt->which.primitive as byte*, prec2$ as byte**),
			"tck_exp_ternary", "Expected to find primitive types in the type promotion hierarchy here!");

		t->typ = ast::typ_primitive_init(mut_non_pointer_typ_qualifiers_init(),
			prec1 < prec2 ? lt->which.primitive : rt->which.primitive,
			NULL as type ast::metadata*);
		util::ht_delete(typ_prom_ht);
	}
	else {
		t->typ = ltc;
		set_mut(t->typ->typ_qualifiers, final_mut);
	}
	t->is_constant = t->cond->is_constant && t->lhs->is_constant && t->rhs->is_constant;

	return true;
}

} } // namespace neutrino::tck
