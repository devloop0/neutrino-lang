import "tck/tck.hsp"

import <"std/io">
import <"std/lib">

import "ast/ast.hsp"
import "tck/symtab.hsp"
import "util/error.hsp"
import "util/vector.hsp"
import "tck/exp_helpers.hsp"
import "tck/util.hsp"

using std::lib::NULL;
using std::io::printf;

namespace neutrino { namespace tck {

func bool tck_exp(type symtab* ctx, type ast::exp* e) {
	switch (e->kind) {
	case ast::exp_kind::PRIMARY: {
		if (!tck_exp_primary(ctx, e->which.primary))
			return false;

		e->value_kind = e->which.primary->value_kind;
		e->typ = typ_copy(e->which.primary->typ);
		e->is_constant = e->which.primary->is_constant;
		return true;
	}
		break;
	case ast::exp_kind::POSTFIX: {
		if (!tck_exp_postfix(ctx, e->which.postfix))
			return false;

		e->value_kind = e->which.postfix->value_kind;
		e->typ = typ_copy(e->which.postfix->typ);
		e->is_constant = e->which.postfix->is_constant;
		return true;
	}
		break;
	case ast::exp_kind::UNARY: {
		if (!tck_exp_unary(ctx, e->which.unary))
			return false;

		e->value_kind = e->which.unary->value_kind;
		e->typ = typ_copy(e->which.unary->typ);
		e->is_constant = e->which.unary->is_constant;
		return true;
	}
		break;
	case ast::exp_kind::BINARY: {
		if (!tck_exp_binary(ctx, e->which.binary))
			return false;

		e->value_kind = e->which.binary->value_kind;
		e->typ = typ_copy(e->which.binary->typ);
		e->is_constant = e->which.binary->is_constant;
		return true;
	}
		break;
	case ast::exp_kind::TERNARY: {
		if (!tck_exp_ternary(ctx, e->which.ternary))
			return false;

		e->value_kind = e->which.ternary->value_kind;
		e->typ = typ_copy(e->which.ternary->typ);
		e->is_constant = e->which.ternary->is_constant;
		return true;
	}
		break;
	case ast::exp_kind::PAT_ASSIGN: {
		type ast::pat_assign* pa = e->which.pat_assign;

		if (!tck_exp(ctx, pa->rhs))
			return false;

		if (!tck_pat(ctx, pat_ctx_kind::EXP, pa->lhs, pa->rhs->typ,
			NULL as type ast::attribute*)) {
			return false;
		}

		e->value_kind = ast::value_kind::RVALUE;
		e->typ = ast::typ_primitive_init(mut_non_pointer_typ_qualifiers_init(),
			ast::primitive_kind::VOID, NULL as type ast::metadata*);
		e->is_constant = false;
		return true;
	}
		break;
	default:
		util::ice("tck_exp",
			"Unrecognized exp_kind while tck'ing!");
	}

	return false;
}

} } // namespace neutrino::tck
