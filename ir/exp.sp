import "ir/ir.hsp"

import <"std/io">
import <"std/lib">

import "ir/exp_helpers.hsp"
import "ir/ir.hsp"
import "ast/ast.hsp"
import "util/error.hsp"
import "util/vector.hsp"

using std::io::printf;
using std::lib::NULL;

namespace neutrino { namespace ir {

func type operand* ir_exp(type ir_ctx* ctx, type ast::exp* e, bool lvalue) {
	switch (e->kind) {
	case ast::exp_kind::PRIMARY: {
		type operand* o = ir_exp_primary(ctx, e->which.primary, lvalue);
		if (o == NULL as type operand*)
			return NULL as type operand*;

		return o;
	}
		break;
	// TODO
	default:
		util::ice("ir_exp",
			"Unrecognized ast::exp_kind while generating IR for an expression!");
	}

	util::ice("ir_exp",
		"This should be unreachable!");
}

} } // namespace neutrino::Ir
