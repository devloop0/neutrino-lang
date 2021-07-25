import "ir/ir.hsp"

import <"std/io">
import <"std/lib">

import "ast/ast.hsp"
import "util/error.hsp"
import "util/vector.hsp"
import "util/stack.hsp"

using std::io::printf;
using std::lib::NULL;

namespace neutrino { namespace ir {

func bool ir_stmt(type ir_ctx* ctx, type ast::stmt* s) {
	switch (s->kind) {
	case ast::stmt_kind::EXP: {
		type ast::exp* e = s->which.exp;

		type operand* o = ir_exp(ctx, e, false);
		operand_print(o), printf("\n");
		if (o == NULL as type operand*)
			return false;
		operand_delete(o);

		return true;
	}
		break;
	case ast::stmt_kind::COMPOUND: {
		type ast::compound* c = s->which.compound;

		util::stack_push(ctx->ctx_stack, c->ctx$ as byte*);

		for (unsigned int i = 0; i < util::vector_size(c->stmts); i++) {
			type ast::stmt* cs = util::vector_at(c->stmts, i)
				as type ast::stmt** @;

			if (!ir_stmt(ctx, cs))
				return false;
		}

		util::stack_pop(ctx->ctx_stack);

		return true;
	}
		break;
	case ast::stmt_kind::DECL: {
		type ast::decl* d = s->which.decl;

		if (!ir_decl(ctx, d))
			return false;

		return true;
	}
		break;
	// TODO
	default:
		util::ice("ir_stmt",
			"Unrecognized ast::stmt_kind while generating a stmt's IR!");
	}

	util::ice("ir_stmt",
		"This should be unreachable!");
}

} } // namespace neutrino::ir
