import "ir/ir.hsp"

import <"std/io">
import <"std/lib">

import "ir/insn.hsp"
import "ast/ast.hsp"
import "util/error.hsp"
import "ir/util.hsp"
import "util/vector.hsp"
import "util/hash_table.hsp"

using std::io::printf;
using std::lib::NULL;

namespace neutrino { namespace ir {

func type prog* ir_prog(type ir_ctx* ctx, type ast::prog* p) {
	type util::vector* tls = util::vector_init(sizeof{type top_level*},
		deref_top_level_free);

	util::stack_push(ctx->ctx_stack, p->ctx$ as byte*);

	// TODO

	for (unsigned int i = 0; i < util::vector_size(p->top_levels); i++) {
		type ast::top_level* tl = util::vector_at(p->top_levels, i)
			as type ast::top_level** @;

		switch (tl->kind) {
		case ast::top_level_kind::USING:
		case ast::top_level_kind::USING_NAMESPACE:
		case ast::top_level_kind::EMPTY:
		case ast::top_level_kind::TYPE_ALIAS:
		case ast::top_level_kind::INCLUDE:
		case ast::top_level_kind::NAMESPACE_ALIAS:
			break;
		case ast::top_level_kind::DECL: {
			type ast::decl* d = tl->which.stmt->which.decl;

			// TODO
		}
			break;
		case ast::top_level_kind::FUN: {
			type ast::function* f = tl->which.function;

			type ir::function* i_f = ir_fun(ctx, f);
			if (i_f == NULL as type ir::function*)
				return NULL as type ir::prog*;

			type top_level* itl = top_level_function_init(i_f);

			util::vector_append(tls, itl$ as byte*);
		}
			break;
		// TODO
		default:
			util::ice("ir_prog",
				"Unrecognized ast::top_level_kind while generating IR!");
		}
	}

	return prog_init(tls);
}

} } // namespace neutrino::ir
