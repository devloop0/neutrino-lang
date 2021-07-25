import "ir/ir.hsp"

import <"std/io">
import <"std/lib">

import "ir/operand.hsp"
import "ir/insn.hsp"
import "ast/ast.hsp"
import "util/error.hsp"
import "util/stack.hsp"
import "tck/symtab.hsp"

using std::io::printf;
using std::lib::NULL;

namespace neutrino { namespace ir {

func bool ir_decl(type ir_ctx* ctx, type ast::decl* d) {
	type tck::symtab* enclosing = util::stack_top(ctx->ctx_stack) as type tck::symtab** @;
	bool is_global = enclosing->kind == tck::symtab_kind::GLOBAL
		|| enclosing->kind == tck::symtab_kind::NAMESPACE;

	for (unsigned int i = 0; i < util::vector_size(d->decl_components); i++) {
		type ast::decl_component* dc = util::vector_at(d->decl_components, i)
			as type ast::decl_component** @;

		if (!is_global && !d->is_const && !d->is_static) {
			type operand* to_assign = NULL as type operand*;
			type ast::typ* from_typ = NULL as type ast::typ*;
			if (dc->init != NULL as type ast::exp*) {
				to_assign = ir_exp(ctx, dc->init, false);
				if (to_assign == NULL as type operand*)
					return false;
				from_typ = dc->init->typ;
			}
			else {
				to_assign = NULL as type operand*;
				from_typ = NULL as type ast::typ*;
			}

			if (!ir_pat_nc_assign(ctx, dc->pat, from_typ, to_assign))
				return false;
		}

		// TODO
	}

	return true;
}

} } // namespace neutrino::ir
