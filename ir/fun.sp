import "ir/ir.hsp"

import <"std/io">
import <"std/lib">

import "ir/typ.hsp"
import "ir/insn.hsp"
import "ir/operand.hsp"
import "ir/util.hsp"
import "ast/ast.hsp"
import "util/error.hsp"
import "util/hash_table.hsp"
import "util/vector.hsp"
import "util/generic_funcs.hsp"
import "util/stack.hsp"

using std::io::printf;
using std::lib::NULL;

namespace neutrino { namespace ir {

func type function* ir_fun(type ir_ctx* ctx, type ast::function* f) {
	char* name = mangle_name(f->symtab_value);

	bool is_public = f->attribute != NULL as type ast::attribute*
		&& f->attribute->attrs != NULL as type util::hash_table*
		&& util::ht_contains(f->attribute->attrs, ast::attribute_value_kind::PUBLIC as byte*);

	util::maybe_ice(f->symtab_value->typ->kind == ast::typ_kind::FN_TYP,
		"ir_fun",
		"Expected a function type for a 'fun'!");
	type ast::fn_typ* f_typ = f->symtab_value->typ->which.fn_typ;

	type typ* rt = ast_2_ir_typ(ctx, f_typ->return_typ);

	type util::vector* pars = util::vector_init(sizeof{type function_parameter*},
		deref_function_parameter_free);

	ctx->local_name_2_reg = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);

	type util::vector* insns = util::vector_init(sizeof{type insn*}, util::no_free),
		pre_insns = util::vector_init(sizeof{type insn*}, util::no_free),
		post_insns = util::vector_init(sizeof{type insn*}, util::no_free);

	ctx->insns = insns;
	ctx->pre_insns = pre_insns;
	ctx->post_insns = post_insns;

	for (unsigned int i = 0; i < util::vector_size(f_typ->parameter_typs); i++) {
		type ast::typ* par_typ = util::vector_at(f_typ->parameter_typs, i)
			as type ast::typ** @;

		type register_operand* ro = register_operand_init(ctx->counter++);
		type typ* pt = ast_2_ir_typ(ctx, par_typ);

		type function_parameter* fp = function_parameter_init(pt, ro);
		util::vector_append(pars, fp$ as byte*);

		if (f->body != NULL as type ast::stmt*) {
			// TODO
		}
	}

	if (f->body == NULL as type ast::stmt*)
		return function_init(!is_public, name, pars, f->is_variadic, rt, NULL as type util::vector*);

	if (!ir_stmt(ctx, f->body))
		return NULL as type function*;

	type util::vector* final_insns = util::vector_init(sizeof{type insn*}, deref_insn_free);

	for (unsigned int i = 0; i < util::vector_size(ctx->pre_insns); i++) {
		type insn* ci = util::vector_at(ctx->pre_insns, i) as type insn** @;

		util::vector_append(final_insns, ci$ as byte*);
	}

	for (unsigned int i = 0; i < util::vector_size(ctx->insns); i++) {
		type insn* ci = util::vector_at(ctx->insns, i) as type insn** @;

		util::vector_append(final_insns, ci$ as byte*);
	}

	for (unsigned int i = 0; i < util::vector_size(ctx->post_insns); i++) {
		type insn* ci = util::vector_at(ctx->post_insns, i) as type insn** @;

		util::vector_append(final_insns, ci$ as byte*);
	}

	util::vector_delete(ctx->insns);
	util::vector_delete(ctx->pre_insns);
	util::vector_delete(ctx->post_insns);
	ctx->insns = NULL as type util::vector*;
	ctx->pre_insns = NULL as type util::vector*;
	ctx->post_insns = NULL as type util::vector*;

	util::ht_delete(ctx->local_name_2_reg);
	ctx->local_name_2_reg = NULL as type util::hash_table*;

	return function_init(!is_public, name, pars,
		f->is_variadic, rt, final_insns);
}

} } // namespace neutrino::ir
