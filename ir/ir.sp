import "ir/ir.hsp"

import <"std/io">
import <"std/lib">

import "ir/insn.hsp"
import "ir/operand.hsp"
import "ir/util.hsp"
import "util/base.hsp"
import "tck/symtab.hsp"
import "util/stack.hsp"
import "util/vector.hsp"
import "util/generic_funcs.hsp"

using std::io::printf;
using std::lib::NULL;
using neutrino::util::n_malloc;
using neutrino::util::n_free;

namespace neutrino { namespace ir {

func type ir_ctx* ir_ctx_init(type tck::symtab* global) {
	type ir_ctx* ic = n_malloc(sizeof{type ir_ctx})
		as type ir_ctx*;
	ic->name_2_aggregate = util::ht_init(
		util::str_hash, util::str_eq,
		util::no_free, top_level_free);
	ic->local_name_2_reg = NULL as type util::hash_table*;
	ic->global_name_2_label = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);

	ic->counter = 0;

	ic->curr_header = NULL as type label_operand*;
	ic->curr_footer = NULL as type label_operand*;
	ic->defer_header = NULL as type label_operand*;
	ic->err_defer_header = NULL as type label_operand*;

	ic->insns = NULL as type util::vector*;
	ic->pre_insns = NULL as type util::vector*;
	ic->post_insns = NULL as type util::vector*;

	ic->defer_stack = util::stack_init(sizeof{type util::vector*},
		util::deref_vector_free);
	ic->err_defer_stack = util::stack_init(sizeof{type util::vector*},
		util::deref_vector_free);
	ic->ctx_stack = util::stack_init(sizeof{type tck::symtab*}, util::no_free);
	util::stack_push(ic->ctx_stack, global$ as byte*);
	return ic;
}

func void ir_ctx_delete(type ir_ctx* ic) {
	util::ht_delete(ic->name_2_aggregate);
	util::ht_delete(ic->global_name_2_label);
	util::stack_delete(ic->defer_stack);
	util::stack_delete(ic->err_defer_stack);
	n_free(ic as byte*);
}

} } // namespace neutrino::ir
