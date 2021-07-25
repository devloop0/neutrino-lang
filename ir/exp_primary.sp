import "ir/exp_helpers.hsp"

import <"std/io">
import <"std/lib">

import "lex/token.hsp"
import "ir/ir.hsp"
import "ir/operand.hsp"
import "ir/util.hsp"
import "ast/ast.hsp"
import "util/error.hsp"

using std::io::printf;
using std::lib::NULL;

namespace neutrino { namespace ir {

func type operand* ir_exp_primary(type ir_ctx* ctx, type ast::primary* p, bool lvalue) {
	switch (p->kind) {
	case ast::primary_kind::LITERAL: {
		type lex::token* lit = p->which.literal;

		switch (lit->kind) {
		case lex::token_kind::INTEGER: {
			switch (lit->suffix_kind) {
			case lex::suffix_kind::UNSIGNED_INT:
			case lex::suffix_kind::NONE:
				return operand_immediate_init(immediate_operand_integral_init(
					lit->value.integral_value), typ_primitive_init(primitive_kind::WORD));
			case lex::suffix_kind::SIGNED_SHORT:
			case lex::suffix_kind::UNSIGNED_SHORT:
				return operand_immediate_init(immediate_operand_integral_init(
					lit->value.integral_value), typ_primitive_init(primitive_kind::HALF));
			case lex::suffix_kind::UNSIGNED_LONG:
			case lex::suffix_kind::SIGNED_LONG:
				return operand_immediate_init(immediate_operand_integral_init(
					lit->value.integral_value), typ_primitive_init(primitive_kind::LONG));
			default:
				util::ice("ir_exp_primary",
					"Unrecognized suffix_kind while generating IR for an integer!");
			}
		}
			break;
		case lex::token_kind::TRUE:
			return operand_immediate_init(immediate_operand_integral_init(
				1), typ_primitive_init(primitive_kind::BYTE));
		case lex::token_kind::FALSE:
			return operand_immediate_init(immediate_operand_integral_init(
				0), typ_primitive_init(primitive_kind::BYTE));
		case lex::token_kind::CHARACTER:
			return operand_immediate_init(immediate_operand_integral_init(
				lit->value.integral_value), typ_primitive_init(primitive_kind::BYTE));
		case lex::token_kind::DECIMAL: {
			switch (lit->suffix_kind) {
			case lex::suffix_kind::FLOAT:
				return operand_immediate_init(immediate_operand_floating_init(
					lit->value.decimal_value), typ_primitive_init(primitive_kind::SINGLE));
			case lex::suffix_kind::NONE:
			case lex::suffix_kind::DOUBLE:
				return operand_immediate_init(immediate_operand_floating_init(
					lit->value.decimal_value), typ_primitive_init(primitive_kind::DOUBLE));
			default:
				util::ice("ir_exp_primary",
					"Unrecognized suffix_kind while generating IR for a decimal!");
			}
		}
			break;
		default:
			util::ice("ir_exp_primary",
				"Unrecognized literal while generating IR!");
		}
	}
		break;
	case ast::primary_kind::QUALIFIED_IDENTIFIER: {
		type ast::qualified_identifier* qi = p->which.qualified_identifier;

		type operand* reg_lookup = NULL as type operand*;
		if (util::ht_get(ctx->local_name_2_reg, qi->symtab_value as byte*, reg_lookup$ as byte**)) {
			if (lvalue)
				return operand_copy(reg_lookup);
			else
				return ir_load(ctx, qi->symtab_value->typ, 0, reg_lookup);
		}

		// TODO

		util::ice("ir_exp_primary",
			"Could not resolve qualified identifier to a location!");
	}
		break;
	case ast::primary_kind::TUPLE: {
		type util::vector* elems = p->which.tuple;

		n_size_data(p->typ);

		type operand* dst = operand_register_init(register_operand_init(ctx->counter++),
			ast_2_ir_typ(ctx, p->typ));

		type insn* alloc = insn_binary_init(insn_op_kind::ALLOCA,
			dst,
			operand_immediate_init(immediate_operand_integral_init(p->typ->size_data->alignment),
				typ_primitive_init(primitive_kind::WORD)),
			operand_immediate_init(immediate_operand_integral_init(p->typ->size_data->size),
				typ_primitive_init(primitive_kind::WORD)));
		util::vector_append(ctx->pre_insns, alloc$ as byte*);

		unsigned int offset = 0;
		for (unsigned int i = 0; i < util::vector_size(elems); i++) {
			type ast::exp* curr_e = util::vector_at(elems, i)
				as type ast::exp** @;

			type operand* oce = ir_exp(ctx, curr_e, false);
			if (oce == NULL as type operand*)
				return NULL as type operand*;

			n_size_data(curr_e->typ);

			if (offset % curr_e->typ->size_data->alignment != 0)
				offset += curr_e->typ->size_data->alignment - (offset % curr_e->typ->size_data->alignment);

			ir_store(ctx, curr_e->typ, oce, offset, operand_copy(dst));

			offset += curr_e->typ->size_data->size;
		}

		return operand_copy(dst);
	}
		break;
	case ast::primary_kind::PARENTHESIZED:
		return ir_exp(ctx, p->which.parenthesized, lvalue);
	// TODO
	default:
		util::ice("ir_exp_primary",
			"Unrecognized ast::primary_kind while generating a primary expression's IR!");
	}

	util::ice("ir_exp_primary",
		"This should be unreachable!");
}

} } // namespace neutrino::ir
