import "ir/ir.hsp"

import <"std/io">
import <"std/lib">

import "ir/operand.hsp"
import "ir/insn.hsp"
import "ast/ast.hsp"
import "util/error.hsp"
import "ir/util.hsp"
import "tck/util.hsp"
import "tck/symtab.hsp"

using std::io::printf;
using std::lib::NULL;

namespace neutrino { namespace ir {

func bool ir_pat_nc_assign(type ir_ctx* ctx, type ast::pat* p,
	type ast::typ* from, type operand* o) {
	switch (p->kind) {
	case ast::pat_kind::WILDCARD:
		return true;
	case ast::pat_kind::IDENT: {
		type ast::ident_pat* ip = p->which.ident_pat;

		util::maybe_ice(ip->symtab_value->kind
			!= tck::symtab_value_ast_node_kind::VARIANT_CONSTRUCTOR,
			"ir_pat_nc_assign",
			"Cannot assign to a nullary variant constructor!");

		type operand* final_o = NULL as type operand*;
		if (o != NULL as type operand*) {
			util::maybe_ice(from != NULL as type ast::typ*,
				"ir_pat_nc_assign",
				"Expected a non-NULL assignment type here!");

			final_o = ir_cast(ctx, o, from,
				ip->symtab_value->typ);
		}
		else
			final_o = NULL as type operand*;

		n_size_data(ip->symtab_value->typ);

		type operand* dst = NULL as type operand*;

		if (tck::is_pointer(ip->symtab_value->typ)
			|| ip->symtab_value->typ->kind == ast::typ_kind::FN_TYP) {
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::WORD));
		}
		else {
			switch (ip->symtab_value->typ->kind) {
			case ast::typ_kind::PRIMITIVE: {
				dst = operand_register_init(register_operand_init(ctx->counter++),
					typ_primitive_init(primitive_kind::WORD));
			}
				break;
			case ast::typ_kind::TUP: {
				dst = operand_register_init(register_operand_init(ctx->counter++),
					ast_2_ir_typ(ctx, ip->symtab_value->typ));
			}
				break;
			// TODO
			default:
				util::ice("ir_pat_nc_assign",
					"Unrecognized typ_kind while assigning to a pattern!");
			}
		}

		util::maybe_ice(dst != NULL as type operand*,
			"ir_pat_nc_assign",
			"Expected dst to be non-NULL at this point!");

		type insn* alloc = insn_binary_init(
			insn_op_kind::ALLOCA,
			dst,
			operand_immediate_init(
				immediate_operand_integral_init(ip->symtab_value->typ->size_data->alignment),
				typ_primitive_init(primitive_kind::WORD)),
			operand_immediate_init(
				immediate_operand_integral_init(ip->symtab_value->typ->size_data->size),
				typ_primitive_init(primitive_kind::WORD)));

		util::vector_append(ctx->pre_insns, alloc$ as byte*);

		if (o != NULL as type operand*)
			ir_store(ctx, ip->symtab_value->typ, final_o, 0, dst);

		util::ht_set(ctx->local_name_2_reg, ip->symtab_value as byte*,
			dst as byte*);

		// TODO

		return true;
	}
		break;
	case ast::pat_kind::NESTED:
		return ir_pat_nc_assign(ctx, p->which.nested, from, o);
	case ast::pat_kind::TUPLE: {
		type ast::tuple_pat* tp = p->which.tuple_pat;

		util::maybe_ice(from->kind == ast::typ_kind::TUP
			&& !tck::is_pointer(from),
			"ir_pat_nc_assign",
			"Expected a tuple type to assign to a tuple pattern here!");

		type util::vector* elem_typs = from->which.tup_typ;

		n_size_data(from);

		switch (tp->ignore_kind) {
		case ast::tuple_ignore_kind::NONE: {
			util::maybe_ice(tp->first != NULL as type util::vector*
				&& util::vector_size(tp->first) == util::vector_size(elem_typs),
				"ir_pat_nc_assign",
				"For a tuple pattern with no elided elements, expected an equal number of elements between a pattern and the resulting type!");

			unsigned int offset = 0;
			for (unsigned int i = 0; i < util::vector_size(tp->first); i++) {
				type ast::pat* cp = util::vector_at(tp->first, i)
					as type ast::pat** @;

				type ast::typ* cfrom = util::vector_at(elem_typs, i)
					as type ast::typ** @;

				if (offset % cfrom->size_data->alignment != 0)
					offset += cfrom->size_data->alignment - (offset % cfrom->size_data->alignment);

				type operand* co = ir_load(ctx, cfrom, offset, o);
				if (co == NULL as type operand*)
					return false;

				if (!ir_pat_nc_assign(ctx, cp, cfrom, co))
					return false;

				offset += cfrom->size_data->size;
			}
		}
			break;
		case ast::tuple_ignore_kind::BEGINNING: {
			util::maybe_ice(tp->first != NULL as type util::vector*
				&& util::vector_size(tp->first) < util::vector_size(elem_typs),
				"ir_pat_nc_assign",
				"For a tuple pattern with elements elided in the beginning, expected a smaller number of elements between a pattern and the resulting type!");

			unsigned int offset = 0;
			for (unsigned int i = 0;
				i < util::vector_size(elem_typs) - util::vector_size(tp->first);
				i++) {
				type ast::typ* ct = util::vector_at(elem_typs, i)
					as type ast::typ** @;

				if (offset % ct->size_data->alignment != 0)
					offset += ct->size_data->alignment - (offset % ct->size_data->alignment);

				offset += ct->size_data->size;
			}

			for (unsigned int i = 0; i < util::vector_size(tp->first); i++) {
				type ast::pat* cp = util::vector_at(tp->first, i)
					as type ast::pat** @;

				type ast::typ* cfrom = util::vector_at(elem_typs,
					(util::vector_size(elem_typs) - util::vector_size(tp->first)) + i)
						as type ast::typ** @;

				if (offset % cfrom->size_data->alignment != 0)
					offset += cfrom->size_data->alignment - (offset % cfrom->size_data->alignment);

				type operand* co = ir_load(ctx, cfrom, offset, o);
				if (co == NULL as type operand*)
					return false;

				if (!ir_pat_nc_assign(ctx, cp, cfrom, co))
					return false;

				offset += cfrom->size_data->size;
			}
		}
			break;
		case ast::tuple_ignore_kind::END: {
			util::maybe_ice(tp->first != NULL as type util::vector*
				&& util::vector_size(tp->first) < util::vector_size(elem_typs),
				"ir_pat_nc_assign",
				"For a tuple pattern with elements elided at the end, expected a pattern with a smaller number of elements than the resulting type!");

			unsigned int offset = 0;
			for (unsigned int i = 0; i < util::vector_size(tp->first); i++) {
				type ast::typ* cfrom = util::vector_at(elem_typs, i)
					as type ast::typ** @;

				type ast::pat* cp = util::vector_at(tp->first, i)
					as type ast::pat** @;

				if (offset % cfrom->size_data->alignment != 0)
					offset += cfrom->size_data->alignment - (offset % cfrom->size_data->alignment);

				type operand* co = ir_load(ctx, cfrom, offset, o);
				if (co == NULL as type operand*)
					return false;

				if (!ir_pat_nc_assign(ctx, cp, cfrom, co))
					return false;

				offset += cfrom->size_data->size;
			}
		}
			break;
		case ast::tuple_ignore_kind::MIDDLE: {
			util::maybe_ice(tp->first != NULL as type util::vector*
				&& tp->last != NULL as type util::vector*
				&& util::vector_size(tp->first) + util::vector_size(tp->last)
					< util::vector_size(elem_typs),
				"ir_nc_pat_assign",
				"For a tuple pattern with elements elided in the middle, expected a pattern with a smaller number of elements than the resulting type!");

			unsigned int offset = 0;
			for (unsigned int i = 0; i < util::vector_size(tp->first); i++) {
				type ast::pat* cp = util::vector_at(tp->first, i)
					as type ast::pat** @;

				type ast::typ* cfrom = util::vector_at(elem_typs, i)
					as type ast::typ** @;

				if (offset % cfrom->size_data->alignment != 0)
					offset += cfrom->size_data->alignment - (offset % cfrom->size_data->alignment);

				type operand* co = ir_load(ctx, cfrom, offset, o);
				if (co == NULL as type operand*)
					return false;

				if (!ir_pat_nc_assign(ctx, cp, cfrom, co))
					return false;

				offset += cfrom->size_data->size;
			}

			for (unsigned int i = util::vector_size(tp->first);
				i < util::vector_size(elem_typs) - util::vector_size(tp->last);
				i++) {
				type ast::typ* cfrom = util::vector_at(elem_typs, i)
					as type ast::typ** @;

				if (offset % cfrom->size_data->alignment != 0)
					offset += cfrom->size_data->alignment - (offset % cfrom->size_data->alignment);

				offset += cfrom->size_data->size;
			}

			for (unsigned int i = 0; i < util::vector_size(tp->last); i++) {
				type ast::pat* cp = util::vector_at(tp->last, i)
					as type ast::pat** @;

				type ast::typ* cfrom = util::vector_at(elem_typs,
					(util::vector_size(elem_typs) - util::vector_size(tp->last)) + i)
						as type ast::typ** @;

				if (offset % cfrom->size_data->alignment != 0)
					offset += cfrom->size_data->alignment - (offset % cfrom->size_data->alignment);

				type operand* co = ir_load(ctx, cfrom, offset, o);
				if (co == NULL as type operand*)
					return false;

				if (!ir_pat_nc_assign(ctx, cp, cfrom, co))
					return false;

				offset += cfrom->size_data->size;
			}
		}
			break;
		default:
			util::ice("ir_pat_nc_assign",
				"Unrecognized tuple_ignore_kind while generating IR!");
		}

		return true;
	}
		break;
	// TODO
	default:
		util::ice("ir_pat_nc_assign",	
			"Unrecognized ast::pat_kind while generating IR!");
	}

	util::ice("ir_pat_nc_assign",
		"This should be unreachable!");
}

} } // namespace neutrino::ir
