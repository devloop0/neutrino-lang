import "ir/util.hsp"

import <"std/lib">
import <"std/io">

import "ir/typ.hsp"
import "ir/operand.hsp"
import "ir/insn.hsp"
import "tck/symtab.hsp"
import "util/base.hsp"
import "util/string.hsp"
import "util/error.hsp"
import "tck/util.hsp"
import "ast/ast.hsp"
import "util/lib.hsp"

using std::io::printf;
using std::lib::NULL;
using neutrino::util::n_strdup;
using neutrino::util::n_malloc;
using neutrino::util::n_free;

namespace neutrino { namespace ir {

func void deref_aggregate_member_type_free(byte* b) {
	type aggregate_member_type* amt = b as type aggregate_member_type** @;
	aggregate_member_type_delete(amt);
}

func void deref_operand_free(byte* b) {
	type operand* o = b as type operand** @;
	operand_delete(o);
}

func void deref_static_data_member_free(byte* b) {
	type static_data_member* sdm = b as type static_data_member** @;
	static_data_member_delete(sdm);
}

func void deref_function_parameter_free(byte* b) {
	type function_parameter* fp = b as type function_parameter** @;
	function_parameter_delete(fp);
}

func void deref_insn_free(byte* b) {
	type insn* i = b as type insn** @;
	insn_delete(i);
}

func void deref_top_level_free(byte* b) {
	type top_level* tl = b as type top_level** @;
	top_level_delete(tl);
}

func void deref_typ_free(byte* b) {
	type typ* t = b as type typ** @;
	typ_delete(t);
}

func void typ_free(byte* b) {
	type typ* t = b as type typ*;
	typ_delete(t);
}

func void top_level_free(byte* b) {
	type top_level* tl = b as type top_level*;
	top_level_delete(tl);
}

func bool is_global(type tck::symtab_value* sv) {
	type tck::symtab* iter = sv->enclosing;

	while (iter != NULL as type tck::symtab*) {
		if (iter->kind != tck::symtab_kind::NAMESPACE
			&& iter->kind != tck::symtab_kind::GLOBAL) {
			return false;
		}

		iter = iter->parent;
	}

	return true;
}

func char* mangle_name(type tck::symtab_value* sv) {
	util::maybe_ice(is_global(sv),
		"mangle_name",
		"Cannot mangle a non-global symbol!");

	type tck::symtab* iter = sv->enclosing;
	type util::string* ret = util::string_init(sv->name->text);
	while (iter->kind != tck::symtab_kind::GLOBAL) {
		util::string_ccat("_", ret);
		util::string_ccat(iter->name->text, ret);

		iter = iter->parent;
	}

	char* tmp = n_strdup(util::string_data(ret));
	util::string_delete(ret);
	return tmp;
}

func type size_data* size_data_init(unsigned int a, unsigned int s) {
	type size_data* sd = n_malloc(sizeof{type size_data})
		as type size_data*;
	sd->alignment = a;
	sd->size = s;
	return sd;
}

func void size_data_delete(type size_data* sd) {
	n_free(sd as byte*);
}

func void n_size_data(type ast::typ* t) {
	if (t->size_data != NULL as type size_data*)
		return;

	if (tck::is_pointer(t)) {
		t->size_data = size_data_init(4, 4);
		return;
	}

	switch (t->kind) {
	case ast::typ_kind::PRIMITIVE: {
		unsigned int pk = t->which.primitive;
		switch (pk) {
		case ast::primitive_kind::VOID:
			t->size_data = size_data_init(1, 1);
			return;
		case ast::primitive_kind::SIGNED_BYTE:
		case ast::primitive_kind::UNSIGNED_BYTE:
		case ast::primitive_kind::SIGNED_CHAR:
		case ast::primitive_kind::UNSIGNED_CHAR:
		case ast::primitive_kind::BOOL:
			t->size_data = size_data_init(1, 1);
			return;
		case ast::primitive_kind::SIGNED_SHORT:
		case ast::primitive_kind::UNSIGNED_SHORT:
			t->size_data = size_data_init(2, 2);
			return;
		case ast::primitive_kind::SIGNED_INT:
		case ast::primitive_kind::UNSIGNED_INT:
			t->size_data = size_data_init(4, 4);
			return;
		case ast::primitive_kind::SIGNED_LONG:
		case ast::primitive_kind::UNSIGNED_LONG:
			t->size_data = size_data_init(4, 4);
			return;
		case ast::primitive_kind::FLOAT:
			t->size_data = size_data_init(4, 4);
			return;
		case ast::primitive_kind::DOUBLE:
			t->size_data = size_data_init(8, 8);
			return;
		default:
			util::ice("n_size_data",
				"Unrecognied ast::primitive_kind while calculating size!");
		}
	}
		break;
	case ast::typ_kind::FN_TYP: {
		t->size_data = size_data_init(4, 4);
		return;
	}
		break;
	case ast::typ_kind::TUP: {
		type util::vector* tt = t->which.tup_typ;

		if (util::vector_empty(tt))
			t->size_data = size_data_init(1, 1);
		else {
			unsigned int size = 0, alignment = 1;

			for (unsigned int i = 0; i < util::vector_size(tt); i++) {
				type ast::typ* ct = util::vector_at(tt, i)
					as type ast::typ** @;

				n_size_data(ct);

				if (size % ct->size_data->alignment != 0)
					size += ct->size_data->alignment - (size % ct->size_data->alignment);

				size += ct->size_data->size;

				alignment = alignment > ct->size_data->alignment
					? alignment
					: ct->size_data->alignment;
			}

			if (size % alignment != 0)
				size += alignment - (size % alignment);
			t->size_data = size_data_init(alignment, size);
		}
		return;
	}
		break;
	// TODO
	default:
		util::ice("n_size_data",
			"Unrecognized ast::typ_kind while calculating size!");
	}

	util::ice("n_size_data",
		"This should be unreachable!");
}

func[static] type typ* flatten_typs(type ir_ctx* ctx, type util::vector* typs) {
	type util::vector* amts = util::vector_init(sizeof{type aggregate_member_type*},
		deref_aggregate_member_type_free);

	unsigned int overall_alignment = 0;
	if (util::vector_empty(typs))
		overall_alignment = 1;
	else {
		for (unsigned int i = 0; i < util::vector_size(typs); i++) {
			type typ* ct = util::vector_at(typs, i)
				as type typ** @;

			switch (ct->kind) {
			case typ_kind::PRIMITIVE: {
				unsigned int a = 0x0;

				switch (ct->which.primitive) {
				case primitive_kind::BYTE:
					a = 1;
					break;
				case primitive_kind::HALF:
					a = 2;
					break;
				case primitive_kind::WORD:
					a = 4;
					break;
				case primitive_kind::LONG:
					a = 4;
					break;
				case primitive_kind::SINGLE:
					a = 4;
					break;
				case primitive_kind::DOUBLE:
					a = 8;
					break;
				default:
					util::ice("flatten_typs",
						"Unrecognized primitive member type while trying to flatten!");
				}

				util::maybe_ice(a != 0x0,
					"flatten_typs",
					"Expected alignment 'a' to be non-NULL at this point!");

				type aggregate_member_type* amt = aggregate_member_type_init(
					a, typ_copy(ct), 1);
				util::vector_append(amts, amt$ as byte*);

				if (overall_alignment < a)
					overall_alignment = a;
			}
				break;
			case typ_kind::AGGREGATE_NAME: {
				char* an = ct->which.aggregate_name;

				type top_level* tl;
				util::maybe_ice(util::ht_get(ctx->name_2_aggregate, an as byte*, tl$ as byte**),
					"flatten_typs",
					"Expected a valid aggregate name here!");

				util::maybe_ice(tl->kind == top_level_kind::AGGREGATE_TYPE,
					"flatten_typs",
					"Expected an aggregate top level type here!");

				type aggregate_type* at = tl->which.aggregate_type;
				type aggregate_member_type* amt = aggregate_member_type_init(
					at->alignment, typ_copy(ct), 1);
				util::vector_append(amts, amt$ as byte*);

				if (overall_alignment < amt->alignment)
					overall_alignment = amt->alignment;
			}
				break;
			// TODO
			default:
				util::ice("flatten_typs",
					"Unrecognized member type while trying to flatten!");
			}
		}
	}

	type util::string* tmp = util::string_init("__tuple_");
	util::utoustr(ctx->counter++, tmp);
	char* name = n_strdup(util::string_data(tmp));
	util::string_delete(tmp);

	type aggregate_type* at = aggregate_type_init(name, overall_alignment,
		amts);
	type top_level* tl = top_level_aggregate_type_init(at);
	util::ht_set(ctx->name_2_aggregate, name as byte*, tl as byte*);
	return typ_aggregate_name_init(n_strdup(name));
}

func type typ* ast_2_ir_typ(type ir_ctx* ctx, type ast::typ* t) {
	if (tck::is_pointer(t))
		return typ_primitive_init(primitive_kind::WORD);
	
	switch (t->kind) {
	case ast::typ_kind::PRIMITIVE: {
		switch (t->which.primitive) {
		case ast::primitive_kind::VOID:
			return typ_primitive_init(primitive_kind::BYTE);
		case ast::primitive_kind::SIGNED_BYTE:
		case ast::primitive_kind::UNSIGNED_BYTE:
		case ast::primitive_kind::UNSIGNED_CHAR:
		case ast::primitive_kind::SIGNED_CHAR:
		case ast::primitive_kind::BOOL:
			return typ_primitive_init(primitive_kind::BYTE);
		case ast::primitive_kind::SIGNED_SHORT:
		case ast::primitive_kind::UNSIGNED_SHORT:
			return typ_primitive_init(primitive_kind::HALF);
		case ast::primitive_kind::SIGNED_INT:
		case ast::primitive_kind::UNSIGNED_INT:
			return typ_primitive_init(primitive_kind::WORD);
		case ast::primitive_kind::UNSIGNED_LONG:
		case ast::primitive_kind::SIGNED_LONG:
			return typ_primitive_init(primitive_kind::LONG);
		case ast::primitive_kind::FLOAT:
			return typ_primitive_init(primitive_kind::SINGLE);
		case ast::primitive_kind::DOUBLE:
			return typ_primitive_init(primitive_kind::DOUBLE);
		default:
			util::ice("ast_2_ir_typ",
				"Unrecognized ast::primitive_kind while converting an AST type to an IR type!");
		}
	}
		break;
	case ast::typ_kind::TUP: {
		type util::vector* typs = util::vector_init(sizeof{type typ*},
			deref_typ_free);
		
		for (unsigned int i = 0; i < util::vector_size(t->which.tup_typ); i++) {
			type ast::typ* cat = util::vector_at(t->which.tup_typ, i)
				as type ast::typ** @;

			type typ* ct = ast_2_ir_typ(ctx, cat);

			util::vector_append(typs, ct$ as byte*);
		}

		type typ* flattened = flatten_typs(ctx, typs);
		util::vector_delete(typs);
		return flattened;
	}
		break;
	case ast::typ_kind::FN_TYP:
		return typ_primitive_init(primitive_kind::WORD);
	// TODO
	default:
		util::ice("ast_2_ir_typ",
			"Unrecognized ast::typ_kind while converting an AST type to an IR type!");
	}

	util::ice("ast_2_ir_typ",
		"This should be unreachable!");
}

func type operand* ir_cast(type ir_ctx* ctx, type operand* src,
	type ast::typ* from, type ast::typ* to) {
	bool from_isp = tck::is_pointer(from),
		to_isp = tck::is_pointer(to);

	if ((from->kind != ast::typ_kind::PRIMITIVE
		&& to->kind != ast::typ_kind::PRIMITIVE)
		|| (from_isp && to_isp)) {
		type operand* ret = operand_copy(src);
		typ_delete(ret->typ);
		ret->typ = ast_2_ir_typ(ctx, to);
		return ret;
	}

	type operand* ret = NULL as type operand*;
	if (from_isp || to_isp) {
		type ast::typ* pointer_typ = ast::typ_primitive_init(
			tck::mut_non_pointer_typ_qualifiers_init(),
			ast::primitive_kind::UNSIGNED_INT, 
			NULL as type ast::metadata*);

		if (from_isp) {
			type operand* ret = ir_cast(ctx, src, pointer_typ, to);
			ast::typ_delete(pointer_typ);
			return ret;
		}

		type operand* ret = ir_cast(ctx, src, from, pointer_typ);
		ast::typ_delete(pointer_typ);
		return ret;
	}

	util::maybe_ice(!from_isp && !to_isp
		&& from->kind == ast::typ_kind::PRIMITIVE
		&& to->kind == ast::typ_kind::PRIMITIVE,
		"ir_cast",
		"Expected a non-pointer, primitive-to-primitive cast here!");

	unsigned int fromp = from->which.primitive,
		top = to->which.primitive;

	type operand* dst = NULL as type operand*;
	unsigned int ik = 0x0;

	switch (fromp) {
	case ast::primitive_kind::UNSIGNED_INT:
	case ast::primitive_kind::SIGNED_INT: {
		bool is_signed = fromp == ast::primitive_kind::SIGNED_INT;
		switch (top) {
		case ast::primitive_kind::BOOL:
			// TODO
			break;
		case ast::primitive_kind::SIGNED_BYTE:
		case ast::primitive_kind::UNSIGNED_BYTE:
		case ast::primitive_kind::SIGNED_CHAR:
		case ast::primitive_kind::UNSIGNED_CHAR:
		case ast::primitive_kind::VOID:
			ik = insn_op_kind::TRUNCWB;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::BYTE));
			break;
		case ast::primitive_kind::SIGNED_SHORT:
		case ast::primitive_kind::UNSIGNED_SHORT:
			ik = insn_op_kind::TRUNCWH;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::HALF));
			break;
		case ast::primitive_kind::SIGNED_INT:
		case ast::primitive_kind::UNSIGNED_INT:
			return operand_copy(src);
		case ast::primitive_kind::SIGNED_LONG:
		case ast::primitive_kind::UNSIGNED_LONG: {
			if (is_signed)
				ik = insn_op_kind::EXTSWL;
			else
				ik = insn_op_kind::EXTUWL;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::LONG));
		}
			break;
		case ast::primitive_kind::FLOAT: {
			if (is_signed)
				ik = insn_op_kind::CVTSWS;
			else
				ik = insn_op_kind::CVTUWS;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::SINGLE));
		}
			break;
		case ast::primitive_kind::DOUBLE: {
			if (is_signed)
				ik = insn_op_kind::CVTSWD;
			else
				ik = insn_op_kind::CVTUWD;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::DOUBLE));
		}
			break;
		}
	}
		break;
	case ast::primitive_kind::SIGNED_SHORT:
	case ast::primitive_kind::UNSIGNED_SHORT: {
		bool is_signed = fromp == ast::primitive_kind::SIGNED_SHORT;

		switch (top) {
		case ast::primitive_kind::BOOL:
			// TODO
			break;
		case ast::primitive_kind::VOID:
		case ast::primitive_kind::SIGNED_CHAR:
		case ast::primitive_kind::UNSIGNED_CHAR:
		case ast::primitive_kind::SIGNED_BYTE:
		case ast::primitive_kind::UNSIGNED_BYTE: {
			ik = insn_op_kind::TRUNCHB;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::BYTE));
		}
			break;
		case ast::primitive_kind::SIGNED_SHORT:
		case ast::primitive_kind::UNSIGNED_SHORT:
			return operand_copy(src);
		case ast::primitive_kind::SIGNED_INT:
		case ast::primitive_kind::UNSIGNED_INT: {
			if (is_signed)
				ik = insn_op_kind::EXTSHW;
			else
				ik = insn_op_kind::EXTUHW;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::WORD));
		}
			break;
		case ast::primitive_kind::SIGNED_LONG:
		case ast::primitive_kind::UNSIGNED_LONG: {
			if (is_signed)
				ik = insn_op_kind::EXTSHL;
			else
				ik = insn_op_kind::EXTUHL;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::LONG));
		}
			break;
		case ast::primitive_kind::FLOAT: {
			if (is_signed)
				ik = insn_op_kind::CVTSHS;
			else
				ik = insn_op_kind::CVTUHS;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::SINGLE));
		}
			break;
		case ast::primitive_kind::DOUBLE: {
			if (is_signed)
				ik = insn_op_kind::CVTSHD;
			else
				ik = insn_op_kind::CVTUHD;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::DOUBLE));
		}
			break;
		}
	}
		break;
	case ast::primitive_kind::VOID:
	case ast::primitive_kind::BOOL:
	case ast::primitive_kind::SIGNED_BYTE:
	case ast::primitive_kind::UNSIGNED_BYTE:
	case ast::primitive_kind::SIGNED_CHAR:
	case ast::primitive_kind::UNSIGNED_CHAR: {
		bool is_signed = fromp == ast::primitive_kind::SIGNED_BYTE
			|| fromp == ast::primitive_kind::UNSIGNED_BYTE;
		switch (top) {
		case ast::primitive_kind::BOOL:
			// TODO
			break;
		case ast::primitive_kind::SIGNED_CHAR:
		case ast::primitive_kind::UNSIGNED_CHAR:
		case ast::primitive_kind::SIGNED_BYTE:
		case ast::primitive_kind::UNSIGNED_BYTE:
		case ast::primitive_kind::VOID:
			return operand_copy(src);
		case ast::primitive_kind::SIGNED_SHORT:
		case ast::primitive_kind::UNSIGNED_SHORT: {
			if (is_signed)
				ik = insn_op_kind::EXTSBH;
			else
				ik = insn_op_kind::EXTUBH;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::HALF));
		}
			break;
		case ast::primitive_kind::SIGNED_INT:
		case ast::primitive_kind::UNSIGNED_INT: {
			if (is_signed)
				ik = insn_op_kind::EXTSBW;
			else
				ik = insn_op_kind::EXTUBW;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::WORD));
		}
			break;
		case ast::primitive_kind::SIGNED_LONG:
		case ast::primitive_kind::UNSIGNED_LONG: {
			if (is_signed)
				ik = insn_op_kind::EXTSBL;
			else
				ik = insn_op_kind::EXTUBL;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::LONG));
		}
			break;
		case ast::primitive_kind::FLOAT: {
			if (is_signed)
				ik = insn_op_kind::CVTSBS;
			else
				ik = insn_op_kind::CVTUBS;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::SINGLE));
		}
			break;
		case ast::primitive_kind::DOUBLE: {
			if (is_signed)
				ik = insn_op_kind::CVTSBD;
			else
				ik = insn_op_kind::CVTUBD;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::DOUBLE));
		}
			break;
		}
	}
		break;
	case ast::primitive_kind::SIGNED_LONG:
	case ast::primitive_kind::UNSIGNED_LONG: {
		bool is_signed = fromp == ast::primitive_kind::SIGNED_LONG;

		switch (top) {
		case ast::primitive_kind::BOOL:
			// TODO
			break;
		case ast::primitive_kind::VOID:
		case ast::primitive_kind::SIGNED_BYTE:
		case ast::primitive_kind::UNSIGNED_BYTE:
		case ast::primitive_kind::SIGNED_CHAR:
		case ast::primitive_kind::UNSIGNED_CHAR: {
			ik = insn_op_kind::TRUNCLB;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::BYTE));
		}
			break;
		case ast::primitive_kind::SIGNED_SHORT:
		case ast::primitive_kind::UNSIGNED_SHORT: {
			ik = insn_op_kind::TRUNCLH;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::HALF));
		}
			break;
		case ast::primitive_kind::SIGNED_INT:
		case ast::primitive_kind::UNSIGNED_INT: {
			ik = insn_op_kind::TRUNCLW;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::WORD));
		}
			break;
		case ast::primitive_kind::SIGNED_LONG:
		case ast::primitive_kind::UNSIGNED_LONG:
			return operand_copy(src);
		case ast::primitive_kind::FLOAT: {
			if (is_signed)
				ik = insn_op_kind::CVTSLS;
			else
				ik = insn_op_kind::CVTULS;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::SINGLE));
		}
			break;
		case ast::primitive_kind::DOUBLE: {
			if (is_signed)
				ik = insn_op_kind::CVTSLD;
			else
				ik = insn_op_kind::CVTULD;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::DOUBLE));
		}
			break;
		}
	}
		break;
	case ast::primitive_kind::DOUBLE:
	case ast::primitive_kind::FLOAT: {
		bool is_float = fromp == ast::primitive_kind::FLOAT;

		switch (top) {
		case ast::primitive_kind::BOOL:
			// TODO
			break;
		case ast::primitive_kind::SIGNED_CHAR:
		case ast::primitive_kind::SIGNED_BYTE: {
			if (is_float)
				ik = insn_op_kind::CVTSSB;
			else
				ik = insn_op_kind::CVTDSB;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::BYTE));
		}
			break;
		case ast::primitive_kind::UNSIGNED_BYTE:
		case ast::primitive_kind::UNSIGNED_CHAR:
		case ast::primitive_kind::VOID: {
			if (is_float)
				ik = insn_op_kind::CVTSUB;
			else
				ik = insn_op_kind::CVTDUB;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::BYTE));
		}
			break;
		case ast::primitive_kind::SIGNED_SHORT: {
			if (is_float)
				ik = insn_op_kind::CVTSSH;
			else
				ik = insn_op_kind::CVTDSH;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::HALF));
		}
			break;
		case ast::primitive_kind::UNSIGNED_SHORT: {
			if (is_float)
				ik = insn_op_kind::CVTSUH;
			else
				ik = insn_op_kind::CVTDUH;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::HALF));
		}
			break;
		case ast::primitive_kind::SIGNED_INT: {
			if (is_float)
				ik = insn_op_kind::CVTSSW;
			else
				ik = insn_op_kind::CVTDSW;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::WORD));
		}
			break;
		case ast::primitive_kind::UNSIGNED_INT: {
			if (is_float)
				ik = insn_op_kind::CVTSUW;
			else
				ik = insn_op_kind::CVTDUW;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::WORD));
		}
			break;
		case ast::primitive_kind::SIGNED_LONG: {
			if (is_float)
				ik = insn_op_kind::CVTSSL;
			else
				ik = insn_op_kind::CVTDSL;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::LONG));
		}
			break;
		case ast::primitive_kind::UNSIGNED_LONG: {
			if (is_float)
				ik = insn_op_kind::CVTSUL;
			else
				ik = insn_op_kind::CVTDUL;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::LONG));
		}
			break;
		case ast::primitive_kind::FLOAT: {
			if (is_float)
				return operand_copy(src);
			ik = insn_op_kind::CVTDS;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::SINGLE));
		}
			break;
		case ast::primitive_kind::DOUBLE: {
			if (!is_float)
				return operand_copy(src);
			ik = insn_op_kind::CVTSD;
			dst = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::DOUBLE));
		}
			break;
		}
	}
		break;
	}

	util::maybe_ice(ik != 0x0 && dst != NULL as type operand*,
		"ir_cast",
		"Invalid from-primitive/to-primitive pair!");

	type insn* cast_insn = insn_unary_init(ik, dst, operand_copy(src));
	util::vector_append(ctx->insns, cast_insn$ as byte*);

	return operand_copy(dst);
}

func type operand* ir_load(type ir_ctx* ctx, type ast::typ* final_typ,
	unsigned int off, type operand* loc) {
	if (tck::is_pointer(final_typ)
		|| final_typ->kind == ast::typ_kind::FN_TYP) {
		type operand* dst = operand_register_init(register_operand_init(ctx->counter++),
			typ_primitive_init(primitive_kind::WORD));

		type insn* i = insn_binary_init(
			insn_op_kind::LOADW,
			dst,
			operand_immediate_init(immediate_operand_integral_init(off),
				typ_primitive_init(primitive_kind::WORD)),
			operand_copy(loc));
		util::vector_append(ctx->insns, i$ as byte*);

		return operand_copy(dst);
	}

	switch (final_typ->kind) {
	case ast::typ_kind::PRIMITIVE: {
		unsigned int ik = 0x0, pk = 0x0;

		switch (final_typ->which.primitive) {
		case ast::primitive_kind::VOID:
		case ast::primitive_kind::BOOL:
		case ast::primitive_kind::SIGNED_CHAR:
		case ast::primitive_kind::UNSIGNED_CHAR:
		case ast::primitive_kind::SIGNED_BYTE:
		case ast::primitive_kind::UNSIGNED_BYTE:
			ik = insn_op_kind::LOADB;
			pk = primitive_kind::BYTE;
			break;
		case ast::primitive_kind::SIGNED_SHORT:
		case ast::primitive_kind::UNSIGNED_SHORT:
			ik = insn_op_kind::LOADH;
			pk = primitive_kind::HALF;
			break;
		case ast::primitive_kind::SIGNED_INT:
		case ast::primitive_kind::UNSIGNED_INT:
			ik = insn_op_kind::LOADW;
			pk = primitive_kind::WORD;
			break;
		case ast::primitive_kind::SIGNED_LONG:
		case ast::primitive_kind::UNSIGNED_LONG:
			ik = insn_op_kind::LOADL;
			pk = primitive_kind::LONG;
			break;
		case ast::primitive_kind::FLOAT:
			ik = insn_op_kind::LOADS;
			pk = primitive_kind::SINGLE;
			break;
		case ast::primitive_kind::DOUBLE:
			ik = insn_op_kind::LOADD;
			pk = primitive_kind::DOUBLE;
			break;
		}

		util::maybe_ice(pk != 0x0 && ik != 0x0,
			"ir_load",
			"Expected a valid primitive_kind and insn_op_kind for a primitive load!");

		type operand* dst = operand_register_init(register_operand_init(ctx->counter++),
			typ_primitive_init(pk));

		type insn* i = insn_binary_init(
			ik,
			dst,
			operand_immediate_init(immediate_operand_integral_init(off),
				typ_primitive_init(primitive_kind::WORD)),
			operand_copy(loc));

		util::vector_append(ctx->insns, i$ as byte*);
		return operand_copy(dst);
	}
		break;
	case ast::typ_kind::TUP: {
		type operand* ret = operand_register_init(register_operand_init(ctx->counter++),
			typ_primitive_init(primitive_kind::WORD));

		type insn* move = insn_binary_init(insn_op_kind::ADDW,
			operand_copy(ret),
			operand_copy(loc),
			operand_immediate_init(immediate_operand_integral_init(off),
				typ_primitive_init(primitive_kind::WORD)));
		util::vector_append(ctx->insns, move$ as byte*);

		return ret;
	}
	// TODO
	default:
		util::ice("ir_load",
			"Unrecognized typ_kind while trying to load from a location!");
	}

	util::ice("ir_load",
		"This should be unreachable!");
}

func void ir_store(type ir_ctx* ctx, type ast::typ* final_typ,
	type operand* src, unsigned int off, type operand* loc) {
	if (tck::is_pointer(final_typ)
		|| final_typ->kind == ast::typ_kind::FN_TYP) {
		type insn* i = insn_binary_init(
			insn_op_kind::STOREW,
			src,
			operand_immediate_init(immediate_operand_integral_init(off),
				typ_primitive_init(primitive_kind::WORD)),
			operand_copy(loc));
		return;
	}

	switch (final_typ->kind) {
	case ast::typ_kind::PRIMITIVE: {
		unsigned int ik = 0x0;

		switch (final_typ->which.primitive) {
		case ast::primitive_kind::VOID:
		case ast::primitive_kind::BOOL:
		case ast::primitive_kind::SIGNED_BYTE:
		case ast::primitive_kind::UNSIGNED_BYTE:
		case ast::primitive_kind::SIGNED_CHAR:
		case ast::primitive_kind::UNSIGNED_CHAR:
			ik = insn_op_kind::STOREB;
			break;
		case ast::primitive_kind::SIGNED_SHORT:
		case ast::primitive_kind::UNSIGNED_SHORT:
			ik = insn_op_kind::STOREH;
			break;
		case ast::primitive_kind::SIGNED_INT:
		case ast::primitive_kind::UNSIGNED_INT:
			ik = insn_op_kind::STOREW;
			break;
		case ast::primitive_kind::SIGNED_LONG:
		case ast::primitive_kind::UNSIGNED_LONG:
			ik = insn_op_kind::STOREL;
			break;
		case ast::primitive_kind::FLOAT:
			ik = insn_op_kind::STORES;
			break;
		case ast::primitive_kind::DOUBLE:
			ik = insn_op_kind::STORED;
			break;
		default:
			util::ice("ir_load",
				"Unrecognized primitive_kind while trying to store to a location!");
		}

		util::maybe_ice(ik != 0x0,
			"ir_load",
			"Expected a valid primitive_kind and insn_op_kind for a primitive store!");

		type insn* i = insn_binary_init(
			ik,
			src,
			operand_immediate_init(immediate_operand_integral_init(off),
				typ_primitive_init(primitive_kind::WORD)),
			operand_copy(loc));

		util::vector_append(ctx->insns, i$ as byte*);
		return;
	}
		break;
	case ast::typ_kind::TUP: {
		n_size_data(final_typ);

		type operand* tmp = NULL as type operand*;

		if (off != 0) {
			tmp = operand_register_init(register_operand_init(ctx->counter++),
				typ_primitive_init(primitive_kind::WORD));

			type insn* move = insn_binary_init(
				insn_op_kind::ADDW,
				operand_copy(tmp),
				operand_copy(loc),
				operand_immediate_init(immediate_operand_integral_init(off),
					typ_primitive_init(primitive_kind::WORD)));
			util::vector_append(ctx->insns, move$ as byte*);
		}
		else
			tmp = loc;

		type insn* cpy = insn_binary_init(
			insn_op_kind::MEMCPY,
			tmp,
			operand_copy(src),
			operand_immediate_init(
				immediate_operand_integral_init(final_typ->size_data->size),
				typ_primitive_init(primitive_kind::WORD)));
		util::vector_append(ctx->insns, cpy$ as byte*);
		return;
	}
		break;
	// TODO
	default:
		util::ice("ir_store",
			"Unrecognized typ_kind while trying to store to a location!");
	}

	util::ice("ir_store",
		"This should be unreachable!");
}

} } // namespace neutrino::ir
