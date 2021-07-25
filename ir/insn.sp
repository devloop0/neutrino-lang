import "ir/insn.hsp"

import <"std/io">
import <"std/lib">

import "ir/operand.hsp"
import "ir/util.hsp"
import "util/base.hsp"
import "util/error.hsp"

using std::lib::NULL;
using std::io::printf;
using neutrino::util::n_malloc;
using neutrino::util::n_free;
using neutrino::util::n_strdup;

namespace neutrino { namespace ir {

func type insn* insn_nullary_init(unsigned int ok) {
	type insn* i = n_malloc(sizeof{type insn}) as type insn*;
	i->op_kind = ok;
	i->lhs = NULL as type operand*;
	i->rhs1 = NULL as type operand*;
	i->rhs2 = NULL as type operand*;
	i->multi = NULL as type util::vector*;
	i->asm = NULL as char*;
	return i;
}

func type insn* insn_unary_init(unsigned int ok, type operand* l, type operand* r1) {
	type insn* i = n_malloc(sizeof{type insn}) as type insn*;
	i->op_kind = ok;
	i->lhs = l;
	i->rhs1 = r1;
	i->rhs2 = NULL as type operand*;
	i->multi = NULL as type util::vector*;
	i->asm = NULL as char*;
	return i;
}

func type insn* insn_binary_init(unsigned int ok, type operand* l, type operand* r1, type operand* r2) {
	type insn* i = n_malloc(sizeof{type insn}) as type insn*;
	i->op_kind = ok;
	i->lhs = l;
	i->rhs1 = r1;
	i->rhs2 = r2;
	i->multi = NULL as type util::vector*;
	i->asm = NULL as char*;
	return i;
}

func type insn* insn_multi_init(unsigned int ok, type operand* l, type operand* r1,
	type operand* r2, type util::vector* m) {
	type insn* i = n_malloc(sizeof{type insn}) as type insn*;
	i->op_kind = ok;
	i->lhs = l;
	i->rhs1 = r1;
	i->rhs2 = r2;
	i->multi = m;
	i->asm = NULL as char*;
	return i;
}

func type insn* insn_asm_init(unsigned int ok, char* a, type operand* l) {
	type insn* i = n_malloc(sizeof{type insn}) as type insn*;
	i->op_kind = ok;
	i->lhs = l;
	i->rhs1 = NULL as type operand*;
	i->rhs2 = NULL as type operand*;
	i->multi = NULL as type util::vector*;
	i->asm = a;
	return i;
}

func void insn_delete(type insn* i) {
	if (i->lhs != NULL as type operand*)
		operand_delete(i->lhs);
	if (i->rhs1 != NULL as type operand*)
		operand_delete(i->rhs1);
	if (i->rhs2 != NULL as type operand*)
		operand_delete(i->rhs2);
	if (i->multi != NULL as type util::vector*)
		util::vector_delete(i->multi);
	if (i->asm != NULL as char*)
		n_free(i->asm as byte*);
	n_free(i as byte*);
}

func type insn* insn_copy(type insn* i) {
	type insn* c = n_malloc(sizeof{type insn}) as type insn*;
	c->op_kind = i->op_kind;

	if (i->lhs != NULL as type operand*)
		c->lhs = operand_copy(i->lhs);
	else
		c->lhs = NULL as type operand*;

	if (i->rhs1 != NULL as type operand*)
		c->rhs1 = operand_copy(i->rhs1);
	else
		c->rhs1 = NULL as type operand*;
	
	if (i->rhs2 != NULL as type operand*)
		c->rhs2 = operand_copy(i->rhs2);
	else
		c->rhs2 = NULL as type operand*;

	if (i->multi != NULL as type util::vector*) {
		type util::vector* multi = util::vector_init(sizeof{type operand*},
			deref_operand_free);

		for (unsigned int j = 0; j < util::vector_size(i->multi); j++) {
			type operand* co = util::vector_at(i->multi, j) as type operand** @,
				cco = operand_copy(co);

			util::vector_append(multi, cco$ as byte*);
		}

		c->multi = multi;
	}
	else
		c->multi = NULL as type util::vector*;

	if (i->asm != NULL as char*)
		c->asm = n_strdup(i->asm);
	else
		c->asm = NULL as char*;
	return c;
}

func[static] void insn_op_print_helper(unsigned int iok) {
	switch (iok) {
	case insn_op_kind::LABEL:
		printf("LABEL");
		break;
	case insn_op_kind::MOVB:
		printf("MOV.B");
		break;
	case insn_op_kind::MOVH:
		printf("MOV.H");
		break;
	case insn_op_kind::MOVW:
		printf("MOV.W");
		break;
	case insn_op_kind::MOVL:
		printf("MOV.L");
		break;
	case insn_op_kind::MOVS:
		printf("MOV.S");
		break;
	case insn_op_kind::MOVD:
		printf("MOV.D");
		break;
	case insn_op_kind::EXTSBL:
		printf("EXT.S.B.L");
		break;
	case insn_op_kind::EXTUBL:
		printf("EXT.U.B.L");
		break;
	case insn_op_kind::EXTSBH:
		printf("EXT.S.B.H");
		break;
	case insn_op_kind::EXTUBH:
		printf("EXT.U.B.H");
		break;
	case insn_op_kind::EXTSBW:
		printf("EXT.S.B.W");
		break;
	case insn_op_kind::EXTUBW:
		printf("EXT.U.B.W");
		break;

	case insn_op_kind::EXTSHL:
		printf("EXT.S.H.L");
		break;
	case insn_op_kind::EXTUHL:
		printf("EXT.U.H.L");
		break;
	case insn_op_kind::EXTSHW:
		printf("EXT.S.H.W");
		break;
	case insn_op_kind::EXTUHW:
		printf("EXT.U.H.W");
		break;

	case insn_op_kind::EXTSWL:
		printf("EXT.S.W.L");
		break;
	case insn_op_kind::EXTUWL:
		printf("EXT.U.W.L");
		break;

	case insn_op_kind::TRUNCLB:
		printf("TRUNC.L.B");
		break;
	case insn_op_kind::TRUNCLH:
		printf("TRUNC.L.H");
		break;
	case insn_op_kind::TRUNCLW:
		printf("TRUNC.L.W");
		break;
	case insn_op_kind::TRUNCWB:
		printf("TRUNC.W.B");
		break;
	case insn_op_kind::TRUNCWH:
		printf("TRUNC.W.H");
		break;
	case insn_op_kind::TRUNCHB:
		printf("TRUNC.H.B");
		break;

	case insn_op_kind::CVTSD:
		printf("CVT.S.D");
		break;
	case insn_op_kind::CVTDS:
		printf("CVT.D.S");
		break;

	case insn_op_kind::CVTSSB:
		printf("CVT.S.SB");
		break;
	case insn_op_kind::CVTSUB:
		printf("CVT.S.UB");
		break;
	case insn_op_kind::CVTSSH:
		printf("CVT.S.SH");
		break;
	case insn_op_kind::CVTSUH:
		printf("CVT.S.UH");
		break;
	case insn_op_kind::CVTSSW:
		printf("CVT.S.SW");
		break;
	case insn_op_kind::CVTSUW:
		printf("CVT.S.UW");
		break;
	case insn_op_kind::CVTSSL:
		printf("CVT.S.SL");
		break;
	case insn_op_kind::CVTSUL:
		printf("CVT.S.UL");
		break;

	case insn_op_kind::CVTDSB:
		printf("CVT.D.SB");
		break;
	case insn_op_kind::CVTDUB:
		printf("CVT.D.UB");
		break;
	case insn_op_kind::CVTDSH:
		printf("CVT.D.SH");
		break;
	case insn_op_kind::CVTDUH:
		printf("CVT.D.UH");
		break;
	case insn_op_kind::CVTDSW:
		printf("CVT.D.SW");
		break;
	case insn_op_kind::CVTDUW:
		printf("CVT.D.UW");
		break;
	case insn_op_kind::CVTDSL:
		printf("CVT.D.SL");
		break;
	case insn_op_kind::CVTDUL:
		printf("CVT.D.UL");
		break;

	case insn_op_kind::CVTSBS:
		printf("CVT.SB.S");
		break;
	case insn_op_kind::CVTUBS:
		printf("CVT.UB.S");
		break;
	case insn_op_kind::CVTSHS:
		printf("CVT.SH.S");
		break;
	case insn_op_kind::CVTUHS:
		printf("CVT.UH.S");
		break;
	case insn_op_kind::CVTSWS:
		printf("CVT.SW.S");
		break;
	case insn_op_kind::CVTUWS:
		printf("CVT.UW.S");
		break;
	case insn_op_kind::CVTSLS:
		printf("CVT.SL.S");
		break;
	case insn_op_kind::CVTULS:
		printf("CVT.UL.S");
		break;

	case insn_op_kind::CVTSBD:
		printf("CVT.SB.D");
		break;
	case insn_op_kind::CVTUBD:
		printf("CVT.UB.D");
		break;
	case insn_op_kind::CVTSHD:
		printf("CVT.SH.D");
		break;
	case insn_op_kind::CVTUHD:
		printf("CVT.UH.D");
		break;
	case insn_op_kind::CVTSWD:
		printf("CVT.SW.D");
		break;
	case insn_op_kind::CVTUWD:
		printf("CVT.UW.D");
		break;
	case insn_op_kind::CVTSLD:
		printf("CVT.SL.D");
		break;
	case insn_op_kind::CVTULD:
		printf("CVT.UL.D");
		break;

	case insn_op_kind::ALLOCA:
		printf("ALLOCA");
		break;

	case insn_op_kind::STOREB:
		printf("STORE.B");
		break;
	case insn_op_kind::STOREH:
		printf("STORE.H");
		break;
	case insn_op_kind::STOREW:
		printf("STORE.W");
		break;
	case insn_op_kind::STOREL:
		printf("STORE.L");
		break;
	case insn_op_kind::STORES:
		printf("STORE.S");
		break;
	case insn_op_kind::STORED:
		printf("STORE.D");
		break;

	case insn_op_kind::LOADB:
		printf("LOAD.B");
		break;
	case insn_op_kind::LOADH:
		printf("LOAD.H");
		break;
	case insn_op_kind::LOADW:
		printf("LOAD.W");
		break;
	case insn_op_kind::LOADL:
		printf("LOAD.L");
		break;
	case insn_op_kind::LOADS:
		printf("LOAD.S");
		break;
	case insn_op_kind::LOADD:
		printf("LOAD.D");
		break;

	case insn_op_kind::MEMCPY:
		printf("MEMCPY");
		break;

	case insn_op_kind::ADDB:
		printf("ADD.B");
		break;
	case insn_op_kind::ADDH:
		printf("ADD.H");
		break;
	case insn_op_kind::ADDW:
		printf("ADD.W");
		break;
	case insn_op_kind::ADDL:
		printf("ADD.L");
		break;
	case insn_op_kind::ADDS:
		printf("ADD.S");
		break;
	case insn_op_kind::ADDD:
		printf("ADD.D");
		break;
	// TODO
	default:
		util::ice("insn_op_print_helper",
			"Unrecognized insn_op_kind while printing!");
	}
}

func void insn_print(type insn* i) {
	insn_op_print_helper(i->op_kind);

	bool l = i->lhs != NULL as type operand*,
		r1 = i->rhs1 != NULL as type operand*,
		r2 = i->rhs2 != NULL as type operand*,
		m = i->multi != NULL as type util::vector*
			&& !util::vector_empty(i->multi),
		a = i->asm != NULL as char*;

	if (l) {
		printf(" "), operand_print(i->lhs);
		if (r1 || r2 || m || a)
			printf(",");
	}

	if (r1) {
		printf(" "), operand_print(i->rhs1);
		if (r2 || m || a)
			printf(",");
	}

	if (r2) {
		printf(" "), operand_print(i->rhs2);
		if (m || a)
			printf(",");
	}

	if (m) {
		for (unsigned int j = 0; j < util::vector_size(i->multi); j++) {
			type operand* o = util::vector_at(i->multi, j) as type operand** @;

			printf(" "), operand_print(o);

			if (j != util::vector_size(i->multi) - 1)
				printf(",");
		}

		if (a)
			printf(",");
	}

	if (a) {
		printf(" %s", i->asm);
	}
}

func type static_data_member* static_data_member_init(unsigned int a, unsigned int p,
	type immediate_operand* io, unsigned int n) {
	type static_data_member* sdm = n_malloc(sizeof{type static_data_member})
		as type static_data_member** @;
	sdm->alignment = a;
	sdm->primitive = p;
	sdm->immediate_operand = io;
	sdm->num = n;
	return sdm;
}

func void static_data_member_delete(type static_data_member* sdm) {
	immediate_operand_delete(sdm->immediate_operand);
	n_free(sdm as byte*);
}

func type static_data_member* static_data_member_copy(type static_data_member* sdm) {
	return static_data_member_init(sdm->alignment, sdm->primitive,
		immediate_operand_copy(sdm->immediate_operand), sdm->num);
}

func void static_data_member_print(type static_data_member* sdm) {
	printf("[%u] ", sdm->alignment);
	typ_primitive_print(sdm->primitive);
	printf(": "), immediate_operand_print(sdm->immediate_operand);
	printf(" (%u)", sdm->num);
}

func type static_data* static_data_init(bool il, char* l, type util::vector* d) {
	type static_data* sd = n_malloc(sizeof{type static_data})
		as type static_data*;
	sd->is_local = il;
	sd->label = l;
	sd->data = d;
	return sd;
}

func void static_data_delete(type static_data* sd) {
	n_free(sd->label as byte*);
	util::vector_delete(sd->data);
	n_free(sd as byte*);
}

func type static_data* static_data_copy(type static_data* sd) {
	type util::vector* tmp = util::vector_init(sizeof{type static_data_member*},
		deref_static_data_member_free);
	
	for (unsigned int i = 0; i < util::vector_size(sd->data); i++) {
		type static_data_member* curr_sdm = util::vector_at(sd->data, i)
			as type static_data_member** @;

		type static_data_member* copy = static_data_member_copy(curr_sdm);
		util::vector_append(tmp, copy$ as byte*);
	}

	return static_data_init(sd->is_local, n_strdup(sd->label), tmp);
}

func void static_data_print(type static_data* sd) {
	printf("%s%s = {", sd->is_local ? "local " : "", sd->label);

	for (unsigned int i = 0; i < util::vector_size(sd->data); i++) {
		type static_data_member* curr_sdm = util::vector_at(sd->data, i)
			as type static_data_member** @;

		printf(" "), static_data_member_print(curr_sdm);

		if (i != util::vector_size(sd->data) - 1)
			printf(",");
		printf(" ");
	}

	printf("}");
}

func type function_parameter* function_parameter_init(type typ* t,
	type register_operand* ro) {
	type function_parameter* fp = n_malloc(sizeof{type function_parameter})
		as type function_parameter*;
	fp->typ = t;
	fp->register_operand = ro;
	return fp;
}

func void function_parameter_delete(type function_parameter* fp) {
	typ_delete(fp->typ);
	register_operand_delete(fp->register_operand);
	n_free(fp as byte*);
}

func type function_parameter* function_parameter_copy(type function_parameter* fp) {
	return function_parameter_init(typ_copy(fp->typ),
		register_operand_copy(fp->register_operand));
}

func void function_parameter_print(type function_parameter* fp) {
	register_operand_print(fp->register_operand),
		printf(": "), typ_print(fp->typ);
}

func type function* function_init(bool il, char* n, type util::vector* ps,
	bool iv, type typ* rt, type util::vector* is) {
	type function* f = n_malloc(sizeof{type function})
		as type function*;
	f->is_local = il;
	f->name = n;
	f->parameters = ps;
	f->is_variadic = iv;
	f->return_typ = rt;
	f->insns = is;
	return f;
}

func void function_delete(type function* f) {
	n_free(f->name as byte*);
	util::vector_delete(f->parameters);
	typ_delete(f->return_typ);
	if (f->insns != NULL as type util::vector*)
		util::vector_delete(f->insns);
	n_free(f as byte*);
}

func type function* function_copy(type function* f) {
	type util::vector* tmp_ps = util::vector_init(sizeof{type function_parameter*},
		deref_function_parameter_free);

	for (unsigned int i = 0; i < util::vector_size(f->parameters); i++) {
		type function_parameter* curr_fp = util::vector_at(f->parameters, i)
			as type function_parameter** @;

		type function_parameter* copy = function_parameter_copy(curr_fp);
		util::vector_append(tmp_ps, copy$ as byte*);
	}

	type util::vector* tmp_is = NULL as type util::vector*;
	if (f->insns != NULL as type util::vector*) {
		tmp_is = util::vector_init(sizeof{type insn*}, deref_insn_free);

		for (unsigned int i = 0; i < util::vector_size(f->insns); i++) {
			type insn* curr_i = util::vector_at(f->insns, i)
				as type insn** @;

			type insn* copy = insn_copy(curr_i);
			util::vector_append(tmp_is, copy$ as byte*);
		}
	}
	else
		tmp_is = NULL as type util::vector*;

	return function_init(f->is_local, n_strdup(f->name), tmp_ps,
		f->is_variadic, typ_copy(f->return_typ), tmp_is);
}

func void function_print(type function* f) {
	if (f->is_local)
		printf("local ");

	printf("fun %s(", f->name);

	for (unsigned int i = 0; i < util::vector_size(f->parameters); i++) {
		type function_parameter* curr_fp = util::vector_at(f->parameters, i)
			as type function_parameter** @;

		function_parameter_print(curr_fp);

		if (i != util::vector_size(f->parameters) - 1 || f->is_variadic)
			printf(", ");
	}

	if (f->is_variadic)
		printf("...");
	printf(") : ");
	typ_print(f->return_typ);

	if (f->insns == NULL as type util::vector*)
		printf(";");
	else {
		printf(" {\n");

		for (unsigned int i = 0; i < util::vector_size(f->insns); i++) {
			type insn* ci = util::vector_at(f->insns, i)
				as type insn** @;

			if (ci->op_kind != insn_op_kind::LABEL)
				printf("    ");
			insn_print(ci), printf("\n");
		}

		printf("}");
	}
}

func type top_level* top_level_static_data_init(type static_data* sd) {
	type top_level* tl = n_malloc(sizeof{type top_level})
		as type top_level*;
	tl->kind = top_level_kind::STATIC_DATA;
	tl->which.static_data = sd;
	return tl;
}

func type top_level* top_level_function_init(type function* f) {
	type top_level* tl = n_malloc(sizeof{type top_level})
		as type top_level*;
	tl->kind = top_level_kind::FUNCTION;
	tl->which.function = f;
	return tl;
}

func type top_level* top_level_aggregate_type_init(type aggregate_type* at) {
	type top_level* tl = n_malloc(sizeof{type top_level})
		as type top_level*;
	tl->kind = top_level_kind::AGGREGATE_TYPE;
	tl->which.aggregate_type = at;
	return tl;
}

func void top_level_delete(type top_level* tl) {
	switch (tl->kind) {
	case top_level_kind::STATIC_DATA:
		static_data_delete(tl->which.static_data);
		break;
	case top_level_kind::FUNCTION:
		function_delete(tl->which.function);
		break;
	case top_level_kind::AGGREGATE_TYPE:
		aggregate_type_delete(tl->which.aggregate_type);
		break;
	default:
		util::ice("top_level_delete",
			"Unrecognized top_level_kind while free'ing!");
	}

	n_free(tl as byte*);
}

func type top_level* top_level_copy(type top_level* tl) {
	switch (tl->kind) {
	case top_level_kind::STATIC_DATA:
		return top_level_static_data_init(static_data_copy(tl->which.static_data));
	case top_level_kind::FUNCTION:
		return top_level_function_init(function_copy(tl->which.function));
	case top_level_kind::AGGREGATE_TYPE:
		return top_level_aggregate_type_init(aggregate_type_copy(tl->which.aggregate_type));
	default:
		util::ice("top_level_copy",
			"Unrecognized top_level_kind while copying!");
	}

	util::ice("top_level_copy", "This should be unreachable!");
}

func void top_level_print(type top_level* tl) {
	switch (tl->kind) {
	case top_level_kind::STATIC_DATA:
		static_data_print(tl->which.static_data);
		break;
	case top_level_kind::FUNCTION:
		function_print(tl->which.function);
		break;
	case top_level_kind::AGGREGATE_TYPE:
		aggregate_type_print(tl->which.aggregate_type);
		break;
	default:
		util::ice("top_level_print",
			"Unrecognized top_level_kind while printing!");
	}

	printf("\n");
}

func type prog* prog_init(type util::vector* tls) {
	type prog* p = n_malloc(sizeof{type prog}) as type prog*;
	p->top_levels = tls;
	return p;
}

func void prog_delete(type prog* p) {
	util::vector_delete(p->top_levels);
	n_free(p as byte*);
}

func type prog* prog_copy(type prog* p) {
	type util::vector* tmp = util::vector_init(sizeof{type top_level*},
		deref_top_level_free);

	for (unsigned int i = 0; i < util::vector_size(p->top_levels); i++) {
		type top_level* curr_tl = util::vector_at(p->top_levels, i)
			as type top_level** @;

		type top_level* copy = top_level_copy(curr_tl);
		util::vector_append(tmp, copy$ as byte*);
	}

	return prog_init(tmp);
}

func void prog_print(type prog* p) {
	for (unsigned int i = 0; i < util::vector_size(p->top_levels); i++) {
		type top_level* tl = util::vector_at(p->top_levels, i)
			as type top_level** @;

		top_level_print(tl), printf("\n");

		if (i != util::vector_size(p->top_levels) - 1)
			printf("\n");
	}
}

func type aggregate_member_type* aggregate_member_type_init(unsigned int a,
	type typ* t, unsigned int n) {
	type aggregate_member_type* amt = n_malloc(sizeof{type aggregate_member_type})
		as type aggregate_member_type*;
	amt->alignment = a;
	amt->typ = t;
	amt->num = n;
	return amt;
}

func void aggregate_member_type_delete(type aggregate_member_type* amt) {
	typ_delete(amt->typ);
	n_free(amt as byte*);
}

func type aggregate_member_type* aggregate_member_type_copy(type aggregate_member_type* amt) {
	return aggregate_member_type_init(amt->alignment,
		typ_copy(amt->typ), amt->num);
}

func type aggregate_type* aggregate_type_init(char* n, unsigned int a,
	type util::vector* m) {
	type aggregate_type* at = n_malloc(sizeof{type aggregate_type})
		as type aggregate_type*;
	at->name = n;
	at->alignment = a;
	at->members = m;
	return at;
}

func void aggregate_type_delete(type aggregate_type* at) {
	n_free(at->name as byte*);
	if (at->members != NULL as type util::vector*)
		util::vector_delete(at->members);
	n_free(at as byte*);
}

func type aggregate_type* aggregate_type_copy(type aggregate_type* at) {
	type util::vector* vec = util::vector_init(sizeof{type aggregate_member_type*},
		deref_aggregate_member_type_free);

	for (unsigned int i = 0; i < util::vector_size(at->members); i++) {
		type aggregate_member_type* amt = util::vector_at(at->members, i)
			as type aggregate_member_type** @;
		
		util::vector_append(vec, amt$ as byte*);
	}

	return aggregate_type_init(n_strdup(at->name), at->alignment, vec);
}

func void aggregate_type_print(type aggregate_type* at) {
	printf("type $%s (%u) ", at->name, at->alignment);

	if (at->members == NULL as type util::vector*)
		printf(";");
	else {
		printf(" { ");
		for (unsigned int i = 0; i < util::vector_size(at->members); i++) {
			type aggregate_member_type* amt = util::vector_at(at->members, i)
				as type aggregate_member_type** @;

			printf("(%u) ", amt->alignment);

			typ_print(amt->typ);

			printf(": %u", amt->num);

			if (i != util::vector_size(at->members) - 1)
				printf(",");
			printf(" ");
		}
		printf("}");
	}
}

} } // namespace neutrino::ir
