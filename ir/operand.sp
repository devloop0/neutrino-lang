import "ir/operand.hsp"

import <"std/lib">
import <"std/io">

import "ir/typ.hsp"
import "util/base.hsp"
import "util/error.hsp"

using std::io::printf;
using std::lib::NULL;
using neutrino::util::n_malloc;
using neutrino::util::n_free;
using neutrino::util::n_strdup;

namespace neutrino { namespace ir {

func type register_operand* register_operand_init(unsigned int n) {
	type register_operand* ro = n_malloc(sizeof{type register_operand})
		as type register_operand*;
	ro->num = n;
	return ro;
}

func void register_operand_delete(type register_operand* ro) {
	n_free(ro as byte*);
}

func type register_operand* register_operand_copy(type register_operand* ro) {
	return register_operand_init(ro->num);
}

func void register_operand_print(type register_operand* ro) {
	printf("%%r%u", ro->num);
}

func type immediate_operand* immediate_operand_integral_init(unsigned int w) {
	type immediate_operand* io = n_malloc(sizeof{type immediate_operand})
		as type immediate_operand*;
	io->which.w = w;
	io->kind = immediate_operand_kind::INTEGRAL;
	return io;
}

func type immediate_operand* immediate_operand_floating_init(double d) {
	type immediate_operand* io = n_malloc(sizeof{type immediate_operand})
		as type immediate_operand*;
	io->which.d = d;
	io->kind = immediate_operand_kind::FLOATING;
	return io;
}

func void immediate_operand_delete(type immediate_operand* io) {
	n_free(io as byte*);
}

func void immediate_operand_print(type immediate_operand* io) {
	switch (io->kind) {
	case immediate_operand_kind::INTEGRAL:
		printf("(w: %u/%d/0x%x)", io->which.w, io->which.w, io->which.w);
		break;
	case immediate_operand_kind::FLOATING: {
		unsigned int* arr = io->which.d$ as unsigned int*;
		printf("(d: 0x");
		for (unsigned int i = 1; i <= 8; i++)
			printf("%x", (arr[1] >> (32 - 4 * i)) & 0xf);
		for (unsigned int i = 1; i <= 8; i++)
			printf("%x", (arr[0] >> (32 - 4 * i)) & 0xf);
		printf(")");
	}
		break;
	default:
		util::ice("immediate_operand_print",
			"Unrecognized immediate_operand_kind while printing!");
	}
}

func type immediate_operand* immediate_operand_copy(type immediate_operand* io) {
	switch (io->kind) {
	case immediate_operand_kind::INTEGRAL:
		return immediate_operand_integral_init(io->which.w);
	case immediate_operand_kind::FLOATING:
		return immediate_operand_floating_init(io->which.d);
	default:
		util::ice("immediate_operand_copy",
			"Unrecognized immediate_operand_kind while copying!");
	}

	util::ice("immediate_operand_copy",
		"This should be unreachable!");
}

func type label_operand* label_operand_init(char* l) {
	type label_operand* lo = n_malloc(sizeof{type label_operand})
		as type label_operand*;
	lo->label = l;
	return lo;
}

func void label_operand_delete(type label_operand* lo) {
	n_free(lo->label as byte*);
	n_free(lo as byte*);
}

func type label_operand* label_operand_copy(type label_operand* lo) {
	return label_operand_init(lo->label);
}

func void label_operand_print(type label_operand* lo) {
	printf("%s", lo->label);
}

func type operand* operand_register_init(type register_operand* ro,
	type typ* t) {
	type operand* o = n_malloc(sizeof{type operand})
		as type operand*;
	o->kind = operand_kind::REGISTER;
	o->which.register_operand = ro;
	o->typ = t;
	return o;
}

func type operand* operand_immediate_init(type immediate_operand* io,
	type typ* t) {
	type operand* o = n_malloc(sizeof{type operand})
		as type operand*;
	o->kind = operand_kind::IMMEDIATE;
	o->which.immediate_operand = io;
	o->typ = t;
	return o;
}

func type operand* operand_label_init(type label_operand* lo,
	type typ* t) {
	type operand* o = n_malloc(sizeof{type operand})
		as type operand*;
	o->kind = operand_kind::LABEL;
	o->which.label_operand = lo;
	o->typ = t;
	return o;
}

func void operand_delete(type operand* o) {
	switch (o->kind) {
	case operand_kind::REGISTER:
		register_operand_delete(o->which.register_operand);
		break;
	case operand_kind::IMMEDIATE:
		immediate_operand_delete(o->which.immediate_operand);
		break;
	case operand_kind::LABEL:
		label_operand_delete(o->which.label_operand);
		break;
	default:
		util::ice("operand_delete",
			"Unrecognized operand_kind while free'ing!");
	}

	typ_delete(o->typ);
	n_free(o as byte*);
}

func type operand* operand_copy(type operand* o) {
	switch (o->kind) {
	case operand_kind::REGISTER:
		return operand_register_init(register_operand_copy(o->which.register_operand),
			typ_copy(o->typ));
	case operand_kind::IMMEDIATE:
		return operand_immediate_init(immediate_operand_copy(o->which.immediate_operand),
			typ_copy(o->typ));
	case operand_kind::LABEL:
		return operand_label_init(label_operand_copy(o->which.label_operand),
			typ_copy(o->typ));
	default:
		util::ice("operand_copy",
			"Unrecognized operand_kind while copying!");
	}

	util::ice("operand_copy",
		"This should be unreachable!");
}

func void operand_print(type operand* o) {
	switch (o->kind) {
	case operand_kind::REGISTER:
		register_operand_print(o->which.register_operand);
		break;
	case operand_kind::IMMEDIATE:
		immediate_operand_print(o->which.immediate_operand);
		break;
	case operand_kind::LABEL:
		label_operand_print(o->which.label_operand);
	default:
		util::ice("operand_print",
			"Unrecognized operand_kind while printing!");
	}

	printf(" ["), typ_print(o->typ), printf("]");
}

} } // namespace neutrino::ir
