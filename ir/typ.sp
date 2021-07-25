import "ir/typ.hsp"

import <"std/lib">
import <"std/io">

import "ir/util.hsp"
import "util/error.hsp"
import "util/base.hsp"
import "util/vector.hsp"

using std::lib::NULL;
using std::io::printf;
using neutrino::util::n_malloc;
using neutrino::util::n_free;
using neutrino::util::n_strdup;

namespace neutrino { namespace ir {

func type typ* typ_primitive_init(unsigned int pk) {
	type typ* t = n_malloc(sizeof{type typ})
		as type typ*;
	t->kind = typ_kind::PRIMITIVE;
	t->which.primitive = pk;
	return t;
}

func type typ* typ_aggregate_name_init(char* an) {
	type typ* t = n_malloc(sizeof{type typ})
		as type typ*;
	t->kind = typ_kind::AGGREGATE_NAME;
	t->which.aggregate_name = an;
	return t;
}

func void typ_delete(type typ* t) {
	switch (t->kind) {
	case typ_kind::PRIMITIVE:
		break;
	case typ_kind::AGGREGATE_NAME:
		n_free(t->which.aggregate_name as byte*);
		break;
	default:
		util::ice("typ_delete",
			"Unrecognized ir::typ_kind while free'ing!");
	}

	n_free(t as byte*);
}

func type typ* typ_copy(type typ* t) {
	switch (t->kind) {
	case typ_kind::PRIMITIVE:
		return typ_primitive_init(t->which.primitive);
	case typ_kind::AGGREGATE_NAME:
		return typ_aggregate_name_init(n_strdup(t->which.aggregate_name));
	default:
		util::ice("typ_copy",
			"Unrecognized ir::typ_kind while copying!");
	}

	util::ice("typ_copy",
		"This should be unreachable!");
}

func void typ_primitive_print(unsigned int pk) {
	switch (pk) {
	case primitive_kind::BYTE:
		printf("b");
		break;
	case primitive_kind::HALF:
		printf("h");
		break;
	case primitive_kind::WORD:
		printf("w");
		break;
	case primitive_kind::LONG:
		printf("l");
		break;
	case primitive_kind::SINGLE:
		printf("s");
		break;
	case primitive_kind::DOUBLE:
		printf("d");
		break;
	default:
		util::ice("typ_primitive_print",
			"Unrecognized ir::primitive_kind while printing!");
	}
}

func void typ_print(type typ* t) {
	switch (t->kind) {
	case typ_kind::PRIMITIVE:
		typ_primitive_print(t->which.primitive);
		break;
	case typ_kind::AGGREGATE_NAME: 
		printf("$%s", t->which.aggregate_name);
		break;
	default:
		util::ice("typ_print",
			"Unrecognized ir::typ_kind while printing!");
	}
}

} } // namespace neutrino::ir
