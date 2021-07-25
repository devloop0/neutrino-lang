import "ast/ast.hsp"

import <"std/io">
import <"std/lib">

import "util/base.hsp"
import "util/error.hsp"
import "util/vector.hsp"
import "ir/util.hsp"

using std::lib::NULL;
using std::io::printf;
using neutrino::util::n_malloc;
using neutrino::util::n_free;

namespace neutrino { namespace ast {

func type typ_qualifiers* typ_qualifiers_init(type util::vector* q) {
	type typ_qualifiers* tq = n_malloc(sizeof{type typ_qualifiers})
		as type typ_qualifiers*;
	tq->qualifiers = q;
	return tq;
}

func void typ_qualifiers_delete(type typ_qualifiers* tq) {
	util::vector_delete(tq->qualifiers);
	n_free(tq as byte*);
}

func type fn_typ* fn_typ_init(type util::vector* pts, bool iv, type typ* r) {
	type fn_typ* ft = n_malloc(sizeof{type fn_typ})
		as type fn_typ*;
	ft->parameter_typs = pts;
	ft->is_variadic = iv;
	ft->return_typ = r;
	return ft;
}

func void fn_typ_delete(type fn_typ* ft) {
	util::vector_delete(ft->parameter_typs);
	typ_delete(ft->return_typ);
	n_free(ft as byte*);
}

func type lambda_typ* lambda_typ_init(type typ* s, unsigned int uid,
	bool hcl) {
	type lambda_typ* lt = n_malloc(sizeof{type lambda_typ}) as type lambda_typ*;
	lt->signature = s;
	lt->unique_identifier = uid;
	lt->has_capture_list = hcl;
	return lt;
}

func void lambda_typ_delete(type lambda_typ* lt) {
	typ_delete(lt->signature);
	n_free(lt as byte*);
}

func type typ* typ_primitive_init(type typ_qualifiers* tq,
	unsigned int p, type metadata* m) {
	type typ* t = n_malloc(sizeof{type typ})
		as type typ*;
	t->kind = typ_kind::PRIMITIVE;
	t->typ_qualifiers = tq;
	t->which.primitive = p;
	t->metadata = m;
	t->size_data = NULL as type ir::size_data*;
	return t;
}

func type typ* typ_aggregate_name_init(type typ_qualifiers* tq,
	type qualified_identifier* an, type metadata* m) {
	type typ* t = n_malloc(sizeof{type typ})
		as type typ*;
	t->kind = typ_kind::AGGREGATE_NAME;
	t->typ_qualifiers = tq;
	t->which.aggregate_name = an;
	t->metadata = m;
	t->size_data = NULL as type ir::size_data*;
	return t;
}

func type typ* typ_fn_typ_init(type typ_qualifiers* tq,
	type fn_typ* ft, type metadata* m) {
	type typ* t = n_malloc(sizeof{type typ})
		as type typ*;
	t->typ_qualifiers = tq;
	t->which.fn_typ = ft;
	t->kind = typ_kind::FN_TYP;
	t->metadata = m;
	t->size_data = NULL as type ir::size_data*;
	return t;
}

func type typ* typ_tup_init(type typ_qualifiers* tq,
	type util::vector* tt, type metadata* m) {
	type typ* t = n_malloc(sizeof{type typ})
		as type typ*;
	t->typ_qualifiers = tq;
	t->which.tup_typ = tt;
	t->metadata = m;
	t->kind = typ_kind::TUP;
	t->size_data = NULL as type ir::size_data*;
	return t;
}

func type typ* typ_lambda_typ_init(type typ_qualifiers* tq,
	type lambda_typ* lt, type metadata* m) {
	type typ* t = n_malloc(sizeof{type typ})
		as type typ*;
	t->typ_qualifiers = tq;
	t->which.lambda_typ = lt;
	t->metadata = m;
	t->kind = typ_kind::LAMBDA;
	t->size_data = NULL as type ir::size_data*;
	return t;
}

func void typ_delete(type typ* t) {
	typ_qualifiers_delete(t->typ_qualifiers);

	switch (t->kind) {
	case typ_kind::PRIMITIVE:
		break;
	case typ_kind::AGGREGATE_NAME:
		qualified_identifier_delete(t->which.aggregate_name);
		break;
	case typ_kind::FN_TYP:
		fn_typ_delete(t->which.fn_typ);
		break;
	case typ_kind::TUP:
		util::vector_delete(t->which.tup_typ);
		break;
	case typ_kind::LAMBDA:
		lambda_typ_delete(t->which.lambda_typ);
		break;
	default:
		util::ice("typ_delete",
			"Unrecognized typ_kind while free'ing!");
		break;
	}

	if (t->metadata != NULL as type metadata*)
		metadata_delete(t->metadata);
	if (t->size_data != NULL as type ir::size_data*)
		ir::size_data_delete(t->size_data);
	n_free(t as byte*);
}

func[static] void typ_qualifiers_print(type typ_qualifiers* tq) {
	printf("(TYP-QUALIFIERS:");

	util::maybe_ice(!util::vector_empty(tq->qualifiers),
		"typ_qualifiers_print",
		"Expected a non-empty qualifiers list to print here!");

	for (unsigned int i = 0; i < util::vector_size(tq->qualifiers) - 1; i++) {
		bool is_mut = util::vector_at(tq->qualifiers, i) as bool* @;
		if (is_mut)
			printf(" [MUT *]");
		else
			printf(" [*]");
	}

	bool last = util::vector_at(tq->qualifiers,
		util::vector_size(tq->qualifiers) - 1) as bool* @;
	if (last)
		printf(" MUT");

	printf(")");
}

func void typ_print(type typ* t, unsigned int ta) {
	for (unsigned int i = 0; i < ta; i++)
		printf(" ");
	printf("(");

	typ_qualifiers_print(t->typ_qualifiers), printf(" ");

	switch (t->kind) {
	case typ_kind::PRIMITIVE: {
		printf("TYP-PRIMITIVE: ");

		switch (t->which.primitive) {
		case primitive_kind::BOOL:
			printf("BOOL");
			break;
		case primitive_kind::SIGNED_CHAR:
			printf("SIGNED-CHAR");
			break;
		case primitive_kind::UNSIGNED_CHAR:
			printf("UNSIGNED-CHAR");
			break;
		case primitive_kind::SIGNED_BYTE:
			printf("SIGNED-BYTE");
			break;
		case primitive_kind::UNSIGNED_BYTE:
			printf("UNSIGNED-BYTE");
			break;
		case primitive_kind::SIGNED_SHORT:
			printf("SIGNED-SHORT");
			break;
		case primitive_kind::UNSIGNED_SHORT:
			printf("UNSIGNED-SHORT");
			break;
		case primitive_kind::SIGNED_INT:
			printf("SIGNED-INT");
			break;
		case primitive_kind::UNSIGNED_INT:
			printf("UNSIGNED-INT");
			break;
		case primitive_kind::SIGNED_LONG:
			printf("SIGNED-LONG");
			break;
		case primitive_kind::UNSIGNED_LONG:
			printf("UNSIGNED-LONG");
			break;
		case primitive_kind::FLOAT:
			printf("FLOAT");
			break;
		case primitive_kind::DOUBLE:
			printf("DOUBLE");
			break;
		case primitive_kind::VOID:
			printf("VOID");
			break;
		default:
			util::ice("typ_print_helper",
				"Unrecognized primitive_kind while printing!");
		}
	}
		break;
	case typ_kind::FN_TYP: {
		printf("TYP-FN:\n");
		type ast::fn_typ* ft = t->which.fn_typ;

		type util::vector* par_typs = ft->parameter_typs;
		if (util::vector_empty(par_typs)) {
			for (unsigned int i = 0; i < ta + 1; i++)
				printf(" ");
			printf("[]");
		}
		else {
			for (unsigned int i = 0; i < ta + 1; i++)
				printf(" ");
			printf("[\n");
			for (unsigned int i = 0; i < util::vector_size(par_typs); i++) {
				type ast::typ* par_typ = util::vector_at(par_typs, i)
					as type ast::typ** @;
				typ_print(par_typ, ta + 2);

				if (i != util::vector_size(par_typs) - 1)
					printf(",");
				printf("\n");
			}
			for (unsigned int i = 0; i < ta + 1; i++)
				printf(" ");
			printf("]");
		}
		
		if (ft->is_variadic)
			printf(" VARIADIC");
		printf("\n");
		typ_print(ft->return_typ, ta + 1);
	}
		break;
	case typ_kind::AGGREGATE_NAME:
		printf("TYP-AGGREGATE-NAME: ");
		print_qualified_identifier(t->which.aggregate_name, ta);
		break;
	case typ_kind::TUP: {
		type util::vector* tt = t->which.tup_typ;

		printf("TYP-TUP:");
		if (!util::vector_empty(tt))
			printf("\n");

		for (unsigned int i = 0; i < util::vector_size(tt); i++) {
			type typ* curr = util::vector_at(tt, i) as type typ** @;

			typ_print(curr, ta + 1);
			if (i != util::vector_size(tt) - 1)
				printf("\n");
		}
	}
		break;
	case typ_kind::LAMBDA: {
		printf("TYP-LAMBDA:\n");

		type lambda_typ* lt = t->which.lambda_typ;

		for (unsigned int i = 0; i < ta + 1; i++)
			printf(" ");
		printf("<UID: %u>\n", lt->unique_identifier);

		typ_print(lt->signature, ta + 1), printf("\n");

		for (unsigned int i = 0; i < ta + 1; i++)
			printf(" ");
		if (lt->has_capture_list)
			printf("<CAPTURE-LIST>");
		else
			printf("<NO-CAPTURE-LIST>");
	}
		break;
	default:
		util::ice("typ_print_helper",
			"Unrecognized typ_kind while printing!");
	}

	printf(")");
}

} } // namespace neutrino::ast
