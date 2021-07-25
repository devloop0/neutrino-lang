import "ast/ast.hsp"

import <"std/lib">
import <"std/io">

import "lex/token.hsp"
import "util/vector.hsp"
import "util/base.hsp"
import "util/error.hsp"

using std::io::printf;
using std::lib::NULL;
using neutrino::util::n_malloc;
using neutrino::util::n_free;

namespace neutrino { namespace ast {

func type member_init* member_init_init(type lex::token* i,
	type exp* in, type metadata* m) {
	type member_init* mi = n_malloc(sizeof{type member_init})
		as type member_init*;
	mi->ident = i;
	mi->init = in;
	mi->metadata = m;
	mi->symtab_value = NULL as type tck::symtab_value*;
	return mi;
}

func void member_init_delete(type member_init* mi) {
	exp_delete(mi->init);
	if (mi->metadata != NULL as type metadata*)
		metadata_delete(mi->metadata);
	n_free(mi as byte*);
}

func type struct_init* struct_init_init(type qualified_identifier* qi,
	type util::vector* mis, type metadata* m) {
	type struct_init* si = n_malloc(sizeof{type struct_init})
		as type struct_init*;
	si->qualified_identifier = qi;
	si->member_inits = mis;
	si->metadata = m;
	return si;
}

func void struct_init_delete(type struct_init* si) {
	qualified_identifier_delete(si->qualified_identifier);
	util::vector_delete(si->member_inits);
	if (si->metadata != NULL as type metadata*)
		metadata_delete(si->metadata);
	n_free(si as byte*);
}

func type qualified_identifier* qualified_identifier_init(
	type util::vector* n, type ast::metadata* m) {
	type qualified_identifier* qi = n_malloc(
		sizeof{type qualified_identifier})
			as type qualified_identifier*;
	qi->name = n;
	qi->full_name = NULL as type util::vector*;
	qi->metadata = m;
	qi->symtab_value = NULL as type tck::symtab_value*;
	qi->ref_ctx = NULL as type tck::symtab*;
	return qi;
}

func void qualified_identifier_delete(type qualified_identifier* qi) {
	if (qi->metadata != NULL as type metadata*)
		metadata_delete(qi->metadata);
	util::vector_delete(qi->name);
	if (qi->full_name != NULL as type util::vector*)
		util::vector_delete(qi->full_name);
	n_free(qi as byte*);
}

func type primary* primary_struct_init_init(type struct_init* si) {
	type primary* p = n_malloc(sizeof{type primary}) as type primary*;
	p->kind = primary_kind::STRUCT_INIT;
	p->which.struct_init = si;
	p->typ = NULL as type typ*;
	p->metadata = metadata_copy(si->metadata);
	p->value_kind = value_kind::INVALID;
	p->is_constant = false;
	return p;
}

func type primary* primary_qualified_identifier_init(
	type qualified_identifier* qi) {
	type primary* p = n_malloc(sizeof{type primary})
		as type primary*;
	p->kind = primary_kind::QUALIFIED_IDENTIFIER;
	p->which.qualified_identifier = qi;
	p->typ = NULL as type typ*;
	p->metadata = metadata_copy(qi->metadata);
	p->value_kind = value_kind::INVALID;
	p->is_constant = false;
	return p;
}

func type primary* primary_literal_init(type lex::token* l,
	type metadata* m) {
	type primary* p = n_malloc(sizeof{type primary})
		as type primary*;
	p->kind = primary_kind::LITERAL;
	p->which.literal = l;
	p->typ = NULL as type typ*;
	p->metadata = m;
	p->value_kind = value_kind::INVALID;
	p->is_constant = false;
	return p;
}

func type primary* primary_parenthesized_init(type exp* pa,
	type metadata* m) {
	type primary* p = n_malloc(sizeof{type primary})
		as type primary*;
	p->kind = primary_kind::PARENTHESIZED;
	p->which.parenthesized = pa;
	p->typ = NULL as type typ*;
	p->metadata = m;
	p->value_kind = value_kind::INVALID;
	p->is_constant = false;
	return p;
}

func type primary* primary_array_init(type util::vector* a,
	type metadata* m) {
	type primary* p = n_malloc(sizeof{type primary})
		as type primary*;
	p->kind = primary_kind::ARRAY;
	p->which.array = a;
	p->typ = NULL as type typ*;
	p->metadata = m;
	p->value_kind = value_kind::INVALID;
	p->is_constant = false;
	return p;
}

func type primary* primary_stmt_init(type stmt* s, type metadata* m) {
	type primary* p = n_malloc(sizeof{type primary})
		as type primary*;
	p->kind = primary_kind::STMT;
	p->which.stmt = s;
	p->typ = NULL as type typ*;
	p->metadata = m;
	p->value_kind = value_kind::INVALID;
	p->is_constant = false;
	return p;
}

func type primary* primary_tuple_init(type util::vector* t, type metadata* m) {
	type primary* p = n_malloc(sizeof{type primary}) as type primary*;
	p->kind = primary_kind::TUPLE;
	p->which.tuple = t;
	p->typ = NULL as type typ*;
	p->metadata = m;
	p->value_kind = value_kind::INVALID;
	p->is_constant = false;
	return p;
}

func type primary* primary_lambda_init(type function* f) {
	type primary* p = n_malloc(sizeof{type primary}) as type primary*;
	p->kind = primary_kind::LAMBDA;
	p->which.lambda = f;
	p->typ = NULL as type typ*;
	p->metadata = metadata_copy(f->metadata);
	p->value_kind = value_kind::INVALID;
	p->is_constant = false;
	return p;
}

func void primary_delete(type primary* p) {
	switch (p->kind) {
	case primary_kind::QUALIFIED_IDENTIFIER:
		qualified_identifier_delete(p->which.qualified_identifier);
		break;
	case primary_kind::LITERAL:
		break;
	case primary_kind::PARENTHESIZED:
		exp_delete(p->which.parenthesized);
		break;
	case primary_kind::ARRAY:
		util::vector_delete(p->which.array);
		break;
	case primary_kind::STMT:
		stmt_delete(p->which.stmt);
		break;
	case primary_kind::TUPLE:
		util::vector_delete(p->which.tuple);
		break;
	case primary_kind::STRUCT_INIT:
		struct_init_delete(p->which.struct_init);
		break;
	case primary_kind::LAMBDA:
		function_delete(p->which.lambda);
		break;
	default:
		util::ice("primary_delete",
			"Unrecognized primary_kind while free'ing!");
		break;
	}

	if (p->typ != NULL as type typ*)
		typ_delete(p->typ);
	if (p->metadata != NULL as type metadata*)
		metadata_delete(p->metadata);
	n_free(p as byte*);
}

func type member_data* member_data_init(type lex::token* m, bool iu) {
	type member_data* mb = n_malloc(sizeof{type member_data})
		as type member_data*;
	mb->member = m;
	mb->is_ufcs = iu;
	mb->is_resolved_ufcs = false;
	mb->symtab_value = NULL as type tck::symtab_value*;
	return mb;
}

func void member_data_delete(type member_data* mb) {
	n_free(mb as byte*);
}

func type postfix* postfix_member_access_init(type exp* b,
	unsigned int k, type member_data* mb, type metadata* md) {
	type postfix* p = n_malloc(sizeof{type postfix})
		as type postfix*;
	p->base = b;
	p->kind = k;
	p->which.member_data = mb;
	p->typ = NULL as type typ*;
	p->next = NULL as type postfix*;
	p->metadata = md;
	p->value_kind = value_kind::INVALID;
	p->is_constant = false;
	return p;
}

func type postfix* postfix_nullary_init(type exp* b, unsigned int k,
	type metadata* m) {
	type postfix* p = n_malloc(sizeof{type postfix})
		as type postfix*;
	p->base = b;
	p->kind = k;
	p->typ = NULL as type typ*;
	p->next = NULL as type postfix*;
	p->metadata = m;
	p->value_kind = value_kind::INVALID;
	p->is_constant = false;
	return p;
}

func type postfix* postfix_index_init(type exp* b, type exp* i,
	type metadata* m) {
	type postfix* p = n_malloc(sizeof{type postfix})
		as type postfix*;
	p->base = b;
	p->which.index = i;
	p->kind = postfix_kind::INDEX;
	p->next = NULL as type postfix*;
	p->typ = NULL as type typ*;
	p->metadata = m;
	p->value_kind = value_kind::INVALID;
	p->is_constant = false;
	return p;
}

func type postfix* postfix_arguments_init(type exp* b,
	type util::vector* a, type metadata* m) {
	type postfix* p = n_malloc(sizeof{type postfix})
		as type postfix*;
	p->base = b;
	p->which.arguments = a;
	p->kind = postfix_kind::FUNCTION_CALL;
	p->next = NULL as type postfix*;
	p->typ = NULL as type typ*;
	p->metadata = m;
	p->value_kind = value_kind::INVALID;
	p->is_constant = false;
	return p;
}

func type postfix* postfix_as_init(type ast::exp* b, type typ* a,
	type metadata* m) {
	type postfix* p = n_malloc(sizeof{type postfix})
		as type postfix*;
	p->base = b;
	p->which.as_typ = a;
	p->kind = postfix_kind::AS;
	p->next = NULL as type postfix*;
	p->typ = NULL as type typ*;
	p->metadata = m;
	p->value_kind = value_kind::INVALID;
	p->is_constant = false;
	return p;
}

func void postfix_delete(type postfix* p) {
	exp_delete(p->base);

	switch (p->kind) {
	case postfix_kind::INCREMENT:
	case postfix_kind::DECREMENT:
	case postfix_kind::AT:
	case postfix_kind::ADDRESS:
		break;
	case postfix_kind::DOT_INDEX:
	case postfix_kind::ARROW_INDEX:
	case postfix_kind::DOT:
	case postfix_kind::ARROW:
		member_data_delete(p->which.member_data);
		break;
	case postfix_kind::INDEX:
		exp_delete(p->which.index);
		break;
	case postfix_kind::FUNCTION_CALL:
		util::vector_delete(p->which.arguments);
		break;
	case postfix_kind::AS:
		typ_delete(p->which.as_typ);
		break;
	default:
		util::ice("postfix_delete",
			"Unrecognized postfix_kind while free'ing!");
	}

	if (p->typ != NULL as type typ*)
		typ_delete(p->typ);
	if (p->metadata != NULL as type metadata*)
		metadata_delete(p->metadata);
	n_free(p as byte*);
}

func type pat_assign* pat_assign_init(type pat* l, type exp* r,
	type metadata* m) {
	type pat_assign* pa = n_malloc(sizeof{type pat_assign})
		as type pat_assign*;
	pa->lhs = l;
	pa->rhs = r;
	pa->metadata = m;
	pa->is_constant = false;
	pa->typ = NULL as type typ*;
	return pa;
}

func void pat_assign_delete(type pat_assign* pa) {
	pat_delete(pa->lhs);
	exp_delete(pa->rhs);
	if (pa->metadata != NULL as type metadata*)
		metadata_delete(pa->metadata);
	n_free(pa as byte*);
}

func type exp* exp_primary_init(type primary* p) {
	type exp* e = n_malloc(sizeof{type exp})
		as type exp*;
	e->which.primary = p;
	e->kind = exp_kind::PRIMARY;
	e->typ = NULL as type typ*;
	e->metadata = metadata_copy(p->metadata);
	e->value_kind = value_kind::INVALID;
	e->is_constant = p->is_constant;
	return e;
}

func type exp* exp_postfix_init(type postfix* p) {
	type exp* e = n_malloc(sizeof{type exp})
		as type exp*;
	e->which.postfix = p;
	e->kind = exp_kind::POSTFIX;
	e->typ = NULL as type typ*;
	e->metadata = metadata_copy(p->metadata);
	e->value_kind = value_kind::INVALID;
	e->is_constant = p->is_constant;
	return e;
}

func type exp* exp_unary_init(type unary* u) {
	type exp* e = n_malloc(sizeof{type exp})
		as type exp*;
	e->which.unary = u;
	e->kind = exp_kind::UNARY;
	e->typ = NULL as type typ*;
	e->metadata = metadata_copy(u->metadata);
	e->value_kind = value_kind::INVALID;
	e->is_constant = u->is_constant;
	return e;
}

func type exp* exp_binary_init(type binary* b) {
	type exp* e = n_malloc(sizeof{type exp}) as type exp*;
	e->which.binary = b;
	e->kind = exp_kind::BINARY;
	e->typ = NULL as type typ*;
	e->metadata = metadata_copy(b->metadata);
	e->value_kind = value_kind::INVALID;
	e->is_constant = b->is_constant;
	return e;
}

func type exp* exp_ternary_init(type ternary* t) {
	type exp* e = n_malloc(sizeof{type exp}) as type exp*;
	e->which.ternary = t;
	e->kind = exp_kind::TERNARY;
	e->typ = NULL as type typ*;
	e->metadata = metadata_copy(t->metadata);
	e->value_kind = value_kind::INVALID;
	e->is_constant = t->is_constant;
	return e;
}

func type exp* exp_pat_assign_init(type pat_assign* pa) {
	type exp* e = n_malloc(sizeof{type exp}) as type exp*;
	e->which.pat_assign = pa;
	e->kind = exp_kind::PAT_ASSIGN;
	e->typ = NULL as type typ*;
	e->metadata = metadata_copy(pa->metadata);
	e->value_kind = value_kind::INVALID;
	e->is_constant = pa->is_constant;
	return e;
}

func void exp_delete(type exp* e) {
	switch (e->kind) {
	case exp_kind::PRIMARY:
		primary_delete(e->which.primary);
		break;
	case exp_kind::POSTFIX:
		postfix_delete(e->which.postfix);
		break;
	case exp_kind::UNARY:
		unary_delete(e->which.unary);
		break;
	case exp_kind::BINARY:
		binary_delete(e->which.binary);
		break;
	case exp_kind::TERNARY:
		ternary_delete(e->which.ternary);
		break;
	case exp_kind::PAT_ASSIGN:
		pat_assign_delete(e->which.pat_assign);
		break;
	default:
		util::ice("exp_delete",
			"Unrecognized exp_kind while free'ing!");
		break;
	}

	if (e->typ != NULL as type typ*)
		typ_delete(e->typ);
	if (e->metadata != NULL as type metadata*)
		metadata_delete(e->metadata);
	n_free(e as byte*);
}

func type alloc_info* alloc_info_init(type typ* t, type exp* e) {
	type alloc_info* ai = n_malloc(sizeof{type alloc_info})
		as type alloc_info*;
	ai->typ = t;
	ai->exp = e;
	return ai;
}

func void alloc_info_delete(type alloc_info* ai) {
	typ_delete(ai->typ);
	if (ai->exp != NULL as type exp*)
		exp_delete(ai->exp);
	n_free(ai as byte*);
}

func type unary* unary_nullary_init(unsigned int k, type exp* b,
	type metadata* m) {
	type unary* u = n_malloc(sizeof{type unary})
		as type unary*;
	u->kind = k;
	u->which.base = b;
	u->typ = NULL as type typ*;
	u->metadata = m;
	u->value_kind = value_kind::INVALID;
	u->is_constant = false;
	return u;
}

func type unary* unary_typ_init(unsigned int k, type typ* t,
	type metadata* m) {
	type unary* u = n_malloc(sizeof{type unary})
		as type unary*;
	u->which.of_typ = t;
	u->kind = k;
	u->typ = NULL as type typ*;
	u->metadata = m;
	u->value_kind = value_kind::INVALID;
	u->is_constant = false;
	return u;
}

func type unary* unary_exp_init(unsigned int k, type exp* e,
	type metadata* m) {
	type unary* u = n_malloc(sizeof{type unary})
		as type unary*;
	u->which.of_exp = e;
	u->kind = k;
	u->typ = NULL as type typ*;
	u->metadata = m;
	u->value_kind = value_kind::INVALID;
	u->is_constant = false;
	return u;
}

func type unary* unary_alloc_info_init(unsigned int k, type alloc_info* ai,
	type metadata* m) {
	type unary* u = n_malloc(sizeof{type unary})
		as type unary*;
	u->which.alloc_info = ai;
	u->kind = k;
	u->metadata = m;
	u->value_kind = value_kind::INVALID;
	u->typ = NULL as type typ*;
	u->is_constant = false;
	return u;
}

func type unary* unary_delete_init(type exp* e, type metadata* m) {
	type unary* u = n_malloc(sizeof{type unary})
		as type unary*;
	u->which.delete_exp = e;
	u->kind = unary_kind::DELETE;
	u->metadata = m;
	u->value_kind = value_kind::INVALID;
	u->typ = NULL as type typ*;
	u->is_constant = false;
	return u;
}

func void unary_delete(type unary* u) {
	switch (u->kind) {
	case unary_kind::INCREMENT:
	case unary_kind::DECREMENT:
	case unary_kind::PLUS:
	case unary_kind::MINUS:
	case unary_kind::NOT:
	case unary_kind::CMPL:
		exp_delete(u->which.base);
		break;
	case unary_kind::ALIGNOF_TYP:
	case unary_kind::SIZEOF_TYP:
		typ_delete(u->which.of_typ);
		break;
	case unary_kind::ALIGNOF_EXP:
	case unary_kind::SIZEOF_EXP:
		exp_delete(u->which.of_exp);
		break;
	case unary_kind::NEW:
	case unary_kind::STK:
	case unary_kind::RESV:
		alloc_info_delete(u->which.alloc_info);
		break;
	case unary_kind::DELETE:
		exp_delete(u->which.delete_exp);
		break;
	default:
		util::ice("unary_delete",
			"Unrecognized unary_kind while free'ing!");
	}

	if (u->typ != NULL as type typ*)
		typ_delete(u->typ);
	if (u->metadata != NULL as type metadata*)
		metadata_delete(u->metadata);
	n_free(u as byte*);
}

func type binary* binary_init(type exp* l, type lex::token* ot,
	unsigned int o, type exp* r, type metadata* m) {
	type binary* b = n_malloc(sizeof{type binary})
		as type binary*;
	b->lhs = l;
	b->op = o;
	b->rhs = r;
	b->op_token = ot;
	b->typ = NULL as type typ*;
	b->metadata = m;
	b->value_kind = value_kind::INVALID;
	b->is_constant = false;
	return b;
}

func void binary_delete(type binary* b) {
	exp_delete(b->lhs);
	exp_delete(b->rhs);
	if (b->typ != NULL as type typ*)
		typ_delete(b->typ);
	if (b->metadata != NULL as type metadata*)
		metadata_delete(b->metadata);
	n_free(b as byte*);
}

func type ternary* ternary_init(type exp* c, type exp* l, type exp* r,
	type metadata* m) {
	type ternary* t = n_malloc(sizeof{type ternary})
		as type ternary*;
	t->cond = c;
	t->lhs = l;
	t->rhs = r;
	t->typ = NULL as type typ*;
	t->metadata = m;
	t->value_kind = value_kind::INVALID;
	t->is_constant = false;
	return t;
}

func void ternary_delete(type ternary* t) {
	exp_delete(t->cond);
	exp_delete(t->lhs);
	exp_delete(t->rhs);
	if (t->typ != NULL as type typ*)
		typ_delete(t->typ);
	if (t->metadata != NULL as type metadata*)
		metadata_delete(t->metadata);
	n_free(t as byte*);
}

func void print_qualified_identifier(type qualified_identifier* qia,
	unsigned int t) {
	type util::vector* qi = qia->name;
	printf("[");
	for (unsigned int i = 0; i < util::vector_size(qi); i++) {
		if (i != 0) {
			for (unsigned int j = 0; j < t + 1; j++)
				printf(" ");
		}
		type lex::token* tok = util::vector_at(qi, i)
				as type lex::token** @;
		if (tok == NULL as type lex::token*)
			printf("<GLOBAL>");
		else
			lex::token_print(tok);
		if (i != util::vector_size(qi) - 1)
			printf(",\n");
	}
	printf("]");
}

func[static] void exp_primary_print_helper(type primary* p, unsigned int t) {
	switch (p->kind) {
	case primary_kind::LITERAL:
		printf("EXP-PRIMARY-LITERAL: ");
		lex::token_print(p->which.literal);
		break;
	case primary_kind::QUALIFIED_IDENTIFIER:
		printf("EXP-PRIMARY-QUALIFIED-IDENTIFIER: ");
		print_qualified_identifier(p->which.qualified_identifier, t);
		break;
	case primary_kind::PARENTHESIZED:
		printf("EXP-PRIMARY-PARENTHESIZED:\n");
		exp_print(p->which.parenthesized, t + 1);
		break;
	case primary_kind::ARRAY: {
		printf("EXP-PRIMARY-ARRAY:\n");
		type util::vector* a = p->which.array;
		for (unsigned int i = 0; i < util::vector_size(a); i++) {
			type ast::exp* elem = util::vector_at(a, i)
				as type ast::exp** @;
			exp_print(elem, t + 1);

			if (i != util::vector_size(a) - 1)
				printf("\n");
		}
	}
		break;
	case primary_kind::STMT:
		printf("EXP-PRIMARY-STMT:\n");
		stmt_print(p->which.stmt, t + 1);
		break;
	case primary_kind::TUPLE: {
		printf("EXP-PRIMARY-TUPLE:");

		type util::vector* tu = p->which.tuple;
		if (!util::vector_empty(tu))
			printf("\n");

		for (unsigned int i = 0; i < util::vector_size(tu); i++) {
			type ast::exp* e = util::vector_at(tu, i)
				as type ast::exp** @;
			exp_print(e, t + 1);

			if (i != util::vector_size(tu) - 1)
				printf("\n");
		}
	}
		break;
	case primary_kind::STRUCT_INIT: {
		printf("EXP-STRUCT-INIT:\n");

		type struct_init* si = p->which.struct_init;

		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		print_qualified_identifier(si->qualified_identifier, t + 1), printf("\n");

		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		printf("[");

		if (!util::vector_empty(si->member_inits))
			printf("\n");
		for (unsigned int i = 0; i < util::vector_size(si->member_inits); i++) {
			type member_init* mi = util::vector_at(si->member_inits, i)
				as type member_init** @;

			for (unsigned int i = 0; i < t + 2; i++)
				printf(" ");
			printf("[");

			lex::token_print(mi->ident), printf("\n");
			exp_print(mi->init, t + 3), printf("]");

			if (i != util::vector_size(si->member_inits))
				printf(",\n");
		}

		printf("]");
	}
		break;
	case primary_kind::LAMBDA: {
		printf("EXP-PRIMARY-LAMBDA:\n");
		function_print(p->which.lambda, t + 1);
	}
		break;
	default:
		util::ice("exp_primary_print_helper",
			"Unrecognized primary_kind while print'ing!");
	}
}

func[static] void exp_postfix_print_helper(type postfix* p, unsigned int t) {
	switch (p->kind) {
	case postfix_kind::INDEX: {
		printf("EXP-POSTFIX-INDEX:\n");

		exp_print(p->base, t + 1);
		printf("\n");
		exp_print(p->which.index, t + 1);
	}
		break;
	case postfix_kind::INCREMENT: {
		printf("EXP-POSTFIX-INCREMENT:\n");
		exp_print(p->base, t + 1);
	}
		break;
	case postfix_kind::DECREMENT: {
		printf("EXP-POSTFIX-DECREMENT:\n");
		exp_print(p->base, t + 1);
	}
		break;
	case postfix_kind::AT: {
		printf("EXP-POSTFIX-AT:\n");
		exp_print(p->base, t + 1);
	}
		break;
	case postfix_kind::ADDRESS: {
		printf("EXP-POSTFIX-ADDRESS:\n");
		exp_print(p->base, t + 1);
	}
		break;
	case postfix_kind::DOT_INDEX:
	case postfix_kind::ARROW_INDEX:
	case postfix_kind::ARROW:
	case postfix_kind::DOT: {
		if (p->kind == ast::postfix_kind::DOT)
			printf("EXP-POSTFIX-DOT:\n");
		else if (p->kind == ast::postfix_kind::DOT_INDEX)
			printf("EXP-POSTFIX-DOT-INDEX:\n");
		else if (p->kind == ast::postfix_kind::ARROW_INDEX)
			printf("EXP-POSTFIX-ARROW-INDEX:\n");
		else
			printf("EXP-POSTFIX-ARROW:\n");

		exp_print(p->base, t + 1), printf("\n");

		type member_data* mb = p->which.member_data;

		if (mb->is_ufcs) { 
			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("<UFCS>\n");
		}

		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		lex::token_print(p->which.member_data->member);
	}
		break;
	case postfix_kind::AS: {
		printf("EXP-POSTFIX-AS:\n");
		exp_print(p->base, t + 1), printf("\n");
		typ_print(p->which.as_typ, t + 1);
	}
		break;
	case postfix_kind::FUNCTION_CALL: {
		printf("EXP-POSTFIX-FUNCTION-CALL:\n");
		exp_print(p->base, t + 1), printf("\n");

		type util::vector* args = p->which.arguments;
		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		if (util::vector_empty(args))
			printf("[]");
		else {
			printf("[\n");

			for (unsigned int i = 0; i < util::vector_size(args); i++) {
				type ast::exp* arg = util::vector_at(args, i)
					as type ast::exp** @;

				exp_print(arg, t + 2);
				if (i != util::vector_size(args) - 1)
					printf(",\n");
			}

			printf("]");
		}
	}
		break;
	default:
		util::ice("exp_postfix_print_helper",
			"Unrecognized postfix_kind while print'ing.");
	}
}

func[static] void exp_unary_print_helper(type unary* u, unsigned int t) {
	switch (u->kind) {
	case unary_kind::INCREMENT:
		printf("EXP-UNARY-INCREMENT:\n");
		exp_print(u->which.base, t + 1);
		break;
	case unary_kind::DECREMENT:
		printf("EXP-UNARY-DECREMENT:\n");
		exp_print(u->which.base, t + 1);
		break;
	case unary_kind::PLUS:
		printf("EXP-UNARY-PLUS:\n");
		exp_print(u->which.base, t + 1);
		break;
	case unary_kind::MINUS:
		printf("EXP-UNARY-MINUS:\n");
		exp_print(u->which.base, t + 1);
		break;
	case unary_kind::NOT:
		printf("EXP-UNARY-NOT:\n");
		exp_print(u->which.base, t + 1);
		break;
	case unary_kind::CMPL:
		printf("EXP-UNARY-CMPL:\n");
		exp_print(u->which.base, t + 1);
		break;
	case unary_kind::SIZEOF_EXP:
		printf("EXP-UNARY-SIZEOF-EXP:\n");
		exp_print(u->which.of_exp, t + 1);
		break;
	case unary_kind::SIZEOF_TYP:
		printf("EXP-UNARY-SIZEOF-TYP:\n");
		typ_print(u->which.of_typ, t + 1);
		break;
	case unary_kind::ALIGNOF_EXP:
		printf("EXP-UNARY-ALIGNOF-EXP:\n");
		exp_print(u->which.of_exp, t + 1);
		break;
	case unary_kind::ALIGNOF_TYP:
		printf("EXP-UNARY-ALIGNOF-TYP:\n");
		typ_print(u->which.of_typ, t + 1);
		break;
	case unary_kind::NEW:
	case unary_kind::STK:
	case unary_kind::RESV: {
		if (u->kind == unary_kind::NEW)
			printf("EXP-UNARY-NEW:\n");
		else if (u->kind == unary_kind::RESV)
			printf("EXP-UNARY-RESV:\n");
		else
			printf("EXP-UNARY-STK:\n");

		typ_print(u->which.alloc_info->typ, t + 1);
		if (u->which.alloc_info->exp != NULL as type exp*) {
			printf("\n");
			exp_print(u->which.alloc_info->exp, t + 1);
		}
	}
		break;
	case unary_kind::DELETE: {
		printf("EXP-UNARY-DELETE:\n");
		exp_print(u->which.delete_exp, t + 1);
	}
		break;
	default:
		util::ice("exp_unary_print_helper",
			"Unrecognized unary_kind while print'ing.");
	}
}

func void exp_binary_print_helper(type binary* b, unsigned int t) {
	switch (b->op) {
	case binary_op_kind::MULT:
		printf("EXP-BINARY-MULT:\n");
		break;
	case binary_op_kind::DIV:
		printf("EXP-BINARY-DIV:\n");
		break;
	case binary_op_kind::MOD:
		printf("EXP-BINARY-MOD:\n");
		break;
	case binary_op_kind::ADD:
		printf("EXP-BINARY-ADD:\n");
		break;
	case binary_op_kind::SUB:
		printf("EXP-BINARY-SUB:\n");
		break;
	case binary_op_kind::SHL:
		printf("EXP-BINARY-SHL:\n");
		break;
	case binary_op_kind::SHR:
		printf("EXP-BINARY-SHR:\n");
		break;
	case binary_op_kind::GT:
		printf("EXP-BINARY-GT:\n");
		break;
	case binary_op_kind::LT:
		printf("EXP-BINARY-LT:\n");
		break;
	case binary_op_kind::GTE:
		printf("EXP-BINARY-GTE:\n");
		break;
	case binary_op_kind::LTE:
		printf("EXP-BINARY-LTE:\n");
		break;
	case binary_op_kind::EQ_EQ:
		printf("EXP-BINARY-EQ-EQ:\n");
		break;
	case binary_op_kind::NE:
		printf("EXP-BINARY-NE:\n");
		break;
	case binary_op_kind::BAND:
		printf("EXP-BINARY-BITWISE-AND:\n");
		break;
	case binary_op_kind::BOR:
		printf("EXP-BINARY-BITWISE-OR:\n");
		break;
	case binary_op_kind::BXOR:
		printf("EXP-BINARY-BITWISE-XOR:\n");
		break;
	case binary_op_kind::LAND:
		printf("EXP-BINARY-LOGICAL-AND:\n");
		break;
	case binary_op_kind::LOR:
		printf("EXP-BINARY-LOGICAL-OR:\n");
		break;
	case binary_op_kind::EQ:
		printf("EXP-BINARY-EQ:\n");
		break;
	case binary_op_kind::MULT_EQ:
		printf("EXP-BINARY-MULT-EQ:\n");
		break;
	case binary_op_kind::DIV_EQ:
		printf("EXP-BINARY-DIV-EQ:\n");
		break;
	case binary_op_kind::MOD_EQ:
		printf("EXP-BINARY-MOD-EQ:\n");
		break;
	case binary_op_kind::ADD_EQ:
		printf("EXP-BINARY-ADD-EQ:\n");
		break;
	case binary_op_kind::SUB_EQ:
		printf("EXP-BINARY-SUB-EQ:\n");
		break;
	case binary_op_kind::SHL_EQ:
		printf("EXP-BINARY-SHL-EQ:\n");
		break;
	case binary_op_kind::SHR_EQ:
		printf("EXP-BINARY-SHR-EQ:\n");
		break;
	case binary_op_kind::BAND_EQ:
		printf("EXP-BINARY-BAND-EQ:\n");
		break;
	case binary_op_kind::BXOR_EQ:
		printf("EXP-BINARY-BXOR-EQ:\n");
		break;
	case binary_op_kind::BOR_EQ:
		printf("EXP-BINARY-BOR-EQ:\n");
		break;
	default:
		util::ice("exp_binary_print_helper",
			"Unrecognized binary_op_kind while print'ing.");
	}
	exp_print(b->lhs, t + 1), printf("\n");
	exp_print(b->rhs, t + 1);
}

func void exp_ternary_print_helper(type ternary* te, unsigned int t) {
	printf("EXP-TERNARY:\n");
	exp_print(te->cond, t + 1), printf("\n");
	exp_print(te->lhs, t + 1), printf("\n");
	exp_print(te->rhs, t + 1);
}

func void exp_print(type exp* e, unsigned int t) {
	for (unsigned int i = 0; i < t; i++)
		printf(" ");
	printf("(");

	switch (e->kind) {
	case exp_kind::PRIMARY: {
		type ast::primary* p = e->which.primary;
		exp_primary_print_helper(p, t);
	}
		break;
	case exp_kind::POSTFIX: {
		type postfix* p = e->which.postfix;
		exp_postfix_print_helper(p, t);
	}
		break;
	case exp_kind::UNARY: {
		type unary* u = e->which.unary;
		exp_unary_print_helper(u, t);
	}
		break;
	case exp_kind::BINARY: {
		type binary* b = e->which.binary;
		exp_binary_print_helper(b, t);
	}
		break;
	case exp_kind::TERNARY: {
		type ternary* te = e->which.ternary;
		exp_ternary_print_helper(te, t);
	}
		break;
	case exp_kind::PAT_ASSIGN: {
		type pat_assign* pa = e->which.pat_assign;

		printf("EXP-PAT-ASSIGN:\n");
		pat_print(pa->lhs, t + 1), printf("\n");
		exp_print(pa->rhs, t + 1);
	}
		break;
	default:
		util::ice("exp_print",
			"Unrecognized exp_kind while printing!");
	}

	printf(")");
}

} } // namespace neutrino::ast
