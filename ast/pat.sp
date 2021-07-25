import "ast/ast.hsp"

import <"std/io">
import <"std/lib">

import "util/vector.hsp"
import "lex/token.hsp"
import "util/error.hsp"
import "util/base.hsp"

using std::io::printf;
using std::lib::NULL;
using neutrino::util::n_malloc;
using neutrino::util::n_free;

namespace neutrino { namespace ast {

func type ident_pat* ident_pat_init(bool im, type lex::token* i,
	type typ* tt, type metadata* m) {
	type ident_pat* ip = n_malloc(sizeof{type ident_pat})
		as type ident_pat*;
	ip->is_mut = im;
	ip->ident = i;
	ip->metadata = m;
	ip->to_typ = tt;
	ip->symtab_value = NULL as type tck::symtab_value*;
	ip->typ = NULL as type typ*;
	return ip;
}

func void ident_pat_delete(type ident_pat* ip) {
	if (ip->to_typ != NULL as type typ*)
		typ_delete(ip->to_typ);
	if (ip->metadata != NULL as type metadata*)
		metadata_delete(ip->metadata);
	if (ip->typ != NULL as type typ*)
		typ_delete(ip->typ);
	n_free(ip as byte*);
}

func type tuple_pat* tuple_pat_init(unsigned int ik, type util::vector* f,
	type util::vector* l, type metadata* m) {
	type tuple_pat* tp = n_malloc(sizeof{type tuple_pat})
		as type tuple_pat*;
	tp->ignore_kind = ik;
	tp->first = f;
	tp->last = l;
	tp->metadata = m;
	tp->first_typs = NULL as type util::vector*;
	tp->last_typs = NULL as type util::vector*;
	return tp;
}

func void tuple_pat_delete(type tuple_pat* tp) {
	if (tp->first != NULL as type util::vector*)
		util::vector_delete(tp->first);
	if (tp->last != NULL as type util::vector*)
		util::vector_delete(tp->last);
	if (tp->metadata != NULL as type metadata*)
		metadata_delete(tp->metadata);
	if (tp->first_typs != NULL as type util::vector*)
		util::vector_delete(tp->first_typs);
	if (tp->last_typs != NULL as type util::vector*)
		util::vector_delete(tp->last_typs);
	n_free(tp as byte*);
}

func type range_bound* range_bound_literal_init(type lex::token* l, bool i,
	type metadata* m) {
	type range_bound* rb = n_malloc(sizeof{type range_bound})
		as type range_bound*;
	rb->which.literal = l;
	rb->inclusive = i;
	rb->kind = ast::range_bound_kind::LITERAL;
	rb->metadata = m;
	rb->typ = NULL as type typ*;
	return rb;
}

func type range_bound* range_bound_qualified_identifier_init(type qualified_identifier* qi,
	bool i, type metadata* m) {
	type range_bound* rb = n_malloc(sizeof{type range_bound})
		as type range_bound*;
	rb->which.qualified_identifier = qi;
	rb->kind = ast::range_bound_kind::QUALIFIED_IDENTIFIER;
	rb->inclusive = i;
	rb->metadata = m;
	rb->typ = NULL as type typ*;
	return rb;
}

func void range_bound_delete(type range_bound* rb) {
	switch (rb->kind) {
	case ast::range_bound_kind::LITERAL:
		break;
	case ast::range_bound_kind::QUALIFIED_IDENTIFIER:
		qualified_identifier_delete(rb->which.qualified_identifier);
		break;
	default:
		util::ice("range_bound_delete",
			"Unrecognized range_bound_kind while free'ing!");
	}

	if (rb->metadata != NULL as type metadata*)
		metadata_delete(rb->metadata);
	if (rb->typ != NULL as type typ*)
		typ_delete(rb->typ);
	n_free(rb as byte*);
}

func type range_pat* range_pat_init(type range_bound* srb, type range_bound* erb,
	type metadata* m) {
	type range_pat* rp = n_malloc(sizeof{type range_pat})
		as type range_pat*;
	rp->start = srb;
	rp->end = erb;
	rp->metadata = m;
	return rp;
}

func void range_pat_delete(type range_pat* rp) {
	if (rp->start != NULL as type range_bound*)
		range_bound_delete(rp->start);
	if (rp->end != NULL as type range_bound*)
		range_bound_delete(rp->end);
	if (rp->metadata != NULL as type metadata*)
		metadata_delete(rp->metadata);
	n_free(rp as byte*);
}

func type constructor_pat* constructor_pat_init(type qualified_identifier* qi,
	type util::vector* n, type metadata* m) {
	type constructor_pat* cp = n_malloc(sizeof{type constructor_pat})
		as type constructor_pat*;
	cp->constructor = qi;
	cp->nested = n;
	cp->metadata = m;
	cp->typ = NULL as type typ*;
	return cp;
}

func void constructor_pat_delete(type constructor_pat* cp) {
	qualified_identifier_delete(cp->constructor);
	if (cp->nested != NULL as type util::vector*)
		util::vector_delete(cp->nested);
	if (cp->metadata != NULL as type metadata*)
		metadata_delete(cp->metadata);
	if (cp->typ != NULL as type typ*)
		typ_delete(cp->typ);
	n_free(cp as byte*);
}

func type member_pat* member_pat_init(bool im, type lex::token* m, type pat* n,
	type metadata* md) {
	type member_pat* mp = n_malloc(sizeof{type member_pat})
		as type member_pat*;
	mp->is_mut = im;
	mp->member = m;
	mp->nested = n;
	mp->metadata = md;
	mp->symtab_value = NULL as type tck::symtab_value*;
	mp->typ = NULL as type typ*;
	return mp;
}

func void member_pat_delete(type member_pat* mp) {
	if (mp->nested != NULL as type pat*)
		pat_delete(mp->nested);
	if (mp->metadata != NULL as type metadata*)
		metadata_delete(mp->metadata);
	if (mp->typ != NULL as type typ*)
		typ_delete(mp->typ);
	n_free(mp as byte*);
}

func type struct_pat* struct_pat_init(type qualified_identifier* qi,
	type util::vector* mps, type metadata* m) {
	type struct_pat* sp = n_malloc(sizeof{type struct_pat})
		as type struct_pat*;
	sp->qualified_identifier = qi;
	sp->member_pats = mps;
	sp->metadata = m;
	sp->typ = NULL as type typ*;
	return sp;
}

func void struct_pat_delete(type struct_pat* sp) {
	qualified_identifier_delete(sp->qualified_identifier);
	util::vector_delete(sp->member_pats);
	if (sp->metadata != NULL as type metadata*)
		metadata_delete(sp->metadata);
	if (sp->typ != NULL as type typ*)
		typ_delete(sp->typ);
	n_free(sp as byte*);
}

func type pat* pat_literal_init(type lex::token* l, type metadata* m) {
	type pat* p = n_malloc(sizeof{type pat}) as type pat*;
	p->kind = ast::pat_kind::LITERAL;
	p->which.literal = l;
	p->metadata = m;
	p->typ = NULL as type typ*;
	return p;
}

func type pat* pat_ident_init(type ident_pat* ip) {
	type pat* p = n_malloc(sizeof{type pat}) as type pat*;
	p->kind = ast::pat_kind::IDENT;
	p->which.ident_pat = ip;
	p->metadata = metadata_copy(ip->metadata);
	p->typ = NULL as type typ*;
	return p;
}

func type pat* pat_nested_init(type pat* n, type metadata* m) {
	type pat* p = n_malloc(sizeof{type pat}) as type pat*;
	p->kind = ast::pat_kind::NESTED;
	p->which.nested = n;
	p->metadata = m;
	p->typ = NULL as type typ*;
	return p;
}

func type pat* pat_tuple_init(type tuple_pat* tp) {
	type pat* p = n_malloc(sizeof{type pat}) as type pat*;
	p->kind = ast::pat_kind::TUPLE;
	p->which.tuple_pat = tp;
	p->metadata = metadata_copy(tp->metadata);
	p->typ = NULL as type typ*;
	return p;
}

func type pat* pat_range_init(type range_pat* rp) {
	type pat* p = n_malloc(sizeof{type pat}) as type pat*;
	p->kind = ast::pat_kind::RANGE;
	p->which.range_pat = rp;
	p->metadata = metadata_copy(rp->metadata);
	p->typ = NULL as type typ*;
	return p;
}

func type pat* pat_wildcard_init(type metadata* m) {
	type pat* p = n_malloc(sizeof{type pat}) as type pat*;
	p->kind = ast::pat_kind::WILDCARD;
	p->metadata = m;
	p->typ = NULL as type typ*;
	return p;
}

func type pat* pat_exp_init(type exp* e, type metadata* m) {
	type pat* p = n_malloc(sizeof{type pat}) as type pat*;
	p->which.exp = e;
	p->kind = ast::pat_kind::EXP;
	p->metadata = m;
	p->typ = NULL as type typ*;
	return p;
}

func type pat* pat_constructor_init(type constructor_pat* cp) {
	type pat* p = n_malloc(sizeof{type pat}) as type pat*;
	p->kind = ast::pat_kind::CONSTRUCTOR;
	p->which.constructor_pat = cp;
	p->metadata = metadata_copy(cp->metadata);
	p->typ = NULL as type typ*;
	return p;
}

func type pat* pat_struct_init(type struct_pat* sp) {
	type pat* p = n_malloc(sizeof{type pat}) as type pat*;
	p->kind = ast::pat_kind::STRUCT;
	p->which.struct_pat = sp;
	p->metadata = metadata_copy(sp->metadata);
	p->typ = NULL as type typ*;
	return p;
}

func void pat_delete(type pat* p) {
	switch (p->kind) {
	case pat_kind::WILDCARD:
	case pat_kind::LITERAL:
		break;
	case pat_kind::IDENT:
		ident_pat_delete(p->which.ident_pat);
		break;
	case pat_kind::NESTED:
		pat_delete(p->which.nested);
		break;
	case pat_kind::TUPLE:
		tuple_pat_delete(p->which.tuple_pat);
		break;
	case pat_kind::RANGE:
		range_pat_delete(p->which.range_pat);
		break;
	case pat_kind::EXP:
		exp_delete(p->which.exp);
		break;
	case pat_kind::CONSTRUCTOR:
		constructor_pat_delete(p->which.constructor_pat);
		break;
	case pat_kind::STRUCT:
		struct_pat_delete(p->which.struct_pat);
		break;
	default:
		util::ice("pat_delete",
			"Unrecognized pat_kind while free'ing!");
	}

	if (p->metadata != NULL as type metadata*)
		metadata_delete(p->metadata);
	if (p->typ != NULL as type typ*)
		typ_delete(p->typ);
	n_free(p as byte*);
}

func void pat_print(type pat* p, unsigned int t) {
	for (unsigned int i = 0; i < t; i++)
		printf(" ");
	printf("(");
	switch (p->kind) {
	case ast::pat_kind::WILDCARD:
		printf("PAT-WILDCARD:");
		break;
	case ast::pat_kind::LITERAL: {
		printf("PAT-LITERAL:\n");
		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		lex::token_print(p->which.literal);
	}
		break;
	case ast::pat_kind::NESTED: {
		printf("PAT-NESTED:\n");
		pat_print(p->which.nested, t + 1);
	}
		break;
	case ast::pat_kind::TUPLE: {
		printf("PAT-TUPLE:\n");
		type tuple_pat* tp = p->which.tuple_pat;

		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		switch (tp->ignore_kind) {
		case tuple_ignore_kind::NONE:
			printf("<IGNORE-NONE>\n");
			break;
		case tuple_ignore_kind::BEGINNING:
			printf("<IGNORE-BEGINNING>\n");
			break;
		case tuple_ignore_kind::MIDDLE:
			printf("<IGNORE-MIDDLE>\n");
			break;
		case tuple_ignore_kind::END:
			printf("<IGNORE-END>\n");
			break;
		default:
			util::ice("pat_print",
				"Unrecognized tuple_ignore_kind while printing!");
		}

		if (tp->first != NULL as type util::vector*) {
			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("[");
			if (!util::vector_empty(tp->first))
				printf("\n");
			for (unsigned int i = 0; i < util::vector_size(tp->first); i++) {
				type pat* cp = util::vector_at(tp->first, i) as type pat** @;

				pat_print(cp, t + 2);
				if (i != util::vector_size(tp->first) - 1)
					printf(",\n");
			}
			printf("]");

			if (tp->last != NULL as type util::vector*)
				printf("\n");
		}

		if (tp->last != NULL as type util::vector*) {
			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("[");
			if (!util::vector_empty(tp->last))
				printf("\n");
			for (unsigned int i = 0; i < util::vector_size(tp->last); i++) {
				type pat* cp = util::vector_at(tp->last, i) as type pat** @;

				pat_print(cp, t + 2);
				if (i != util::vector_size(tp->last) - 1)
					printf(",\n");
			}
			printf("]");
		}
	}
		break;
	case ast::pat_kind::RANGE: {
		printf("PAT-RANGE:\n");

		type range_pat* rp = p->which.range_pat;
		if(rp->start != NULL as type range_bound*) {
			switch (rp->start->kind) {
			case ast::range_bound_kind::LITERAL: {
				for (unsigned int i = 0; i < t + 1; i++)
					printf(" ");
				lex::token_print(rp->start->which.literal);
			}
				break;
			case ast::range_bound_kind::QUALIFIED_IDENTIFIER: {
				for (unsigned int i = 0; i < t + 1; i++)
					printf(" ");
				print_qualified_identifier(rp->start->which.qualified_identifier, t + 1);
			}
				break;
			default:
				util::ice("pat_print",
					"Unrecognized range_bound_kind while printing range start!");
			}
			printf("\n");
			if (rp->start->inclusive) {
				for (unsigned int i = 0; i < t + 1; i++)
					printf(" ");
				printf("<START-INCLUSIVE>\n");
			}
		}
		else {
			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("<NO-START>\n");
		}

		if (rp->end != NULL as type range_bound*) {
			if (rp->end->inclusive) {
				for (unsigned int i = 0; i < t + 1; i++)
					printf(" ");
				printf("<END-INCLUSIVE>\n");
			}
			switch (rp->end->kind) {
			case ast::range_bound_kind::LITERAL: {
				for (unsigned int i = 0; i < t + 1; i++)
					printf(" ");
				lex::token_print(rp->end->which.literal);
			}
				break;
			case ast::range_bound_kind::QUALIFIED_IDENTIFIER: {
				for (unsigned int i = 0; i < t + 1; i++)
					printf(" ");
				print_qualified_identifier(rp->end->which.qualified_identifier, t + 1);
			}
				break;
			default:
				util::ice("pat_print",
					"Unrecognized range_bound_kind while printing range end!");
			}
		}
		else {
			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("<NO-END>");
		}
	}
		break;
	case ast::pat_kind::CONSTRUCTOR: {
		printf("PAT-CONSTRUCTOR:\n");

		type constructor_pat* cp = p->which.constructor_pat;
		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		print_qualified_identifier(cp->constructor, t + 1), printf("\n");

		if (cp->nested != NULL as type util::vector*) {
			for (unsigned int i = 0; i < util::vector_size(cp->nested); i++) {
				type ast::pat* curr_pat = util::vector_at(cp->nested, i)
					as type ast::pat** @;
				pat_print(curr_pat, t + 1);

				if (i != util::vector_size(cp->nested) - 1)
					printf("\n");
			}
		}
		else {
			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("<NONE>");
		}
	}
		break;
	case ast::pat_kind::STRUCT: {
		printf("PAT-STRUCT:\n");

		type struct_pat* sp = p->which.struct_pat;

		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		print_qualified_identifier(sp->qualified_identifier, t + 1), printf("\n");

		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		printf("[");
		if (!util::vector_empty(sp->member_pats))
			printf("\n");
		for (unsigned int i = 0; i < util::vector_size(sp->member_pats); i++) {
			type member_pat* mp = util::vector_at(sp->member_pats, i)
				as type member_pat** @;

			for (unsigned int j = 0; j < t + 2; j++)
				printf(" ");
			printf("[\n");
			if (mp->is_mut) {
				for (unsigned int j = 0; j < t + 3; j++)
					printf(" ");
				printf("<MUT>\n");
			}
			for (unsigned int j = 0; j < t + 3; j++)
				printf(" ");
			lex::token_print(mp->member);
			if (mp->nested != NULL as type pat*)
				printf("\n"), pat_print(mp->nested, t + 3);
			printf("]");

			if (i != util::vector_size(sp->member_pats) - 1)
				printf(",\n");
		}
		printf("]");
	}
		break;
	case ast::pat_kind::EXP: {
		printf("PAT-EXP:\n");
		exp_print(p->which.exp, t + 1);
	}
		break;
	case ast::pat_kind::IDENT: {
		printf("PAT-IDENT:\n");

		type ident_pat* ip = p->which.ident_pat;
		if (ip->is_mut) {
			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("<MUT>\n");
		}

		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		lex::token_print(ip->ident);

		if (ip->to_typ != NULL as type typ*)
			printf("\n"), typ_print(ip->to_typ, t + 1);
	}
		break;
	default:
		util::ice("pat_print",
			"Unrecognized pat_kind while printing!");
	}
	printf(")");
}

} } // namespace neutrino::ast
