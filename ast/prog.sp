import "ast/ast.hsp"

import <"std/lib">
import <"std/io">

import "util/vector.hsp"
import "util/base.hsp"
import "util/error.hsp"
import "lex/token.hsp"

using neutrino::util::n_malloc;
using neutrino::util::n_free;
using std::lib::NULL;
using std::io::printf;

namespace neutrino { namespace ast {

func type capture_data* capture_data_init(bool im, bool ip, bool ipm,
	type lex::token* i, type metadata* m) {
	type capture_data* cd = n_malloc(sizeof{type capture_data})
		as type capture_data*;
	cd->is_mut = im;
	cd->is_pointer = ip;
	cd->is_pointee_mut = ipm;
	cd->ident = i;
	cd->metadata = m;
	cd->typ = NULL as type typ*;
	cd->symtab_value = NULL as type tck::symtab_value*;
	return cd;
}

func void capture_data_delete(type capture_data* cd) {
	if (cd->metadata != NULL as type metadata*)
		metadata_delete(cd->metadata);
	if (cd->typ != NULL as type typ*)
		typ_delete(cd->typ);
	n_free(cd as byte*);
}

func type function* function_init(type attribute* attr, type lex::token* n,
	type util::vector* pars, bool iv, type typ* rt, type util::vector* cl,
	type stmt* b, type metadata* m) {
	type function* f = n_malloc(sizeof{type function})
		as type function*;
	f->attribute = attr;
	f->name = n;
	f->parameters = pars;
	f->is_variadic = iv;
	f->return_typ = rt;
	f->capture_list = cl;
	f->body = b;
	f->metadata = m;
	f->ctx = NULL as type tck::symtab*;
	f->symtab_value = NULL as type tck::symtab_value*;
	return f;
}

func void function_delete(type function* f) {
	if (f->attribute != NULL as type attribute*)
		attribute_delete(f->attribute);
	util::vector_delete(f->parameters);
	typ_delete(f->return_typ);
	if (f->capture_list != NULL as type util::vector*)
		util::vector_delete(f->capture_list);
	if (f->body != NULL as type stmt*)
		stmt_delete(f->body);
	if (f->metadata != NULL as type metadata*)
		metadata_delete(f->metadata);

	n_free(f as byte*);
}

func type namespace_decls* namespace_decls_init(
	type attribute* attr,
	type qualified_identifier* n,
	type util::vector* ds, type metadata* m) {
	type namespace_decls* nd = n_malloc(sizeof{type namespace_decls})
		as type namespace_decls*;
	nd->attribute = attr;
	nd->name = n;
	nd->decls = ds;
	nd->metadata = m;
	nd->ctx = NULL as type tck::symtab*;
	nd->symtab_value = NULL as type tck::symtab_value*;
	return nd;
}

func void namespace_decls_delete(type namespace_decls* nd) {
	if (nd->attribute != NULL as type attribute*)
		attribute_delete(nd->attribute);
	qualified_identifier_delete(nd->name);
	if (nd->decls != NULL as type util::vector*)
		util::vector_delete(nd->decls);
	if (nd->metadata != NULL as type metadata*)
		metadata_delete(nd->metadata);
	n_free(nd as byte*);
}

func type top_level* top_level_fun_init(type function* f) {
	type top_level* t = n_malloc(sizeof{type top_level})
		as type top_level*;
	t->kind = top_level_kind::FUN;
	t->which.function = f;
	t->metadata = metadata_copy(f->metadata);
	return t;
}

func type top_level* top_level_stmt_init(type stmt* s) {
	type top_level* t = n_malloc(sizeof{type top_level})
		as type top_level*;
	switch (s->kind) {
	case stmt_kind::USING:
		t->kind = top_level_kind::USING;
		break;
	case stmt_kind::USING_NAMESPACE:
		t->kind = top_level_kind::USING_NAMESPACE;
		break;
	case stmt_kind::DECL:
		t->kind = top_level_kind::DECL;
		break;
	case stmt_kind::AGGREGATE:
		t->kind = top_level_kind::AGGREGATE;
		break;
	case stmt_kind::NAMESPACE_ALIAS:
		t->kind = top_level_kind::NAMESPACE_ALIAS;
		break;
	case stmt_kind::INCLUDE:
		t->kind = top_level_kind::INCLUDE;
		break;
	case stmt_kind::TYPE_ALIAS:
		t->kind = top_level_kind::TYPE_ALIAS;
		break;
	default:
		util::ice("top_level_stmt_init",
			"Invalid statement for top level.");
	}
	t->which.stmt = s;
	t->metadata = metadata_copy(s->metadata);
	return t;
}

func type top_level* top_level_namespace_init(type namespace_decls* nd) {
	type top_level* t = n_malloc(sizeof{type top_level})
		as type top_level*;
	t->kind = top_level_kind::NAMESPACE;
	t->which.namespace_decls = nd;
	t->metadata = metadata_copy(nd->metadata);
	return t;
}

func type top_level* top_level_empty_init(type metadata* m) {
	type top_level* t = n_malloc(sizeof{type top_level})
		as type top_level*;
	t->kind = top_level_kind::EMPTY;
	t->metadata = m;
	return t;
}

func void top_level_delete(type top_level* t) {
	switch (t->kind) {
	case top_level_kind::FUN:
		function_delete(t->which.function);
		break;
	case top_level_kind::AGGREGATE:
	case top_level_kind::USING:
	case top_level_kind::USING_NAMESPACE:
	case top_level_kind::DECL:
	case top_level_kind::NAMESPACE_ALIAS:
	case top_level_kind::INCLUDE:
	case top_level_kind::TYPE_ALIAS:
		stmt_delete(t->which.stmt);
		break;
	case top_level_kind::NAMESPACE:
		namespace_decls_delete(t->which.namespace_decls);
		break;
	case top_level_kind::EMPTY:
		break;
	default:
		util::ice("top_level_delete",
			"Unrecognized top_level_kind while free'ing.");
	}

	if (t->metadata != NULL as type metadata*)
		metadata_delete(t->metadata);
	n_free(t as byte*);
}

func type prog* prog_init(type util::vector* tls, type metadata* m) {
	type prog* p = n_malloc(sizeof{type prog})
		as type prog*;
	p->top_levels = tls;
	p->metadata = m;
	p->ctx = NULL as type tck::symtab*;
	return p;
}

func void prog_delete(type prog* p) {
	util::vector_delete(p->top_levels);
	if (p->metadata != NULL as type metadata*)
		metadata_delete(p->metadata);
	n_free(p as byte*);
}

func void function_print(type function* f, unsigned int t) {
	for (unsigned int i = 0; i < t; i++)
		printf(" ");
	printf("(");

	bool is_fwd = f->body == NULL as type stmt*;

	if (is_fwd)
		printf("FUN-DECL:\n");
	else
		printf("FUN:\n");

	if (f->attribute != NULL as type attribute*)
		attribute_print(f->attribute, t + 1), printf("\n");

	for (unsigned int i = 0; i < t + 1; i++)
		printf(" ");
	if (f->name != NULL as type lex::token*)
		lex::token_print(f->name), printf("\n");
	else
		printf("<ANON>\n");

	for (unsigned int i = 0; i < t + 1; i++)
		printf(" ");
	if (util::vector_empty(f->parameters)) {
		printf("[]\n");
	}
	else {
		printf("[\n");
		for (unsigned int i = 0; i < util::vector_size(f->parameters); i++) {
			type ast::decl* d = util::vector_at(f->parameters, i)
				as type ast::decl** @;
			decl_print(d, t + 2);
			if (i != util::vector_size(f->parameters) - 1)
				printf(",");
			else {
				if (f->is_variadic) {
					printf(",\n");
					for (unsigned int i = 0; i < t + 2; i++)
						printf(" ");
					printf("<VARIADIC>");
				}
			}
			printf("\n");
		}
		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		printf("]\n");
	}

	typ_print(f->return_typ, t + 1);
	if (!is_fwd)
		printf("\n");

	if (f->capture_list != NULL as type util::vector*) {
		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		printf("[");
		
		if (!util::vector_empty(f->capture_list)) {
			printf("\n");
			for (unsigned int i = 0; i < util::vector_size(f->capture_list); i++) {
				type ast::capture_data* cd = util::vector_at(f->capture_list, i)
					as type ast::capture_data** @;

				for (unsigned int j = 0; j < t + 2; j++)
					printf(" ");
				printf("[");
				if (cd->is_mut)
					printf("MUT ");

				if (cd->is_pointer) {
					printf("* ");
					if (cd->is_pointee_mut)
						printf("MUT ");
				}

				lex::token_print(cd->ident);
				printf("]");
				if (i != util::vector_size(f->capture_list) - 1)
					printf(",\n");
			}
		}
		printf("]\n");
	}
	else {
		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		printf("<NO-CAPTURE-LIST>\n");
	}

	if (!is_fwd)
		stmt_print(f->body, t + 1);
	printf(")");
}

func void top_level_print(type top_level* tl, unsigned int t) {
	for (unsigned int i = 0; i < t; i++)
		printf(" ");
	printf("(");
	switch (tl->kind) {
	case top_level_kind::INCLUDE:
	case top_level_kind::NAMESPACE_ALIAS:
	case top_level_kind::AGGREGATE:
	case top_level_kind::USING:
	case top_level_kind::USING_NAMESPACE:
	case top_level_kind::TYPE_ALIAS:
	case top_level_kind::DECL: {
		switch (tl->kind) {
		case top_level_kind::USING:
			printf("TOP-LEVEL-USING:\n");
			break;
		case top_level_kind::USING_NAMESPACE:
			printf("TOP-LEVEL-USING-NAMESPACE:\n");
			break;
		case top_level_kind::DECL:
			printf("TOP-LEVEL-DECL:\n");
			break;
		case top_level_kind::AGGREGATE:
			printf("TOP-LEVEL-AGGREGATE:\n");
			break;
		case top_level_kind::NAMESPACE_ALIAS:
			printf("TOP-LEVEL-NAMESPACE-ALIAS:\n");
			break;
		case top_level_kind::INCLUDE:
			printf("TOP-LEVEL-INCLUDE:\n");
			break;
		case top_level_kind::TYPE_ALIAS:
			printf("TOP-LEVEL-TYPE-ALIAS:\n");
			break;
		default:
			util::ice("print_top_level",
				"Unrecognized top_level_kind for a stmt while printing!");
			
		}
		stmt_print(tl->which.stmt, t + 1);
	}
		break;
	case top_level_kind::EMPTY:
		printf("TOP-LEVEL-EMPTY:");
		break;
	case top_level_kind::FUN: {
		printf("TOP-LEVEL-FUN:\n");
		function_print(tl->which.function, t + 1);
	}
		break;
	case top_level_kind::NAMESPACE: {
		type namespace_decls* nd = tl->which.namespace_decls;

		if (nd->decls == NULL as type util::vector*)
			printf("TOP-LEVEL-NAMESPACE-DECL:\n");
		else
			printf("TOP-LEVEL-NAMESPACE:\n");

		if (nd->attribute != NULL as type attribute*)
			attribute_print(nd->attribute, t + 1), printf("\n");

		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		print_qualified_identifier(nd->name, t + 1);

		if (nd->decls != NULL as type util::vector*) {
			printf("\n");
			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("[");
			if (!util::vector_empty(nd->decls))
				printf("\n");
			for (unsigned int i = 0; i < util::vector_size(nd->decls); i++) {
				type ast::top_level* tl = util::vector_at(nd->decls, i)
					as type ast::top_level** @;

				top_level_print(tl, t + 2);
				if (i != util::vector_size(nd->decls) - 1)
					printf(",\n");
			}
			printf("]");
		}
	}
		break;
	default:
		util::ice("top_level_print",
			"Unrecognized top_level_kind while printing!");
	}
	printf(")");
}

func void prog_print(type prog* p, unsigned int t) {
	for (unsigned int i = 0; i < t; i++)
		printf(" ");
	printf("(PROG:");
	if (!util::vector_empty(p->top_levels))
		printf("\n");

	for (unsigned int i = 0; i < util::vector_size(p->top_levels); i++) {
		type top_level* tl = util::vector_at(p->top_levels, i)
			as type top_level** @;

		top_level_print(tl, t + 1);
		if (i != util::vector_size(p->top_levels) - 1)
			printf("\n");
	}

	printf(")");
}

} } // namespace neutrino::ast
