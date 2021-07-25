import "ast/ast.hsp"

import <"std/lib">
import <"std/io">

import "util/base.hsp"
import "util/error.hsp"
import "util/vector.hsp"
import "util/hash_table.hsp"
import "lex/token.hsp"

using std::io::printf;
using std::lib::NULL;
using neutrino::util::n_malloc;
using neutrino::util::n_free;

namespace neutrino { namespace ast {

func type decl_component* decl_component_init(type pat* p,
	type typ* t, type exp* in, type metadata* m, bool is, bool ic) {
	type decl_component* dc = n_malloc(sizeof{type decl_component})
		as type decl_component*;
	dc->typ = t;
	dc->pat = p;
	dc->init = in;
	dc->metadata = m;
	dc->is_static = is;
	dc->is_const = ic;
	return dc;
}

func void decl_component_delete(type decl_component* dc) {
	pat_delete(dc->pat);
	if (dc->typ != NULL as type typ*)
		typ_delete(dc->typ);
	if (dc->init != NULL as type exp*)
		exp_delete(dc->init);
	if (dc->metadata != NULL as type metadata*)
		metadata_delete(dc->metadata);
	n_free(dc as byte*);
}

func type decl* decl_init(type attribute* a, bool is, bool ic,
	type util::vector* dcs, type metadata* m) {
	type decl* d = n_malloc(sizeof{type decl}) as type decl*;
	d->is_static = is;
	d->is_const = ic;
	d->attribute = a;
	d->decl_components = dcs;
	d->metadata = m;
	return d;
}

func void decl_delete(type decl* d) {
	if (d->attribute != NULL as type attribute*)
		attribute_delete(d->attribute);
	util::vector_delete(d->decl_components);
	if (d->metadata != NULL as type metadata*)
		metadata_delete(d->metadata);
	n_free(d as byte*);
}

func type labeled* labeled_case_init(type exp* c, type stmt* s,
	type metadata* m) {
	type labeled* l = n_malloc(sizeof{type labeled})
		as type labeled*;
	l->kind = labeled_kind::CASE;
	l->name = NULL as type lex::token*;
	l->exp = c;
	l->stmt = s;
	l->metadata = m;
	l->ctx = NULL as type tck::symtab*;
	l->symtab_value = NULL as type tck::symtab_value*;
	return l;
}

func type labeled* labeled_default_init(type stmt* s,
	type metadata* m) {
	type labeled* l = n_malloc(sizeof{type labeled})
		as type labeled*;
	l->kind = labeled_kind::DEFAULT;
	l->name = NULL as type lex::token*;
	l->exp = NULL as type exp*;
	l->stmt = s;
	l->metadata = m;
	l->ctx = NULL as type tck::symtab*;
	l->symtab_value = NULL as type tck::symtab_value*;
	return l;
}

func type labeled* labeled_label_init(type lex::token* n, type stmt* s,
	type metadata* m) {
	type labeled* l = n_malloc(sizeof{type labeled})
		as type labeled*;
	l->kind = labeled_kind::LABEL;
	l->name = n;
	l->exp = NULL as type exp*;
	l->stmt = s;
	l->metadata = m;
	l->ctx = NULL as type tck::symtab*;
	l->symtab_value = NULL as type tck::symtab_value*;
	return l;
}

func void labeled_delete(type labeled* l) {
	if (l->exp != NULL as type exp*)
		exp_delete(l->exp);
	if (l->stmt != NULL as type stmt*)
		stmt_delete(l->stmt);
	if (l->metadata != NULL as type metadata*)
		metadata_delete(l->metadata);

	n_free(l as byte*);
}

func type selection* selection_if_init(type decl* d,
	type exp* e, type stmt* st, type metadata* m) {
	type selection* s = n_malloc(sizeof{type selection})
		as type selection*;
	s->kind = selection_kind::IF;
	s->decl = d;
	s->exp = e;
	s->stmt1 = st;
	s->stmt2 = NULL as type stmt*;
	s->metadata = m;
	s->ctx1 = NULL as type tck::symtab*;
	s->ctx2 = NULL as type tck::symtab*;
	return s;
}

func type selection* selection_if_else_init(type decl* d,
	type exp* e, type stmt* s1, type stmt* s2, type metadata* m) {
	type selection* s = n_malloc(sizeof{type selection})
		as type selection*;
	s->kind = selection_kind::IF_ELSE;
	s->decl = d;
	s->exp = e;
	s->stmt1 = s1;
	s->stmt2 = s2;
	s->metadata = m;
	s->ctx1 = NULL as type tck::symtab*;
	s->ctx2 = NULL as type tck::symtab*;
	return s;
}

func type selection* selection_switch_init(type decl* d,
	type exp* e, type stmt* st, type metadata* m) {
	type selection* s = n_malloc(sizeof{type selection})
		as type selection*;
	s->kind = selection_kind::SWITCH;
	s->decl = d;
	s->exp = e;
	s->stmt1 = st;
	s->stmt2 = NULL as type stmt*;
	s->metadata = m;
	s->ctx1 = NULL as type tck::symtab*;
	s->ctx2 = NULL as type tck::symtab*;
	return s;
}

func void selection_delete(type selection* s) {
	if (s->decl != NULL as type decl*)
		decl_delete(s->decl);
	if (s->exp != NULL as type exp*)
		exp_delete(s->exp);
	stmt_delete(s->stmt1);
	if (s->stmt2 != NULL as type stmt*)
		stmt_delete(s->stmt2);
	if (s->metadata != NULL as type metadata*)
		metadata_delete(s->metadata);

	n_free(s as byte*);
}

func type iteration* iteration_while_init(type exp* e, type stmt* s,
	type metadata* m) {
	type iteration* i = n_malloc(sizeof{type iteration})
		as type iteration*;
	i->kind = iteration_kind::WHILE;
	i->exp1 = e;
	i->exp2 = i->exp3 = NULL as type exp*;
	i->decl = NULL as type decl*;
	i->stmt = s;
	i->metadata = m;
	i->ctx = NULL as type tck::symtab*;
	return i;
}

func type iteration* iteration_do_init(type stmt* s, type exp* e,
	type metadata* m) {
	type iteration* i = n_malloc(sizeof{type iteration})
		as type iteration*;
	i->kind = iteration_kind::DO;
	i->exp1 = e;
	i->exp2 = i->exp3 = NULL as type exp*;
	i->decl = NULL as type decl*;
	i->stmt = s;
	i->metadata = m;
	i->ctx = NULL as type tck::symtab*;
	return i;
}

func type iteration* iteration_for_exp_init(type exp* e1, type exp* e2,
	type exp* e3, type stmt* s, type metadata* m) {
	type iteration* i = n_malloc(sizeof{type iteration})
		as type iteration*;
	i->kind = iteration_kind::FOR_EXP;
	i->exp1 = e1;
	i->exp2 = e2;
	i->exp3 = e3;
	i->decl = NULL as type decl*;
	i->stmt = s;
	i->metadata = m;
	i->ctx = NULL as type tck::symtab*;
	return i;
}

func type iteration* iteration_for_decl_init(type decl* d, type exp* e1,
	type exp* e2, type stmt* s, type metadata* m) {
	type iteration* i = n_malloc(sizeof{type iteration})
		as type iteration*;
	i->kind = iteration_kind::FOR_DECL;
	i->exp1 = e1;
	i->exp2 = e2;
	i->exp3 = NULL as type exp*;
	i->decl = d;
	i->stmt = s;
	i->metadata = m;
	i->ctx = NULL as type tck::symtab*;
	return i;
}

func void iteration_delete(type iteration* i) {
	if (i->exp1 != NULL as type exp*)
		exp_delete(i->exp1);
	if (i->exp2 != NULL as type exp*)
		exp_delete(i->exp2);
	if (i->exp3 != NULL as type exp*)
		exp_delete(i->exp3);
	if (i->decl != NULL as type decl*)
		decl_delete(i->decl);
	if (i->metadata != NULL as type metadata*)
		metadata_delete(i->metadata);
	
	stmt_delete(i->stmt);

	n_free(i as byte*);
}

func type aggregate_member* aggregate_member_none_init(type attribute* attr,
	type util::vector* ids, type metadata* m) {
	type aggregate_member* am = n_malloc(sizeof{type aggregate_member})
		as type aggregate_member*;
	am->attribute = attr;
	am->kind = aggregate_member_kind::NONE;
	am->idents = ids;
	am->metadata = m;
	am->symtab_values = NULL as type util::vector*;
	return am;
}

func type aggregate_member* aggregate_member_typ_init(type attribute* attr,
	type util::vector* ids, type typ* t, type metadata* m) {
	type aggregate_member* am = n_malloc(sizeof{type aggregate_member})
		as type aggregate_member*;
	am->attribute = attr;
	am->kind = aggregate_member_kind::TYP;
	am->which.typ = t;
	am->idents = ids;
	am->metadata = m;
	am->symtab_values = NULL as type util::vector*;
	return am;
}

func type aggregate_member* aggregate_member_aggregate_init(type attribute* attr,
	type util::vector* ids, type stmt* a, type metadata* m) {
	type aggregate_member* am = n_malloc(sizeof{type aggregate_member})
		as type aggregate_member*;
	am->attribute = attr;
	am->kind = aggregate_member_kind::AGGREGATE;
	am->idents = ids;
	am->which.aggregate = a;
	am->metadata = m;
	am->symtab_values = NULL as type util::vector*;
	return am;
}

func void aggregate_member_delete(type aggregate_member* am) {
	if (am->attribute != NULL as type attribute*)
		attribute_delete(am->attribute);

	util::vector_delete(am->idents);
	switch (am->kind) {
	case aggregate_member_kind::NONE:
		break;
	case aggregate_member_kind::TYP:
		typ_delete(am->which.typ);
		break;
	case aggregate_member_kind::AGGREGATE:
		stmt_delete(am->which.aggregate);
		break;
	default:
		util::ice("aggregate_member_delete",
			"Unrecognized aggregate_member_kind while free'ing!");
	}

	if (am->metadata != NULL as type metadata*)
		metadata_delete(am->metadata);
	if (am->symtab_values != NULL as type util::vector*)
		util::vector_delete(am->symtab_values);
	n_free(am as byte*);
}

func type aggregate* aggregate_init(type attribute* attr, unsigned int k,
	type lex::token* n, type util::vector* ms, type metadata* m) {
	type aggregate* a = n_malloc(sizeof{type aggregate})
		as type aggregate*;
	a->attribute = attr;
	a->kind = k;
	a->name = n;
	a->members = ms;
	a->metadata = m;
	a->ctx = NULL as type tck::symtab*;
	a->symtab_value = NULL as type tck::symtab_value*;
	a->enum_symtab_values = NULL as type util::vector*;
	return a;
}

func void aggregate_delete(type aggregate* a) {
	if (a->attribute != NULL as type attribute*)
		attribute_delete(a->attribute);
	if (a->members != NULL as type util::vector*)
		util::vector_delete(a->members);
	if (a->metadata != NULL as type metadata*)
		metadata_delete(a->metadata);
	if (a->enum_symtab_values != NULL as type util::vector*)
		util::vector_delete(a->enum_symtab_values);

	n_free(a as byte*);
}

func type jump* jump_goto_init(type lex::token* i, type metadata* m) {
	type jump* j = n_malloc(sizeof{type jump})
		as type jump*;
	j->kind = jump_kind::GOTO;
	j->ident = i;
	j->exp = NULL as type exp*;
	j->metadata = m;
	j->symtab_value = NULL as type tck::symtab_value*;
	return j;
}

func type jump* jump_break_init(type metadata* m) {
	type jump* j = n_malloc(sizeof{type jump})
		as type jump*;
	j->kind = jump_kind::BREAK;
	j->ident = NULL as type lex::token*;
	j->exp = NULL as type exp*;
	j->metadata = m;
	j->symtab_value = NULL as type tck::symtab_value*;
	return j;
}

func type jump* jump_continue_init(type metadata* m) {
	type jump* j = n_malloc(sizeof{type jump})
		as type jump*;
	j->kind = jump_kind::CONTINUE;
	j->ident = NULL as type lex::token*;
	j->exp = NULL as type exp*;
	j->metadata = m;
	j->symtab_value = NULL as type tck::symtab_value*;
	return j;
}

func type jump* jump_defer_return_init(unsigned int k, type exp* e,
	type metadata* m) {
	type jump* j = n_malloc(sizeof{type jump})
		as type jump*;
	j->kind = k;
	j->ident = NULL as type lex::token*;
	j->exp = e;
	j->metadata = m;
	j->symtab_value = NULL as type tck::symtab_value*;
	return j;
}

func void jump_delete(type jump* j) {
	if (j->exp != NULL as type exp*)
		exp_delete(j->exp);
	if (j->metadata != NULL as type metadata*)
		metadata_delete(j->metadata);
	n_free(j as byte*);
}

func type asm_component* asm_component_init(type lex::token* s,
	type qualified_identifier* qi, type  metadata* m) {
	type asm_component* ac = n_malloc(sizeof{type asm_component})
		as type asm_component*;
	ac->string = s;
	ac->qualified_identifier = qi;
	ac->metadata = m;
	return ac;
}

func type compound* compound_init(type util::vector* s,
	type metadata* m) {
	type compound* c = n_malloc(sizeof{type compound})
		as type compound*;
	c->stmts = s;
	c->ctx = NULL as type tck::symtab*;
	c->metadata = m;
	return c;
}

func void compound_delete(type compound* c) {
	util::vector_delete(c->stmts);
	if (c->metadata != NULL as type metadata*)
		metadata_delete(c->metadata);

	n_free(c as byte*);
}

func void asm_component_delete(type asm_component* ac) {
	if (ac->qualified_identifier != NULL as type qualified_identifier*)
		qualified_identifier_delete(ac->qualified_identifier);
	if (ac->metadata != NULL as type metadata*)
		metadata_delete(ac->metadata);
	n_free(ac as byte*);
}

func type namespace_alias* namespace_alias_init(
	type attribute* attr, type lex::token* tns,
	type qualified_identifier* fns, type metadata* m) {
	type namespace_alias* na = n_malloc(sizeof{type namespace_alias})
		as type namespace_alias*;
	na->attribute = attr;
	na->to_name = tns;
	na->from_name = fns;
	na->metadata = m;
	return na;
}

func void namespace_alias_delete(type namespace_alias* na) {
	if (na->attribute != NULL as type attribute*)
		attribute_delete(na->attribute);
	qualified_identifier_delete(na->from_name);
	if (na->metadata != NULL as type metadata*)
		metadata_delete(na->metadata);

	n_free(na as byte*);
}

func type import_alias_data* import_alias_data_init(type lex::token* f_n,
	type lex::token* tn) {
	type import_alias_data* iad = n_malloc(sizeof{type import_alias_data})
		as type import_alias_data*;
	iad->from_name = f_n;
	iad->to_name = tn;
	return iad;
}

func void import_alias_data_delete(type import_alias_data* iad) {
	n_free(iad as byte*);
}

func type import_data* import_data_init(type attribute* a, bool is,
	type qualified_identifier* mn, type lex::token* an, bool s,
	type util::vector* mns, type metadata* m) {
	type import_data* id = n_malloc(sizeof{type import_data})
		as type import_data*;
	id->attribute = a;
	id->is_system = is;
	id->module_name = mn;
	id->alias_name = an;
	id->starred = s;
	id->member_names = mns;
	id->metadata = m;
	return id;
}

func void import_data_delete(type import_data* id) {
	if (id->attribute != NULL as type attribute*)
		attribute_delete(id->attribute);
	qualified_identifier_delete(id->module_name);
	if (id->member_names != NULL as type util::vector*)
		util::vector_delete(id->member_names);
	if (id->metadata != NULL as type metadata*)
		metadata_delete(id->metadata);
	n_free(id as byte*);
}

func type attribute* attribute_init(type util::vector* al,
	type metadata* m) {
	type attribute* attr = n_malloc(sizeof{type attribute})
		as type attribute*;
	attr->attrs_list = al;
	attr->attrs = NULL as type util::hash_table*;
	attr->metadata = m;
	return attr;
}

func type using_data* using_data_init(type attribute* attr,
	type qualified_identifier* qi, type metadata* m) {
	type using_data* ud = n_malloc(sizeof{type using_data})
		as type using_data*;
	ud->attribute = attr;
	ud->qualified_identifier = qi;
	ud->metadata = m;
	return ud;
}

func void using_data_delete(type using_data* ud) {
	if (ud->attribute != NULL as type attribute*)
		attribute_delete(ud->attribute);
	qualified_identifier_delete(ud->qualified_identifier);
	if (ud->metadata != NULL as type metadata*)
		metadata_delete(ud->metadata);
	n_free(ud as byte*);
}

func type type_alias_value* type_alias_value_init(type lex::token* i,
	type typ* u, type metadata* m) {
	type type_alias_value* tav = n_malloc(sizeof{type type_alias_value})
		as type type_alias_value*;
	tav->ident = i;
	tav->underlying = u;
	tav->metadata = m;
	tav->symtab_value = NULL as type tck::symtab_value*;
	return tav;
}

func void type_alias_value_delete(type type_alias_value* tav) {
	typ_delete(tav->underlying);
	if (tav->metadata != NULL as type metadata*)
		metadata_delete(tav->metadata);
	n_free(tav as byte*);
}

func type type_alias* type_alias_init(type attribute* a, type util::vector* al,
	type metadata* m) {
	type type_alias* ta = n_malloc(sizeof{type type_alias})
		as type type_alias*;
	ta->attribute = a;
	ta->aliases = al;
	ta->metadata = m;
	return ta;
}

func void type_alias_delete(type type_alias* ta) {
	if (ta->attribute != NULL as type attribute*)
		attribute_delete(ta->attribute);
	util::vector_delete(ta->aliases);
	if (ta->metadata != NULL as type metadata*)
		metadata_delete(ta->metadata);
	n_free(ta as byte*);
}

func void attribute_delete(type attribute* a) {
	util::vector_delete(a->attrs_list);
	if (a->attrs != NULL as type util::hash_table*)
		util::ht_delete(a->attrs);
	if (a->metadata != NULL as type metadata*)
		metadata_delete(a->metadata);
	n_free(a as byte*);
}

func type match_branch* match_branch_init(type util::vector* ps, type exp* g,
	type stmt* b, type metadata* m) {
	type match_branch* mb = n_malloc(sizeof{type match_branch})
		as type match_branch*;
	mb->pats = ps;
	mb->guard = g;
	mb->body = b;
	mb->metadata = m;
	mb->ctx = NULL as type tck::symtab*;
	return mb;
}

func void match_branch_delete(type match_branch* mb) {
	util::vector_delete(mb->pats);
	if (mb->guard != NULL as type exp*)
		exp_delete(mb->guard);
	stmt_delete(mb->body);
	if (mb->metadata != NULL as type metadata*)
		metadata_delete(mb->metadata);
	n_free(mb as byte*);
}

func type match_data* match_data_init(type exp* p, type util::vector* mbs,
	type metadata* m) {
	type match_data* md = n_malloc(sizeof{type match_data})
		as type match_data*;
	md->parent = p;
	md->match_branches = mbs;
	md->metadata = m;
	md->ctx = NULL as type tck::symtab*;
	return md;
}

func void match_data_delete(type match_data* md) {
	exp_delete(md->parent);
	util::vector_delete(md->match_branches);
	if (md->metadata != NULL as type metadata*)
		metadata_delete(md->metadata);
	n_free(md as byte*);
}

func type stmt* stmt_decl_init(type decl* d) {
	type stmt* s = n_malloc(sizeof{type stmt}) as type stmt*;
	s->kind = stmt_kind::DECL;
	s->which.decl = d;
	s->metadata = metadata_copy(d->metadata);
	return s;
}

func type stmt* stmt_exp_init(type exp* e) {
	type stmt* s = n_malloc(sizeof{type stmt}) as type stmt*;
	s->kind = stmt_kind::EXP;
	s->which.exp = e;
	s->metadata = metadata_copy(e->metadata);
	return s;
}

func type stmt* stmt_aggregate_init(type aggregate* a) {
	type stmt* s = n_malloc(sizeof{type stmt})
		as type stmt*;
	s->kind = stmt_kind::AGGREGATE;
	s->which.aggregate = a;
	s->metadata = metadata_copy(a->metadata);
	return s;
}

func type stmt* stmt_labeled_init(type labeled* l) {
	type stmt* s = n_malloc(sizeof{type stmt})
		as type stmt*;
	s->kind = stmt_kind::LABELED;
	s->which.labeled = l;
	s->metadata = metadata_copy(l->metadata);
	return s;
}

func type stmt* stmt_compound_init(type compound* c) {
	type stmt* s = n_malloc(sizeof{type stmt})
		as type stmt*;
	s->kind = stmt_kind::COMPOUND;
	s->which.compound = c;
	s->metadata = metadata_copy(c->metadata);
	return s;
}

func type stmt* stmt_selection_init(type selection* se) {
	type stmt* s = n_malloc(sizeof{type stmt})
		as type stmt*;
	s->kind = stmt_kind::SELECTION;
	s->which.selection = se;
	s->metadata = metadata_copy(se->metadata);
	return s;
}

func type stmt* stmt_iteration_init(type iteration* i) {
	type stmt* s = n_malloc(sizeof{type stmt})
		as type stmt*;
	s->kind = stmt_kind::ITERATION;
	s->which.iteration = i;
	s->metadata = metadata_copy(i->metadata);
	return s;
}

func type stmt* stmt_jump_init(type jump* j) {
	type stmt* s = n_malloc(sizeof{type stmt})
		as type stmt*;
	s->kind = stmt_kind::JUMP;
	s->which.jump = j;
	s->metadata = metadata_copy(j->metadata);
	return s;
}

func type stmt* stmt_using_init(unsigned int k, type using_data* ud) {
	type stmt* s = n_malloc(sizeof{type stmt})
		as type stmt*;
	s->kind = k;
	s->which.using_data = ud;
	s->metadata = metadata_copy(ud->metadata);
	return s;
}

func type stmt* stmt_asm_init(type util::vector* a,
	type metadata* m) {
	type stmt* s = n_malloc(sizeof{type stmt})
		as type stmt*;
	s->kind = stmt_kind::ASM;
	s->which.asm = a;
	s->metadata = m;
	return s;
}

func type stmt* stmt_empty_init(type metadata* m) {
	type stmt* s = n_malloc(sizeof{type stmt})
		as type stmt*;
	s->kind = stmt_kind::EMPTY;
	s->metadata = m;
	return s;
}

func type stmt* stmt_namespace_alias_init(type namespace_alias* na,
	type metadata* m) {
	type stmt* s = n_malloc(sizeof{type stmt})
		as type stmt*;
	s->kind = stmt_kind::NAMESPACE_ALIAS;
	s->which.namespace_alias = na;
	s->metadata = m;
	return s;
}

func type stmt* stmt_import_init(type import_data* id) {
	type stmt* s = n_malloc(sizeof{type stmt})
		as type stmt*;
	s->kind = stmt_kind::IMPORT;
	s->which.import_data = id;
	s->metadata = metadata_copy(id->metadata);
	return s;
}

func type stmt* stmt_include_init(type util::vector* is, type metadata* m) {
	type stmt* s = n_malloc(sizeof{type stmt})
		as type stmt*;
	s->kind = stmt_kind::INCLUDE;
	s->which.includes = is;
	s->metadata = m;
	return s;
}

func type stmt* stmt_type_alias_init(type type_alias* ta) {
	type stmt* s = n_malloc(sizeof{type stmt}) as type stmt*;
	s->kind = stmt_kind::TYPE_ALIAS;
	s->which.type_alias = ta;
	s->metadata = metadata_copy(ta->metadata);
	return s;
}

func type stmt* stmt_match_data_init(type match_data* md) {
	type stmt* s = n_malloc(sizeof{type stmt}) as type stmt*;
	s->kind = stmt_kind::MATCH;
	s->which.match_data = md;
	s->metadata = metadata_copy(md->metadata);
	return s;
}

func void stmt_delete(type stmt* s) {
	switch (s->kind) {
	case stmt_kind::DECL:
		decl_delete(s->which.decl);
		break;
	case stmt_kind::EXP:
		exp_delete(s->which.exp);
		break;
	case stmt_kind::AGGREGATE:
		aggregate_delete(s->which.aggregate);
		break;
	case stmt_kind::LABELED:
		labeled_delete(s->which.labeled);
		break;
	case stmt_kind::COMPOUND:
		compound_delete(s->which.compound);
		break;
	case stmt_kind::SELECTION:
		selection_delete(s->which.selection);
		break;
	case stmt_kind::ITERATION:
		iteration_delete(s->which.iteration);
		break;
	case stmt_kind::JUMP:
		jump_delete(s->which.jump);
		break;
	case stmt_kind::USING:
	case stmt_kind::USING_NAMESPACE:
		using_data_delete(s->which.using_data);
		break;
	case stmt_kind::ASM:
		util::vector_delete(s->which.asm);
		break;
	case stmt_kind::EMPTY:
		break;
	case stmt_kind::NAMESPACE_ALIAS:
		namespace_alias_delete(s->which.namespace_alias);
		break;
	case stmt_kind::IMPORT:
		import_data_delete(s->which.import_data);
		break;
	case stmt_kind::INCLUDE:
		util::vector_delete(s->which.includes);
		break;
	case stmt_kind::TYPE_ALIAS:
		type_alias_delete(s->which.type_alias);
		break;
	case stmt_kind::MATCH:
		match_data_delete(s->which.match_data);
		break;
	default:
		util::ice("stmt_delete",
			"Unrecognized stmt_kind while free'ing!");
	}

	if (s->metadata != NULL as type metadata*)
		metadata_delete(s->metadata);
	n_free(s as byte*);
}

func[static] void stmt_labeled_print_helper(type labeled* l, unsigned int t) {
	switch (l->kind) {
	case labeled_kind::CASE: {
		printf("STMT-CASE:\n");
		exp_print(l->exp, t + 1), printf("\n");
	}
		break;
	case labeled_kind::DEFAULT: {
		printf("STMT-DEFAULT:\n");
	}
		break;
	case labeled_kind::LABEL: {
		printf("STMT-LABEL:\n");
		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		lex::token_print(l->name), printf("\n");
	}
		break;
	default:
		util::ice("stmt_labeled_print_helper",
			"Unrecognized labeled_kind while printing!");
	}
	
	stmt_print(l->stmt, t + 1);
}

func void decl_print(type decl* d, unsigned int t) {
	for (unsigned int i = 0; i < t; i++)
		printf(" ");
	printf("(DECL:\n");

	if (d->attribute != NULL as type attribute*)
		attribute_print(d->attribute, t + 1), printf("\n");

	if (d->is_static) {
		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		printf("<STATIC>\n");
	}
	if (d->is_const) {
		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		printf("<CONST>\n");
	}

	for (unsigned int i = 0; i < t + 1; i++)
		printf(" ");
	printf("[");
	for (unsigned int i = 0; i < util::vector_size(d->decl_components); i++) {
		type decl_component* dc = util::vector_at(d->decl_components, i)
			as type decl_component** @;
		printf("(DECL-COMPONENT:\n");

		pat_print(dc->pat, t + 3);
		if (dc->typ != NULL as type ast::typ*)
			printf("\n"), typ_print(dc->typ, t + 3);
		if (dc->init != NULL as type exp*)
			printf("\n"), exp_print(dc->init, t + 3);

		printf(")");
		if (i != util::vector_size(d->decl_components) - 1) {
			printf(",\n");
			for (unsigned int i = 0; i < t + 2; i++)
				printf(" ");
		}
	}

	printf("])");
}

func[static] void stmt_selection_print_helper(type selection* se,
	unsigned int t) {
	switch (se->kind) {
	case selection_kind::IF: {
		printf("STMT-IF:\n");
		if (se->decl != NULL as type ast::decl*)
			decl_print(se->decl, t + 1), printf("\n");
		exp_print(se->exp, t + 1), printf("\n");
		stmt_print(se->stmt1, t + 1);
	}
		break;
	case selection_kind::IF_ELSE: {
		printf("STMT-IF-ELSE:\n");
		if (se->decl != NULL as type ast::decl*)
			decl_print(se->decl, t + 1), printf("\n");
		exp_print(se->exp, t + 1), printf("\n");
		stmt_print(se->stmt1, t + 1), printf("\n");
		stmt_print(se->stmt2, t + 1);
	}
		break;
	case selection_kind::SWITCH: {
		printf("STMT-SWITCH:\n");
		if (se->decl != NULL as type ast::decl*)
			decl_print(se->decl, t + 1), printf("\n");
		exp_print(se->exp, t + 1), printf("\n");
		stmt_print(se->stmt1, t + 1);
	}
		break;
	default:
		util::ice("stmt_selection_print_helper",
			"Unrecognized selection_kind while printing!");
	}
}

func[static] void stmt_jump_print_helper(type jump* j, unsigned int t) {
	switch (j->kind) {
	case jump_kind::BREAK:
		printf("STMT-BREAK:");
		break;
	case jump_kind::CONTINUE:
		printf("STMT-CONTINUE:");
		break;
	case jump_kind::RETURN:
	case jump_kind::ERR_RETURN:
	case jump_kind::DEFER:
	case jump_kind::ERR_DEFER:
		if (j->kind == ast::jump_kind::RETURN)
			printf("STMT-RETURN:");
		else if (j->kind == ast::jump_kind::ERR_RETURN)
			printf("STMT-ERR_RETURN:");
		else if (j->kind == ast::jump_kind::DEFER)
			printf("STMT-DEFER:");
		else
			printf("STMT-ERR_DEFER:");

		if (j->exp != NULL as type ast::exp*) {
			printf("\n");
			exp_print(j->exp, t + 1);
		}
		break;
	case jump_kind::GOTO:
		printf("STMT-GOTO:\n");
		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		lex::token_print(j->ident);
		break;
	default:
		util::ice("stmt_jump_print_helper",
			"Unrecognized jump_kind while printing!");
	}
}

func[static] void stmt_aggregate_print_helper(type aggregate* a, unsigned int t) {
	switch (a->kind) {
	case aggregate_kind::VARIANT:
	case aggregate_kind::STRUCT:
	case aggregate_kind::UNION: {
		bool is_struct = a->kind == aggregate_kind::STRUCT,
			is_variant = a->kind == aggregate_kind::VARIANT;
		if (a->members == NULL as type util::vector*) {
			util::maybe_ice(a->name != NULL as type lex::token*,
				"stmt_aggregate_print_helper",
				"Cannot forward-declare an anonymous struct/union.");

			if (is_struct)
				printf("STMT-STRUCT-DECL:\n");
			else if (is_variant)
				printf("STMT-VARIANT-DECL:\n");
			else
				printf("STMT-UNION-DECL:\n");

			if (a->attribute != NULL as type attribute*)
				attribute_print(a->attribute, t + 1), printf("\n");

			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			lex::token_print(a->name);
			break;
		}

		if (is_struct)
			printf("STMT-STRUCT:\n");
		else if (is_variant)
			printf("STMT-VARIANT:\n");
		else
			printf("STMT-UNION:\n");

		if (a->attribute != NULL as type attribute*)
			attribute_print(a->attribute, t + 1), printf("\n");

		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		if (a->name == NULL as type lex::token*)
			printf("<ANON>");
		else
			lex::token_print(a->name);
		printf("\n");

		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		printf("[");
		if (!util::vector_empty(a->members)) printf("\n");
		for (unsigned int i = 0; i < util::vector_size(a->members); i++) {
			type ast::aggregate_member* am = util::vector_at(a->members, i)
				as type ast::aggregate_member** @;

			for (unsigned int j = 0; j < t + 2; j++)
				printf(" ");
			printf("[\n");
			if (am->attribute != NULL as type attribute*) {
				printf("\n");
				attribute_print(am->attribute*, t + 3), printf(",\n");
			}

			for (unsigned int j = 0; j < t + 3; j++)
				printf(" ");
			printf("[\n");
			for (unsigned int j = 0; j < util::vector_size(am->idents); j++) {
				type lex::token* id = util::vector_at(am->idents, j)
					as type lex::token** @;

				for (unsigned int k = 0; k < t + 4; k++)
					printf(" ");
				lex::token_print(id);
				if (j != util::vector_size(am->idents) - 1)
					 printf(",\n");
			}
			printf("]");

			switch (am->kind) {
			case aggregate_member_kind::NONE:
				break;
			case aggregate_member_kind::TYP: {
				printf(",\n");
				typ_print(am->which.typ, t + 3);
			}
				break;
			case aggregate_member_kind::AGGREGATE: {
				printf(",\n");
				stmt_print(am->which.aggregate, t + 3);
			}
				break;
			default:
				util::ice("stmt_aggregate_print_helper",
					"Unrecognized variant_member_kind while printing!");
			}
			printf("]");

			if (i != util::vector_size(a->members) - 1)
				printf(",\n");
		}
		printf("]");
	}
		break;
	case aggregate_kind::ENUM: {
		if (a->members == NULL as type util::vector*) {
			printf("STMT-ENUM-DECL:\n");

			if (a->attribute != NULL as type attribute*)
				attribute_print(a->attribute, t + 1), printf("\n");

			util::maybe_ice(a->name != NULL as type lex::token*,
				"stmt_aggregate_print_helper",
				"Cannot forward-declare an anonymous enum!");

			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			lex::token_print(a->name);
			break;
		}

		printf("STMT-ENUM:\n");

		if (a->attribute != NULL as type attribute*)
			attribute_print(a->attribute, t + 1), printf("\n");

		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		if (a->name == NULL as type lex::token*)
			printf("<ANON>");
		else
			lex::token_print(a->name);
		printf("\n");

		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		printf("[");
		if (!util::vector_empty(a->members)) printf("\n");
		for (unsigned int i = 0; i < util::vector_size(a->members); i++) {
			type lex::token* ident = util::vector_at(a->members, i)
				as type lex::token** @;

			for (unsigned int i = 0; i < t + 2; i++)
				printf(" ");
			lex::token_print(ident);

			if (i != util::vector_size(a->members) - 1)
				printf(",\n");
		}
		printf("]");
	}
		break;
	default:
		util::ice("stmt_aggregate_print_helper",
			"Unrecognized aggregate_kind while printing!");
	}
}

func[static] void stmt_iteration_print_helper(type iteration* i, unsigned int t) {
	switch (i->kind) {
	case iteration_kind::WHILE: {
		printf("STMT-ITERATION-WHILE:\n");
		exp_print(i->exp1, t + 1), printf("\n");
		stmt_print(i->stmt, t + 1);
	}
		break;
	case iteration_kind::DO: {
		printf("STMT-ITERATION-DO-WHILE:\n");
		stmt_print(i->stmt, t + 1), printf("\n");
		exp_print(i->exp1, t + 1);
	}
		break;
	case iteration_kind::FOR_EXP: {
		printf("STMT-ITERATION-FOR-EXP:\n");
		if (i->exp1 == NULL as type ast::exp*) {
			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("<EMPTY>");
		}
		else
			exp_print(i->exp1, t + 1);
		printf("\n");

		if (i->exp2 == NULL as type ast::exp*) {
			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("<EMPTY>");
		}
		else
			exp_print(i->exp2, t + 1);
		printf("\n");

		if (i->exp3 == NULL as type ast::exp*) {
			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("<EMPTY>");
		}
		else
			exp_print(i->exp3, t + 1);
		printf("\n");

		stmt_print(i->stmt, t + 1);
	}
		break;
	case iteration_kind::FOR_DECL: {
		printf("STMT-ITERATION-FOR-DECL:\n");
		if (i->decl == NULL as type ast::decl*) {
			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("<EMPTY>");
		}
		else
			decl_print(i->decl, t + 1);
		printf("\n");

		if (i->exp1 == NULL as type ast::exp*) {
			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("<EMPTY>");
		}
		else
			exp_print(i->exp1, t + 1);
		printf("\n");

		if (i->exp2 == NULL as type ast::exp*) {
			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("<EMPTY>");
		}
		else
			exp_print(i->exp2, t + 1);
		printf("\n");

		stmt_print(i->stmt, t + 1);
	}
		break;
	default:
		util::ice("stmt_iteration_print_helper",
			"Unrecognized iteration_kind while printing!");
	}
}

func void attribute_print(type attribute* a, unsigned int t) {
	for (unsigned int i = 0; i < t; i++)
		printf(" ");
	printf("(ATTRIBUTE:");
	if (!util::vector_empty(a->attrs_list))
		printf("\n");

	for (unsigned int i = 0; i < util::vector_size(a->attrs_list); i++) {
		type attribute_value* av = util::vector_at(a->attrs_list, i)
			as type attribute_value*;
		for (unsigned int j = 0; j < t + 1; j++)
			printf(" ");

		lex::token_print(av->key);
		if (av->value != NULL as type lex::token*)
			printf(", "), lex::token_print(av->value);

		if (i != util::vector_size(a->attrs_list) - 1)
			printf("\n");
	}

	printf(")");
}

func void stmt_print(type stmt* s, unsigned int t) {
	for (unsigned int i = 0; i < t; i++)
		printf(" ");
	printf("(");

	switch (s->kind) {
	case stmt_kind::IMPORT:
		// TODO
		util::ice("stmt_print", "Import statements are not yet supported!");
	case stmt_kind::INCLUDE: {
		printf("STMT-INCLUDE:\n");

		type util::vector* is = s->which.includes;
		for (unsigned int i = 0; i < util::vector_size(is); i++) {
			type lex::token* f = util::vector_at(is, i) as type lex::token** @;

			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			lex::token_print(f);
			if (i != util::vector_size(is) - 1)
				printf("\n");
		}
	}
		break;
	case stmt_kind::EXP:
		printf("STMT-EXP:\n");
		exp_print(s->which.exp, t + 1);
		break;
	case stmt_kind::LABELED: {
		type labeled* l = s->which.labeled;
		stmt_labeled_print_helper(l, t + 1);
	}
		break;
	case stmt_kind::DECL: {
		printf("STMT-DECL:\n");
		type decl* d = s->which.decl;
		decl_print(d, t + 1);
	}
		break;
	case stmt_kind::EMPTY:
		printf("STMT-EMPTY:");
		break;
	case stmt_kind::SELECTION: {
		type ast::selection* se = s->which.selection;
		stmt_selection_print_helper(se, t + 1);
	}
		break;
	case stmt_kind::JUMP: {
		type ast::jump* j = s->which.jump;
		stmt_jump_print_helper(j, t + 1);
	}
		break;
	case stmt_kind::COMPOUND: {
		printf("STMT-COMPOUND:");
		type util::vector* stmts = s->which.compound->stmts;
		if (!util::vector_empty(stmts)) {
			printf("\n");
			for (unsigned int i = 0; i < util::vector_size(stmts); i++) {
				type ast::stmt* s = util::vector_at(stmts, i)
					as type ast::stmt** @;
				stmt_print(s, t + 1);
				if (i != util::vector_size(stmts) - 1)
					printf("\n");
			}
		}
	}
		break;
	case stmt_kind::AGGREGATE: {
		type aggregate* a = s->which.aggregate;
		stmt_aggregate_print_helper(a, t + 1);
	}
		break;
	case stmt_kind::ITERATION: {
		type iteration* i = s->which.iteration;
		stmt_iteration_print_helper(i, t + 1);
	}
		break;
	case stmt_kind::ASM: {
		printf("STMT-ASM:");
		if (!util::vector_empty(s->which.asm))
			printf("\n");

		for (unsigned int i = 0; i < util::vector_size(s->which.asm); i++) {
			type asm_component* ac = util::vector_at(s->which.asm, i)
				as type asm_component** @;

			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("(");
			lex::token_print(ac->string);

			if (ac->qualified_identifier != NULL as type qualified_identifier*) {
				printf("\n");
				for (unsigned int i = 0; i < t + 1; i++)
					printf(" ");
				print_qualified_identifier(ac->qualified_identifier, t + 1);
			}
			printf(")");

			if (i != util::vector_size(s->which.asm) - 1)
				printf("\n");
		}
	}
		break;
	case stmt_kind::USING:
	case stmt_kind::USING_NAMESPACE: {
		if (s->kind == stmt_kind::USING)
			printf("STMT-USING: ");
		else
			printf("STMT-USING-NAMESPACE: ");

		type using_data* ud = s->which.using_data;
		if (ud->attribute != NULL as type attribute*)
			attribute_print(ud->attribute, t + 1), printf("\n");

		print_qualified_identifier(ud->qualified_identifier, t + 1);
	}
		break;
	case stmt_kind::NAMESPACE_ALIAS: {
		printf("STMT-NAMESPACE-ALIAS:\n");

		type namespace_alias* na = s->which.namespace_alias;

		if (na->attribute != NULL as type attribute*)
			attribute_print(na->attribute, t + 1), printf("\n");

		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		lex::token_print(na->to_name), printf("\n");

		for (unsigned int i = 0; i < t + 1; i++)
			printf(" ");
		print_qualified_identifier(na->from_name, t + 1);
	}
		break;
	case stmt_kind::TYPE_ALIAS: {
		type type_alias* ta = s->which.type_alias;

		printf("STMT-TYPE-ALIAS:\n");
		if (ta->attribute != NULL as type attribute*)
			attribute_print(ta->attribute, t + 1), printf("\n");

		for (unsigned int i = 0; i < util::vector_size(ta->aliases); i++) {
			type type_alias_value* tav = util::vector_at(ta->aliases, i)
				as type type_alias_value** @;
			for (unsigned int i = 0; i < t + 1; i++)
				printf(" ");
			printf("[");
			lex::token_print(tav->ident), printf("\n");
			typ_print(tav->underlying, t + 2);
			printf("]");

			if (i != util::vector_size(ta->aliases) - 1)
				printf(",\n");
		}
	}
		break;
	case stmt_kind::MATCH: {
		type match_data* md = s->which.match_data;

		printf("STMT-MATCH:\n");
		exp_print(md->parent, t + 1), printf("\n");

		for (unsigned int i = 0; i < util::vector_size(md->match_branches); i++) {
			type match_branch* mb = util::vector_at(md->match_branches, i)
				as type match_branch** @;

			for (unsigned int j = 0; j < t + 1; j++)
				printf(" ");
			printf("[\n");

			for (unsigned int j = 0; j < t + 2; j++)
				printf(" ");
			printf("[\n");

			for (unsigned int j = 0; j < util::vector_size(mb->pats); j++) {
				type pat* cp = util::vector_at(mb->pats, j)
					as type pat** @;

				pat_print(cp, t + 3);
				if (j != util::vector_size(mb->pats) - 1)
					printf(",\n");
			}
			printf("]\n");

			if (mb->guard != NULL as type exp*)
				exp_print(mb->guard, t + 2), printf("\n");

			stmt_print(mb->body, t + 2), printf("]");

			if (i != util::vector_size(md->match_branches) - 1)
				printf("\n");
		}
	}
		break;
	default:
		util::ice("stmt_print",
			"Unrecognized stmt_kind while print'ing!");
	}

	printf(")");
}

} } // namespace neutrino::ast
