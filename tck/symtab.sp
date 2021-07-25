import "tck/symtab.hsp"

import <"std/io">
import <"std/lib">
import <"std/string">

import "ast/ast.hsp"
import "util/hash_table.hsp"
import "util/vector.hsp"
import "util/generic_funcs.hsp"
import "util/base.hsp"
import "util/error.hsp"
import "tck/util.hsp"

using std::io::printf;
using std::lib::NULL;
using std::string::memcpy;
using neutrino::util::n_malloc;
using neutrino::util::n_free;

namespace neutrino { namespace tck {

func[static] type symtab_value* symtab_value_init_helper(type symtab* e, bool f,
	bool cr, type lex::token* n, type ast::typ* t) {
	type symtab_value* sv = n_malloc(sizeof{type symtab_value})
		as type symtab_value*;
	sv->enclosing = e;
	sv->fwd = f;
	sv->name = n;

	sv->typ = t;
	sv->cross_ref = cr;
	sv->members = NULL as type symtab*;
	sv->attrs = NULL as type util::hash_table*;
	sv->is_constant = false;
	return sv;
}

func type symtab_value* symtab_value_type_alias_init(type symtab* e, bool f,
	bool cr, type lex::token* n, type ast::typ* t,
	type ast::type_alias_value* tav) {
	type symtab_value* sv = symtab_value_init_helper(e, f, cr, n, t);

	sv->kind = symtab_value_kind::TYPE_ALIAS;
	sv->ast_node_kind = symtab_value_ast_node_kind::TYPE_ALIAS;
	sv->ast_node.type_alias_value = tav;
	return sv;
}

func type symtab_value* symtab_value_decl_init(type symtab* e, bool f,
	bool cr, type lex::token* n, type ast::typ* t,
	type lex::token* ds) {
	type symtab_value* sv = symtab_value_init_helper(e, f, cr, n, t);

	sv->kind = symtab_value_kind::SYMBOL;
	sv->ast_node_kind = symtab_value_ast_node_kind::DECL;
	sv->ast_node.decl_symbol = ds;
	return sv;
}

func type symtab_value* symtab_value_member_init(type symtab* e, bool f,
	bool cr, type lex::token* n, type ast::typ* t,
	type lex::token* ms) {
	type symtab_value* sv = symtab_value_init_helper(e, f, cr, n, t);

	sv->kind = symtab_value_kind::SYMBOL;
	sv->ast_node_kind = symtab_value_ast_node_kind::MEMBER;
	sv->ast_node.member_symbol = ms;
	return sv;
}

func type symtab_value* symtab_value_nullary_variant_init(type symtab* e, bool f,
	bool cr, type lex::token* n, type ast::typ* t,
	type lex::token* m) {
	type symtab_value* sv = symtab_value_init_helper(e, f, cr, n, t);

	sv->kind = symtab_value_kind::SYMBOL;
	sv->ast_node_kind = symtab_value_ast_node_kind::VARIANT_CONSTRUCTOR;
	sv->ast_node.variant_constructor = m;
	return sv;
}

func type symtab_value* symtab_value_variant_constructor_init(type symtab* e, bool f,
	bool cr, type lex::token* n, type ast::typ* t,
	type lex::token* m) {
	type symtab_value* sv = symtab_value_init_helper(e, f, cr, n, t);

	sv->kind = symtab_value_kind::FUN;
	sv->ast_node_kind = symtab_value_ast_node_kind::VARIANT_CONSTRUCTOR;
	sv->ast_node.variant_constructor = m;
	return sv;
}

func type symtab_value* symtab_value_enum_symbol_init(type symtab* e, bool f,
	bool cr, type lex::token* n, type ast::typ* t,
	type lex::token* es) {
	type symtab_value* sv = symtab_value_init_helper(e, f, cr, n, t);

	sv->kind = symtab_value_kind::SYMBOL;
	sv->ast_node_kind = symtab_value_ast_node_kind::ENUM_SYMBOL;
	sv->ast_node.enum_symbol = es;
	return sv;
}

func type symtab_value* symtab_value_label_init(type symtab* e, bool f,
	bool cr, type lex::token* n, type ast::typ* t,
	type ast::labeled* l) {
	type symtab_value* sv = symtab_value_init_helper(e, f, cr, n, t);

	sv->kind = symtab_value_kind::SYMBOL;
	sv->ast_node_kind = symtab_value_ast_node_kind::LABEL;
	sv->ast_node.label = l;
	return sv;
}

func type symtab_value* symtab_value_lambda_init(type symtab* e, bool f,
	bool cr, type lex::token* n, type ast::typ* t,
	type ast::function* lc) {
	type symtab_value* sv = symtab_value_init_helper(e, f, cr, n, t);

	sv->kind = symtab_value_kind::LAMBDA;
	sv->ast_node_kind = symtab_value_ast_node_kind::LAMBDA;
	sv->ast_node.lambda = lc;
	return sv;
}

func type symtab_value* symtab_value_fun_init(type symtab* e, bool f,
	bool cr, type lex::token* n, type ast::typ* t,
	type ast::function* fc) {
	type symtab_value* sv = symtab_value_init_helper(e, f, cr, n, t);

	sv->kind = symtab_value_kind::FUN;
	sv->ast_node_kind = symtab_value_ast_node_kind::FUN;
	sv->ast_node.function = fc;
	return sv;
}

func type symtab_value* symtab_value_aggregate_init(type symtab* e, bool f,
	bool cr, unsigned int k, type lex::token* n, type ast::typ* t,
	type ast::aggregate* a) {
	type symtab_value* sv = symtab_value_init_helper(e, f, cr, n, t);

	sv->kind = k;
	sv->ast_node_kind = symtab_value_ast_node_kind::AGGREGATE;
	sv->ast_node.aggregate = a;
	return sv;
}

func type symtab_value* symtab_value_namespace_init(type symtab* e, bool f,
	bool cr, type lex::token* n, type ast::typ* t,
	type ast::namespace_decls* nd) {
	type symtab_value* sv = symtab_value_init_helper(e, f, cr, n, t);

	sv->kind = symtab_value_kind::NAMESPACE;
	sv->ast_node_kind = symtab_value_ast_node_kind::NAMESPACE;
	sv->ast_node.namespace_decls = nd;
	return sv;
}

func type symtab_value* symtab_value_cross_ref_init(
	type symtab_value* sv, type lex::token* to_name) {
	type symtab_value* curr_sv = n_malloc(sizeof{type symtab_value})
		as type symtab_value*;
	curr_sv->enclosing = sv->enclosing;
	curr_sv->kind = sv->kind;
	curr_sv->ast_node_kind = sv->ast_node_kind;
	curr_sv->fwd = sv->fwd;
	curr_sv->cross_ref = true;
	curr_sv->name = to_name;
	
	curr_sv->typ = sv->typ;
	curr_sv->members = sv->members;
	curr_sv->attrs = NULL as type util::hash_table*;
	memcpy(curr_sv->ast_node$ as byte*, sv->ast_node$ as byte*,
		sizeof{type symtab_value_ast_node});
	return curr_sv;
}

func void symtab_value_delete(type symtab_value* sv) {
	if (!sv->cross_ref && sv->members != NULL as type symtab*)
		symtab_delete(sv->members);

	if (!sv->cross_ref && sv->typ != NULL as type ast::typ*)
		ast::typ_delete(sv->typ);

	n_free(sv as byte*);
}

func void symtab_value_free(byte* b) {
	type symtab_value* sv = b as type symtab_value*;
	symtab_value_delete(sv);
}

func type util::hash_table* symtab_ht_init() {
	return util::ht_init(util::str_hash, util::str_eq,
		util::str_free, symtab_value_free);
}

func type symtab_info* symtab_info_switch_init(type ast::selection* sw, type ast::typ* rpt) {
	type symtab_info* s = n_malloc(sizeof{type symtab_info})
		as type symtab_info*;
	s->which.switch_stmt = sw;
	s->resolved_parent_typ = rpt;
	s->kind = symtab_info_kind::SWITCH;
	return s;
}

func type symtab_info* symtab_info_fun_init(type ast::function* f, type ast::typ* rpt) {
	type symtab_info* s = n_malloc(sizeof{type symtab_info})
		as type symtab_info*;
	s->which.function = f;
	s->resolved_parent_typ = rpt;
	s->kind = symtab_info_kind::FUN;
	return s;
}

func type symtab_info* symtab_info_global_init() {
	type symtab_info* s = n_malloc(sizeof{type symtab_info})
		as type symtab_info*;
	s->which.ufcs_table = util::ht_init(
		util::str_hash, util::str_eq,
		util::no_free, util::vector_free);
	s->resolved_parent_typ = NULL as type ast::typ*;
	s->kind = symtab_info_kind::GLOBAL;
	return s;
}

func void symtab_info_delete(type symtab_info* s) {
	if (s->resolved_parent_typ != NULL as type ast::typ*)
		ast::typ_delete(s->resolved_parent_typ);
	if (s->kind == symtab_info_kind::GLOBAL)
		util::ht_delete(s->which.ufcs_table);

	n_free(s as byte*);
}

func type symtab* symtab_global_init() {
	type symtab* global = n_malloc(sizeof{type symtab})
		as type symtab*;

	global->kind = symtab_kind::GLOBAL;
	global->name = NULL as type lex::token*;
	global->parent = NULL as type symtab*;
	global->symtab_info = symtab_info_global_init();
	global->global = global;

	global->unnamed_children = util::vector_init(sizeof{type symtab*},
		deref_symtab_free);
	global->symbols = symtab_ht_init();

	return global;
}

func type symtab* symtab_init(unsigned int k, type lex::token* n,
	type symtab* g, type symtab* p) {
	type symtab* s = n_malloc(sizeof{type symtab})
		as type symtab*;

	s->kind = k;
	s->name = n;
	s->parent = p;
	s->global = g;
	s->symtab_info = NULL as type symtab_info*;

	s->unnamed_children = util::vector_init(sizeof{type symtab*},
		deref_symtab_free);
	s->symbols = symtab_ht_init();

	return s;
}

func void symtab_delete(type symtab* s) {
	util::vector_delete(s->unnamed_children);
	util::ht_delete(s->symbols);

	if (s->symtab_info != NULL as type symtab_info*)
		symtab_info_delete(s->symtab_info);

	n_free(s as byte*);
}

func void deref_symtab_free(byte* b) {
	type symtab* s = b as type symtab** @;
	symtab_delete(s);
}

struct symtab_lookup_helper {
	type symtab* ctx;
	type symtab_value* val;
}

func[static] void symtab_lookup_qualified_helper(
	type symtab* ctx, type util::vector* n, unsigned int index,
	type ast::metadata* m, type symtab_lookup_helper* slh,
	bool limit, bool skip_from_lambda) {
	if (limit && index >= util::vector_size(n) - 1) {
		slh->ctx = ctx;
		slh->val = NULL as type symtab_value*;
		return;
	}

	type lex::token* curr = util::vector_at(n, index)
		as type lex::token** @;
	unsigned int token_index = qident2token_index(n, index);

	type symtab_value* res = NULL as type symtab_value*;
	if (!util::ht_get(ctx->symbols, curr->text as byte*,
		res$ as byte**)) {
		if (m != NULL as type ast::metadata*) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Could not find symbol here during a qualified name lookup.",
				m, token_index, token_index, true);
		}
		slh->ctx = NULL as type symtab*;
		slh->val = NULL as type symtab_value*;
		return;
	}

	if (res->kind == symtab_value_kind::TYPE_ALIAS && index != util::vector_size(n) - 1) {
		type ast::typ* nested_typ = res->typ;

		if (nested_typ->kind != ast::typ_kind::AGGREGATE_NAME
			|| !nested_typ->typ_qualifiers->none_specified
			|| is_pointer(nested_typ)) {
			if (m != NULL as type ast::metadata*) {
				util::report_ast_metadata(util::error_kind::ERROR,
					"Expected a scoped type alias with no type qualifiers as part of a qualified name.",
					m, token_index, token_index, true);
				util::report_token(util::error_kind::NOTE,
					"Symbol requested found here.", res->name);
			}
			slh->ctx = NULL as type symtab*;
			slh->val = NULL as type symtab_value*;
			return;
		}

		type ast::qualified_identifier* tmp = nested_typ->which.aggregate_name;

		type symtab_value* tmp_sv = symtab_lookup_qualified(tmp->ref_ctx, tmp->full_name,
			NULL as type ast::metadata*, skip_from_lambda);

		util::maybe_ice(tmp_sv != NULL as type symtab_value*,
			"symtab_lookup_qualified_helper",
			"Expected a valid type for a type alias here!");

		res = tmp_sv;
	}

	if (index == util::vector_size(n) - 1) {
		if (res->ast_node_kind == symtab_value_ast_node_kind::MEMBER) {
			if (m != NULL as type ast::metadata*) {
				util::report_ast_metadata(util::error_kind::ERROR,
					"Cannot use a qualified name to access an aggregate member.",
					m, token_index, token_index, true);
				util::report_token(util::error_kind::NOTE,
					"Symbol requested found here.", res->name);
			}
			slh->ctx = NULL as type symtab*;
			slh->val = NULL as type symtab_value*;
			return;
		}
		slh->ctx = ctx;
		slh->val = res;
	}
	else {
		type symtab* nctx = res->members;
		if (res->kind != symtab_value_kind::NAMESPACE
			&& res->kind != symtab_value_kind::STRUCT
			&& res->kind != symtab_value_kind::UNION
			&& res->kind != symtab_value_kind::VARIANT
			&& res->kind != symtab_value_kind::ENUM) {
			if (m != NULL as type ast::metadata*) {
				util::report_ast_metadata(util::error_kind::ERROR,
					"Expected a scoped symbol as part of a qualified name.",
					m, token_index, token_index, true);
				util::report_token(util::error_kind::NOTE,
					"Symbol requested found here.", res->name);
			}
			slh->ctx = NULL as type symtab*;
			slh->val = NULL as type symtab_value*;
			return;
		}
		else if (res->ast_node_kind == symtab_value_ast_node_kind::MEMBER) {
			if (m != NULL as type ast::metadata*) {
				util::report_ast_metadata(util::error_kind::ERROR,
					"Cannot use a qualified name to access an aggregate member.",
					m, token_index, token_index, true);
				util::report_token(util::error_kind::NOTE,
					"Symbol requested found here.", res->name);
			}
			slh->ctx = NULL as type symtab*;
			slh->val = NULL as type symtab_value*;
			return;
		}
		else if (res->fwd) {
			if (m != NULL as type ast::metadata*) {
				util::report_ast_metadata(util::error_kind::ERROR,
					"Cannot do a qualified lookup into a forward-declared scope.",
					m, token_index, token_index, true);
			}
			slh->ctx = NULL as type symtab*;
			slh->val = NULL as type symtab_value*;
			return;
		}
		symtab_lookup_qualified_helper(nctx, n, index + 1, m, slh, limit, skip_from_lambda);
	}
}

func[static] void symtab_lookup_unqualified_helper(type symtab* ctx,
	type lex::token* n, bool restrict, type symtab_lookup_helper* slh,
	bool skip_from_lambda);

func[static] type symtab_value* symtab_value_initial_lookup(
	type symtab* ctx, type lex::token* initial, type ast::metadata* m,
	bool last, bool skip_from_lambda) {
	type symtab_lookup_helper tmp_slh;
	symtab_lookup_unqualified_helper(ctx, initial, false, tmp_slh$, skip_from_lambda);
	type symtab_value* first_lookup = tmp_slh.val;

	if (first_lookup == NULL as type symtab_value*) {
		if (m != NULL as type ast::metadata*) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Could not find symbol here during a qualified name lookup.",
				m, 0, 0, true);
		}
		return NULL as type symtab_value*;
	}

	if (first_lookup->kind == symtab_value_kind::TYPE_ALIAS && !last) {
		type ast::typ* nested_typ = first_lookup->typ;

		if (nested_typ->kind != ast::typ_kind::AGGREGATE_NAME
			|| !nested_typ->typ_qualifiers->none_specified
			|| is_pointer(nested_typ)) {
			if (m != NULL as type ast::metadata*) {
				util::report_ast_metadata(util::error_kind::ERROR,
					"Expected a scoped type alias with no type qualifiers as part of a qualified name.",
					m, 0, 0, true);
				util::report_token(util::error_kind::NOTE,
					"Symbol requested found here.", first_lookup->name);
			}
			return NULL as type symtab_value*;
		}

		type ast::qualified_identifier* tmp = nested_typ->which.aggregate_name;

		type symtab_value* tmp_sv = symtab_lookup_qualified(tmp->ref_ctx, tmp->full_name,
			NULL as type ast::metadata*, skip_from_lambda);

		util::maybe_ice(tmp_sv != NULL as type symtab_value*,
			"symtab_value_initial_lookup",
			"Expected a valid type for a type alias here! ");

		first_lookup = tmp_sv;
	}

	if (!last) {
		if (first_lookup->kind != symtab_value_kind::NAMESPACE
			&& first_lookup->kind != symtab_value_kind::STRUCT
			&& first_lookup->kind != symtab_value_kind::UNION
			&& first_lookup->kind != symtab_value_kind::VARIANT
			&& first_lookup->kind != symtab_value_kind::ENUM) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Expected a scoped symbol as part of a qualified name.",
				m, 0, 0, true);
			util::report_token(util::error_kind::NOTE,
				"Symbol requested found here.", first_lookup->name);
			return NULL as type symtab_value*;
		}
		else if (first_lookup->ast_node_kind == symtab_value_ast_node_kind::MEMBER) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot use a qualified name to access an aggregate member.",
				m, 0, 0, true);
			util::report_token(util::error_kind::NOTE,
				"Symbol requested found here.", first_lookup->name);
			return NULL as type symtab_value*;
		}

		if (first_lookup->fwd) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot do a qualified lookup into a forward-declared scope.",
				m, 0, 0, true);
			return NULL as type symtab_value*;
		}
	}

	return first_lookup;
}

func type symtab_value* symtab_lookup_qualified(type symtab* ctx,
	type util::vector* n, type ast::metadata* m, bool skip_from_lambda) {
	type lex::token* initial = util::vector_at(n, 0)
		as type lex::token** @;

	type symtab* start_ctx = ctx->global;

	if (initial != NULL as type lex::token*) {
		type symtab_value* first_res = symtab_value_initial_lookup(
			ctx, initial, m, util::vector_size(n) == 1, skip_from_lambda);
		if (first_res  == NULL as type symtab_value*)
			return NULL as type symtab_value*;

		if (util::vector_size(n) == 1)
			return first_res;
		start_ctx = first_res->members;
	}

	type symtab_lookup_helper slh;
	symtab_lookup_qualified_helper(start_ctx, n, 1, m, slh$, false, skip_from_lambda);
	return slh.val;
}

func type symtab* symtab_lookup_qualified_ctx(type symtab* ctx,
	type util::vector* n, type ast::metadata* m, bool skip_from_lambda) {
	type lex::token* initial = util::vector_at(n, 0)
		as type lex::token** @;

	type symtab* start_ctx = ctx->global;

	if (initial != NULL as type lex::token*) {
		type symtab_value* first_res = symtab_value_initial_lookup(
			ctx, initial, m, util::vector_size(n) == 1, skip_from_lambda);
		if (first_res == NULL as type symtab_value*)
			return NULL as type symtab*;

		type symtab_lookup_helper tmp_slh;
		symtab_lookup_unqualified_helper(ctx, initial, false, tmp_slh$,
			skip_from_lambda);
		start_ctx = tmp_slh.ctx;

		if (util::vector_size(n) == 1) {
			if (tmp_slh.ctx == NULL as type symtab*
				|| tmp_slh.val == NULL as type symtab_value*) {
				return start_ctx;
			}
			return tmp_slh.val->enclosing;
		}
		start_ctx = first_res->members;
	}

	type symtab_lookup_helper slh;
	symtab_lookup_qualified_helper(start_ctx, n, 1, m, slh$, true, skip_from_lambda);
	if (slh.ctx == NULL as type symtab* || slh.val == NULL as type symtab_value*)
		return slh.ctx;
	return slh.val->enclosing;
}

func[static] void symtab_lookup_unqualified_helper(type symtab* ctx,
	type lex::token* n, bool restrict, type symtab_lookup_helper* slh,
	bool skip_from_lambda) {
	type symtab* iter = ctx;
	while (iter != NULL as type symtab*) {
		type symtab_value* res = NULL as type symtab_value*;
		bool found = util::ht_get(iter->symbols, n->text as byte*,
			res$ as byte**);
		if (restrict || found) {
			slh->ctx = found ? iter : NULL as type symtab*;
			slh->val = res;
			return;
		}

		if (iter->kind == symtab_kind::LAMBDA && skip_from_lambda) {
			while (iter != NULL as type symtab*
				&& iter->kind != symtab_kind::FUN) {
				iter = iter->parent;
			}

			if (iter == NULL as type symtab*)
				break;

			if (iter->kind == symtab_kind::FUN)
				iter = iter->parent;
		}
		else
			iter = iter->parent;
	}

	slh->ctx = NULL as type symtab*;
	slh->val = NULL as type symtab_value*;
}

func type symtab_value* symtab_lookup_unqualified(type symtab* ctx,
	type lex::token* n, bool restrict, bool skip_from_lambda) {
	type symtab_lookup_helper slh;
	symtab_lookup_unqualified_helper(ctx, n, restrict, slh$, skip_from_lambda);
	return slh.val;
}

func type symtab* symtab_lookup_unqualified_ctx(type symtab* ctx,
	type lex::token* n, bool restrict, bool skip_from_lambda) {
	type symtab_lookup_helper slh;
	symtab_lookup_unqualified_helper(ctx, n, restrict, slh$, skip_from_lambda);
	if (slh.ctx == NULL as type symtab* || slh.val == NULL as type symtab_value*)
		return slh.ctx;
	return slh.val->enclosing;
}

func void to_fully_qualified(type symtab* ctx,
	type ast::qualified_identifier* qi) {
	qi->ref_ctx = ctx;
	qi->full_name = util::vector_init(sizeof{type lex::token*},
		util::no_free);

	type lex::token* initial = util::vector_at(qi->name, 0)
		as type lex::token** @;
	if (initial == NULL as type lex::token*) {
		for (unsigned int i = 0; i < util::vector_size(qi->name); i++) {
			type lex::token* curr = util::vector_at(qi->name, i)
				as type lex::token** @;

			util::vector_append(qi->full_name, curr$ as byte*);
		}
		return;
	}

	type util::vector* tmp = util::vector_init(sizeof{type lex::token*},
		util::no_free);
	bool local = false;
	type symtab* ctx_iter = ctx;
	while (ctx_iter != NULL as type symtab*) {
		if (ctx_iter->kind != symtab_kind::NAMESPACE
			&& ctx_iter->kind != symtab_kind::STRUCT
			&& ctx_iter->kind != symtab_kind::UNION
			&& ctx_iter->kind != symtab_kind::VARIANT
			&& ctx_iter->kind != symtab_kind::ENUM
			&& ctx_iter->kind != symtab_kind::GLOBAL) {
			local = true;
			break;
		}

		util::vector_append(tmp, ctx_iter->name$ as byte*);

		ctx_iter = ctx_iter->parent;
	}

	if (!local) {
		for (unsigned int i = util::vector_size(tmp); i > 0; i--) {
			type lex::token* curr = util::vector_at(tmp, i - 1)
				as type lex::token** @;

			util::vector_append(qi->full_name, curr$ as byte*);
		}
	}
	util::vector_delete(tmp);

	util::maybe_ice(util::vector_size(qi->name) > 0,
		"to_fully_qualified",
		"Expected at least one identifier in a qualified identifier name!");

	type lex::token* last = util::vector_at(qi->name, util::vector_size(qi->name) - 1)
		as type lex::token** @;
	util::vector_append(qi->full_name, last$ as byte*);
}

} } // namespace neutrino::tck
