import "tck/tck.hsp"

import <"std/io">
import <"std/lib">

import "ast/ast.hsp"
import "util/vector.hsp"
import "util/error.hsp"
import "tck/symtab.hsp"
import "util/base.hsp"
import "tck/util.hsp"

using std::lib::NULL;
using std::io::printf;
using neutrino::util::n_strdup;

namespace neutrino { namespace tck {

func bool tck_top_level(type symtab* ctx, type ast::top_level* tl) {
	switch (tl->kind) {
	case ast::top_level_kind::DECL: {
		type ast::decl* d = tl->which.stmt->which.decl;
		if (!tck_decl(ctx, d, true))
			return false;
	}
		break;
	case ast::top_level_kind::NAMESPACE: {
		type ast::namespace_decls* nd = tl->which.namespace_decls;
		type lex::token* initial = util::vector_at(nd->name->name, 0)
			as type lex::token** @;

		if (initial == NULL as type lex::token*) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Expected a non-globally qualified name for a namespace here.",
				nd->name->metadata, 0, 0, true);
			return false;
		}

		if (nd->attribute != NULL as type ast::attribute*) {
			type util::hash_table* attrs = tck_attribute(ctx, ast::attribute_kind::NAMESPACE, nd->attribute);
			if (attrs == NULL as type util::hash_table*)
				return false;
			nd->attribute->attrs = attrs;
		}

		type symtab* iter = ctx;

		bool is_fwd = nd->decls == NULL as type util::vector*;
		for (unsigned int i = 0; i < util::vector_size(nd->name->name); i++) {
			type lex::token* curr = util::vector_at(nd->name->name, i)
				as type lex::token** @;

			bool curr_is_fwd = is_fwd;
			if (i != util::vector_size(nd->name->name) - 1)
				curr_is_fwd = false;

			type symtab_value* sv = symtab_lookup_unqualified(iter, curr, true, true);
			if (sv == NULL as type symtab_value*) {
				char* ccurr = n_strdup(curr->text);
				type symtab_value* nsv = symtab_value_namespace_init(iter,
					curr_is_fwd, false, curr, NULL as type ast::typ*, nd);

				nsv->members = symtab_init(symtab_kind::NAMESPACE, curr, iter->global, iter);
				util::ht_set(iter->symbols, ccurr as byte*, nsv as byte*);

				if (nd->attribute != NULL as type ast::attribute*)
					nsv->attrs = nd->attribute->attrs;

				if (i == util::vector_size(nd->name->name) - 1) {
					to_fully_qualified(ctx, nd->name);
					nd->name->symtab_value = nsv;
				}

				iter = nsv->members;
			}
			else {
				if (sv->kind != symtab_value_kind::NAMESPACE) {
					unsigned int token_index = qident2token_index(nd->name->name, i);
					util::report_ast_metadata(util::error_kind::ERROR,
						"Expected a namespace symbol as part of a qualified identifier here.", nd->name->metadata,
						token_index, token_index, true);
					util::report_token(util::error_kind::NOTE,
						"Originally declared here.", sv->name);
					return false;
				}
				if (!curr_is_fwd && sv->fwd)
					sv->fwd = false;

				if (!attribute_compare(nd->attribute, sv->ast_node.namespace_decls->attribute)) {
					unsigned int token_index = qident2token_index(nd->name->name, i);
					util::report_ast_metadata(util::error_kind::ERROR,
						"Incompatible attribute specifications between the current namespace declaration and the original declaration.",
						nd->name->metadata, token_index, token_index, true);

					util::report_token(util::error_kind::NOTE,
						"Originally declared here.",
						sv->name);
					return false;
				}

				if (i == util::vector_size(nd->name->name) - 1) {
					to_fully_qualified(ctx, nd->name);
					nd->name->symtab_value = sv;
				}

				iter = sv->members;
			}
		}

		if (is_fwd) break;

		for (unsigned int i = 0; i < util::vector_size(nd->decls); i++) {
			type ast::top_level* tl = util::vector_at(nd->decls, i)
				as type ast::top_level** @;
			if (!tck_top_level(iter, tl))
				return false;
		}
		nd->ctx = iter;
	}
		break;
	case ast::top_level_kind::FUN: {
		type ast::function* f = tl->which.function;
		type ast::typ* check = tck_function(ctx, f);
		if (check == NULL as type ast::typ*)
			return false;
		ast::typ_delete(check);
	}
		break;
	case ast::top_level_kind::AGGREGATE: {
		type ast::aggregate* a = tl->which.stmt->which.aggregate;
		if (!tck_aggregate(ctx, a))
			return false;
	}
		break;
	case ast::top_level_kind::USING:
	case ast::top_level_kind::USING_NAMESPACE: {
		type ast::stmt* s = tl->which.stmt;
		type ast::using_data* ud = s->which.using_data;

		type ast::qualified_identifier* from_name = ud->qualified_identifier;
		util::maybe_ice(!util::vector_empty(from_name->name),
			"tck_top_level",
			"Expected a valid qualified identifier for a using-directive here!");

		type lex::token* to_name = util::vector_at(from_name->name, util::vector_size(from_name->name) - 1)
			as type lex::token** @;
		if (!tck_alias(ctx, s->kind, to_name, from_name, ud->attribute, tl->metadata))
			return false;
	}
		break;
	case ast::top_level_kind::NAMESPACE_ALIAS: {
		type ast::stmt* s = tl->which.stmt;
		type ast::namespace_alias* na = s->which.namespace_alias;

		if (!tck_alias(ctx, s->kind, na->to_name, na->from_name, na->attribute, tl->metadata))
			return false;
	}
		break;
	case ast::top_level_kind::INCLUDE:
		break;
	case ast::top_level_kind::TYPE_ALIAS: {
		type ast::stmt* s = tl->which.stmt;
		type ast::type_alias* ta = s->which.type_alias;

		if (!tck_type_alias(ctx, ta))
			return false;
	}
		break;
	// TODO
	default:
		util::ice("tck_top_level",
			"Unrecognized top_level_kind while tck'ing!");
	}

	return true;
}

func bool tck_prog(type symtab* ctx, type ast::prog* p) {
	for (unsigned int i = 0; i < util::vector_size(p->top_levels); i++) {
		type ast::top_level* tl = util::vector_at(p->top_levels, i)
			as type ast::top_level** @;

		if (!tck_top_level(ctx, tl)) return false;
	}
	p->ctx = ctx;

	return true;
}

} } // namespace neutrino::tck
