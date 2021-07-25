import "tck/tck.hsp"

import <"std/io">
import <"std/lib">

import "ast/ast.hsp"
import "util/error.hsp"
import "tck/symtab.hsp"
import "util/vector.hsp"
import "lex/token.hsp"
import "util/generic_funcs.hsp"
import "util/base.hsp"
import "util/hash_table.hsp"
import "tck/util.hsp"
import "parse/util.hsp"

using std::io::printf;
using std::lib::NULL;
using neutrino::util::n_strdup;
using neutrino::util::n_malloc;

namespace neutrino { namespace tck {

func[static] bool tck_aggregate_helper(type symtab* ctx,
	type symtab* su_ctx, type ast::aggregate* a, bool is_variant,
	type symtab_value* sv) {
	type util::hash_table* a_mem_ht = util::ht_init(
		util::str_hash, util::str_eq,
		util::no_free, util::no_free);

	for (unsigned int i = 0; i < util::vector_size(a->members); i++) {
		type ast::aggregate_member* am = util::vector_at(a->members, i)
			as type ast::aggregate_member** @;

		util::maybe_ice(!util::vector_empty(am->idents),
			"tck_aggregate_helper",
			"Expected a non-empty member, identifier list here!");
		type lex::token* initial_ident = util::vector_at(am->idents, 0)
			as type lex::token** @;
		bool is_underscore = initial_ident->kind == lex::token_kind::UNDERSCORE;
		if (is_underscore) {
			util::maybe_ice(util::vector_size(am->idents) == 1,
				"tck_aggregate_helper",
				"For an empty member declaration, expected only a single member in the member list.");

			if (is_variant) {
				util::report_ast_metadata(util::error_kind::ERROR,
					"Cannot have an empty member in a variant.",
					am->metadata, 0, 0, true);
				return false;
			}

			if (am->kind != ast::aggregate_member_kind::AGGREGATE) {
				util::report_ast_metadata(util::error_kind::ERROR,
					"An empty member must introduce an aggregate.",
					am->metadata, 0, 0, true);
				return false;
			}
		}

		type ast::typ* mem_typ = NULL as type ast::typ*;
		switch (am->kind) {
		case ast::aggregate_member_kind::NONE: {
			if (!is_variant) {
				unsigned int token_index
					= (am->metadata->end - am->metadata->start) - 1;
				util::report_ast_metadata(util::error_kind::ERROR,
					"Cannot have a nullary member in a non-variant aggregate.",
					am->metadata, token_index, token_index, true);
				return false;
			}
		}
			break;
		case ast::aggregate_member_kind::TYP: {
			type symtab* ctx_to_use = is_variant ? ctx : su_ctx;
			type ast::typ* curr_typ = tck_typ(ctx_to_use, am->which.typ);
			if (curr_typ == NULL as type ast::typ*)
				return false;

			mem_typ = curr_typ;
		}
			break;
		case ast::aggregate_member_kind::AGGREGATE: {
			type ast::aggregate* na = am->which.aggregate->which.aggregate;

			if (is_variant) {
				if (na->name != NULL as type lex::token*
					|| na->kind != ast::aggregate_kind::STRUCT) {
					util::report_ast_metadata(util::error_kind::ERROR,
						"Expected an anonymous struct for a variant member declaration.",
						na->metadata, 0, 0, true);
					return false;
				}
			}
			else {
				if (na->name == NULL as type lex::token*
					&& !is_underscore) {
					type ast::metadata m;
					m.token_stream = na->metadata->token_stream;
					m.start = na->metadata->start;
					m.end = na->metadata->start + 2;

					util::report_ast_metadata(util::error_kind::ERROR,
						"Cannot have an anonymous struct/union as the type of a member.",
						m$, 0, 0, false);
					return false;
				}

				if (!tck_aggregate(su_ctx, na))
					return false;

				if (na->name != NULL as type lex::token*) {
					type symtab_value* sv = symtab_lookup_unqualified(su_ctx, na->name, true, true);

					util::maybe_ice(sv != NULL as type symtab_value*,
						"tck_aggregate_helper",
						"Expected to find the aggregate member at this point!");

					mem_typ = sv->typ;
				}
			}
		}
			break;
		default:
			util::ice("tck_aggregate_helper",
				"Unrecognized aggregate_member_kind while tck'ing!");
		}

		if (!is_underscore)
			am->symtab_values = util::vector_init(sizeof{type symtab_value*}, util::no_free);

		for (unsigned int j = 0; j < util::vector_size(am->idents) && !is_underscore; j++) {
			type lex::token* curr_ident = util::vector_at(am->idents, j)
				as type lex::token** @;

			if (am->attribute != NULL as type ast::attribute*) {
				type util::hash_table* attrs = tck_attribute(ctx, ast::attribute_kind::MEMBER,
					am->attribute);
				if (attrs == NULL as type util::hash_table*)
					return false;
				am->attribute->attrs = attrs;
			}

			type lex::token* prev_ident;
			if (util::ht_get(a_mem_ht, curr_ident->text as byte*, prev_ident$ as byte**)) {
				util::report_token(util::error_kind::ERROR,
					"Duplicate aggregate member found here.",
					curr_ident);
				util::report_token(util::error_kind::NOTE,
					"Originally declared here.",
					prev_ident);
				return false;
			}

			util::ht_set(a_mem_ht, curr_ident->text as byte*, curr_ident as byte*);

			type symtab_value* check = symtab_lookup_unqualified(
				is_variant ? ctx : su_ctx, curr_ident, true, true);
			if (check != NULL as type symtab_value*) {
				util::report_token(util::error_kind::ERROR,
					"Aggregate member name conflicts with existing name in parent scope.",
					curr_ident);
				util::report_token(util::error_kind::NOTE,
					"Originally declared here.", check->name);
				return false;
			}

			if (am->kind == ast::aggregate_member_kind::NONE) {
				util::maybe_ice(is_variant,
					"tck_aggregate_helper",
					"Expected a nullary member as part of a variant at this point!");

				util::maybe_ice(sv != NULL as type symtab_value*,
					"tck_aggregate_helper",
					"For a nullary variant member, expected a non-NULL symtab_value here!");

				type ast::typ* tcopy = typ_copy(sv->typ);
				set_mut(tcopy->typ_qualifiers,  true);

				type symtab_value* nc = symtab_value_nullary_variant_init(ctx,
					false, false, curr_ident, tcopy, curr_ident);

				util::vector_append(am->symtab_values, nc$ as byte*);

				char* cname = n_strdup(curr_ident->text);
				util::ht_set(ctx->symbols, cname as byte*, nc as byte*);
			}
			else {
				if (!is_variant) {
					util::maybe_ice(mem_typ != NULL as type ast::typ*,
						"tck_aggregate_helper",
						"Expected a non-NULL struct/union member type at this point!");

					type ast::typ* tmp = typ_copy(mem_typ);
					set_mut(tmp->typ_qualifiers, true);

					type symtab_value* sv = symtab_value_member_init(su_ctx, false,
						false, curr_ident, tmp, curr_ident);

					util::vector_append(am->symtab_values, sv$ as byte*);

					char* cname = n_strdup(curr_ident->text);
					util::ht_set(su_ctx->symbols, cname as byte*, sv as byte*);
				}
				else {
					util::maybe_ice(sv != NULL as type symtab_value*,
						"tck_aggregate_helper",
						"For an aggregate or typed variant member, expected a non-NULL symtab_value here!");

					if (am->kind == ast::aggregate_member_kind::AGGREGATE) {
						type ast::aggregate* na = am->which.aggregate->which.aggregate;
						util::maybe_ice(na->kind == ast::aggregate_kind::STRUCT,
							"tck_aggregate_helper",
							"Expected a struct aggregate for a variant member!");

						util::maybe_ice(na->name == NULL as type lex::token*,
							"tck_aggregate_helper",
							"Expected an anonymous struct for an aggregate variant member!");

						type util::vector* tmp = util::vector_init(sizeof{type lex::token*},
							util::no_free);
						util::vector_append(tmp, curr_ident$ as byte*);

						type ast::qualified_identifier* qi =
							ast::qualified_identifier_init(tmp, NULL as type ast::metadata*);
						to_fully_qualified(su_ctx, qi);

						type ast::typ* st = ast::typ_aggregate_name_init(
							mut_non_pointer_typ_qualifiers_init(),
							qi, NULL as type ast::metadata*);

						type symtab_value* ssv = symtab_value_aggregate_init(su_ctx, false,
							false, symtab_value_kind::STRUCT, curr_ident, st, na);
						ssv->members = symtab_init(symtab_kind::STRUCT, curr_ident,
							su_ctx->global, su_ctx);
						if (!tck_aggregate(ssv->members, na))
							return false;
						na->symtab_value = ssv;

						util::maybe_ice(
							symtab_lookup_unqualified(su_ctx, curr_ident, true, true) == NULL as type symtab_value*,
							"tck_aggregate_helper",
							"Expected a non-conflicting member name here!");

						char* scname = n_strdup(curr_ident->text);
						util::ht_set(su_ctx->symbols, scname as byte*, ssv as byte*);

						type ast::typ* rt = typ_copy(sv->typ);
						set_mut(rt->typ_qualifiers, true);
						type util::vector* args = util::vector_init(sizeof{type ast::typ*}, parse::deref_typ_free);
						type ast::fn_typ* vcft = ast::fn_typ_init(args, false, rt);

						type ast::typ_qualifiers* tqs = mut_non_pointer_typ_qualifiers_init();
						type ast::typ* final_typ = ast::typ_fn_typ_init(tqs, vcft,
							NULL as type ast::metadata*);

						type ast::typ* cst = typ_copy(st);
						set_mut(cst->typ_qualifiers, true);
						util::vector_append(args, cst$ as byte*);

						type symtab_value* vcsv = symtab_value_variant_constructor_init(ctx, false,
							false, curr_ident, final_typ, curr_ident);

						util::vector_append(am->symtab_values, vcsv$ as byte*);

						char* vccname = n_strdup(curr_ident->text);
						util::ht_set(ctx->symbols, vccname as byte*, vcsv as byte*);
					}
					else if (am->kind == ast::aggregate_member_kind::TYP) {
						util::maybe_ice(mem_typ != NULL as type ast::typ*,
							"tck_aggregate_helper",
							"Expected a non-NULL type for a variant member constructor at this pointer!");

						type ast::typ* rt = typ_copy(sv->typ);
						set_mut(rt->typ_qualifiers, true);
						type util::vector* args = util::vector_init(sizeof{type ast::typ*}, parse::deref_typ_free);
						type ast::fn_typ* ft = ast::fn_typ_init(args, false, rt);

						type ast::typ_qualifiers* tqs = mut_non_pointer_typ_qualifiers_init();
						type ast::typ* final_typ = ast::typ_fn_typ_init(tqs,
							ft, NULL as type ast::metadata*);

						if (is_primitive(mem_typ, ast::primitive_kind::VOID)) {}
						else if (mem_typ->kind == ast::typ_kind::TUP &&
							!is_pointer(mem_typ)) {
							type util::vector* tt = mem_typ->which.tup_typ;

							for (unsigned int i = 0; i < util::vector_size(tt); i++) {
								type ast::typ* ct = util::vector_at(tt, i)
									as type ast::typ** @;

								type ast::typ* cct = typ_copy(ct);
								set_mut(cct->typ_qualifiers, true);
								util::vector_append(args, cct$ as byte*);
							}
						}
						else {
							type ast::typ* cmt = typ_copy(mem_typ);
							set_mut(cmt->typ_qualifiers, true);
							util::vector_append(args, cmt$ as byte*);
						}

						type symtab_value* sv = symtab_value_variant_constructor_init(ctx, false,
							false, curr_ident, final_typ, curr_ident);

						util::vector_append(am->symtab_values, sv$ as byte*);

						char* cname = n_strdup(curr_ident->text);
						util::ht_set(ctx->symbols, cname as byte*, sv as byte*);
					}
					else {
						util::ice("tck_aggregate_helper",
							"This should be unreachable!");
					}
				}
			}
		}
	}

	util::ht_delete(a_mem_ht);

	return true;
}

func bool tck_aggregate(type symtab* ctx, type ast::aggregate* a) {
	bool is_anon = a->name == NULL as type lex::token*;
	bool is_fwd = a->members == NULL as type util::vector*;

	bool is_struct = false, is_union = false, is_variant = false, is_enum = false;
	unsigned int svk = 0, sk = 0, ak = 0;
	switch (a->kind) {
	case ast::aggregate_kind::STRUCT:
		svk = symtab_value_kind::STRUCT;
		sk = symtab_kind::STRUCT;
		ak = ast::attribute_kind::STRUCT;
		is_struct = true;
		break;
	case ast::aggregate_kind::UNION:
		svk = symtab_value_kind::UNION;
		sk = symtab_kind::UNION;
		ak = ast::attribute_kind::UNION;
		is_union = true;
		break;
	case ast::aggregate_kind::VARIANT:
		svk = symtab_value_kind::VARIANT;
		sk = symtab_kind::VARIANT;
		ak = ast::attribute_kind::VARIANT;
		is_variant = true;
		break;
	case ast::aggregate_kind::ENUM:
		svk = symtab_value_kind::ENUM;
		sk = symtab_kind::ENUM;
		ak = ast::attribute_kind::ENUM;
		is_enum = true;
		break;
	default:
		util::ice("tck_aggregate", "This should be unreachable!");
	}

	if (a->attribute != NULL as type ast::attribute*) {
		type util::hash_table* attrs = tck_attribute(ctx, ak, a->attribute);
		if (attrs == NULL as type util::hash_table*)
			return false;
		a->attribute->attrs = attrs;
	}

	type symtab* a_ctx = NULL as type symtab*;
	type symtab_value* sv = NULL as type symtab_value*;
	if (!is_anon) {
		sv = symtab_lookup_unqualified(ctx, a->name, true, true);

		if (sv != NULL as type symtab_value*) {
			if ((!sv->fwd && !is_fwd)
				|| (is_struct && sv->kind != symtab_value_kind::STRUCT)
				|| (is_union && sv->kind != symtab_value_kind::UNION)
				|| (is_variant && sv->kind != symtab_value_kind::VARIANT)
				|| (is_enum && sv->kind != symtab_value_kind::ENUM)) {
				util::report_token(util::error_kind::ERROR,
					"Duplicate symbol found here.", a->name);
				util::report_token(util::error_kind::NOTE,
					"Originally declared here.", sv->name);
				return false;
			}

			if (!attribute_compare(a->attribute, sv->ast_node.aggregate->attribute)) {
				util::report_token(util::error_kind::ERROR,
					"Incompatible attribute specification between the current declaration and the original declaration.",
					a->name);

				util::report_token(util::error_kind::NOTE,
					"Originally declared here.",
					sv->name);
				return false;
			}
		}
		else {
			type util::vector* tmp = util::vector_init(sizeof{type lex::token*},
				util::no_free);
			util::vector_append(tmp, a->name$ as byte*);

			type ast::qualified_identifier* qi =
				ast::qualified_identifier_init(tmp, NULL as type ast::metadata*);
			to_fully_qualified(ctx, qi);

			type ast::typ* t = ast::typ_aggregate_name_init(
				mut_non_pointer_typ_qualifiers_init(),
				qi, NULL as type ast::metadata*);
			sv = symtab_value_aggregate_init(ctx, is_fwd, false,
				svk, a->name, t, a);
			if (a->attribute != NULL as type ast::attribute*)
				sv->attrs = a->attribute->attrs;

			char* caname = n_strdup(a->name->text);
			util::ht_set(ctx->symbols, caname as byte*, sv as byte*);
		}

		if (is_fwd) return true;

		sv->fwd = false;

		util::maybe_ice(sv->members == NULL as type symtab*,
			"tck_aggregate", "Expected a NULL child context for an aggregate at this point!");

		sv->members = symtab_init(sk, a->name, ctx->global, ctx);

		a_ctx = sv->members;
	}
	else {
		util::maybe_ice(!is_fwd,
			"tck_aggregate",
			"A forward-declared anonymous aggregate should be impossible here!");
		util::maybe_ice(!is_enum,
			"tck_aggregate",
			"Cannot have anonymous enums!");

		if (is_variant) {
			type ast::metadata m;
			m.token_stream = a->metadata->token_stream;
			m.start = a->metadata->start;
			m.end = a->metadata->start + 2;

			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot declare an anonymous 'variant'.",
				m$, 0, 0, false);
			return false;
		}

		if (a->attribute != NULL as type ast::attribute*) {
			if (util::ht_contains(a->attribute->attrs, ast::attribute_value_kind::PUBLIC as byte*)) {
				util::report_ast_metadata(util::error_kind::ERROR,
					"Cannot declare an anonymous struct or union as 'public'.",
					a->attribute->metadata, 0, 0, false);
				return false;
			}
		}

		a_ctx = ctx;
	}

	if (!is_enum) {
		if (sv != NULL as type symtab_value*)
			sv->fwd = true;
		bool ret = tck_aggregate_helper(ctx, a_ctx, a, is_variant, sv);
		if (sv != NULL as type symtab_value*)
			sv->fwd = false;
		a->ctx = a_ctx;

		if (sv != NULL as type symtab_value*)
			a->symtab_value = sv;

		return ret;
	}

	util::maybe_ice(a->name != NULL as type lex::token*
		&& sv != NULL as type symtab_value*,
		"tck_aggregate",
		"Expected a name while trying to process an enum here (anonymous 'enum''s are not allowed)!");

	a->enum_symtab_values = util::vector_init(sizeof{type symtab_value*}, util::no_free);
	a->symtab_value = sv;

	for (unsigned int i = 0; i < util::vector_size(a->members); i++) {
		type lex::token* curr = util::vector_at(a->members, i)
			as type lex::token** @;

		type symtab_value* check = symtab_lookup_unqualified(a_ctx, curr, true, true);
		if (check != NULL as type symtab_value*) {
			util::report_token(util::error_kind::ERROR,
				"Duplicate symbol found here.", curr);
			util::report_token(util::error_kind::NOTE,
				"Originally declared here.", check->name);

			return false;
		}

		type ast::typ* et = typ_copy(sv->typ);
		set_mut(et->typ_qualifiers, false);
		type symtab_value* csv = symtab_value_enum_symbol_init(a_ctx, false,
			false, curr, et, curr);
		csv->is_constant = true;

		util::vector_append(a->enum_symtab_values, csv$ as byte*);

		char* cesname = n_strdup(curr->text);
		util::ht_set(a_ctx->symbols, cesname as byte*, csv as byte*);
	}
	a->ctx = a_ctx;

	return true;
}

} } // namespace neutrino::tck
