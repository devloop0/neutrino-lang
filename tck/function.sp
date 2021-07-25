import "tck/tck.hsp"

import <"std/io">
import <"std/lib">

import "ast/ast.hsp"
import "util/error.hsp"
import "tck/symtab.hsp"
import "tck/util.hsp"
import "util/string.hsp"
import "util/hash_table.hsp"
import "util/generic_funcs.hsp"
import "util/vector.hsp"
import "parse/util.hsp"
import "util/base.hsp"
import "util/lib.hsp"

using std::io::printf;
using std::lib::NULL;
using neutrino::util::n_strdup;

namespace neutrino { namespace tck {

func[static] bool tck_function_helper_body(type symtab* fsymtab,
	type symtab_value* sv, type ast::function* f, type ast::typ* new_return_typ);

func[static] type ast::typ* tck_function_helper(type symtab* ctx, type ast::function* f,
	type symtab_value* sv, type ast::fn_typ* orig_ft, bool is_fwd,
	type ast::typ* new_return_typ) {
	bool is_lambda = f->name == NULL as type lex::token*;

	type symtab* fsymtab = symtab_init(
		is_lambda ? symtab_kind::LAMBDA : symtab_kind::FUN,
		is_lambda ? NULL as type lex::token* : f->name,
		ctx->global, ctx);
	type util::vector* par_typs = util::vector_init(sizeof{type ast::typ*},
		parse::deref_typ_free);

	for (unsigned int i = 0; i < util::vector_size(f->parameters); i++) {
		type ast::decl* par = util::vector_at(f->parameters, i)
			as type ast::decl** @;

		util::maybe_ice(util::vector_size(par->decl_components) == 1,
			"tck_function", "Expected a single decl_component for a function parameter!");

		type ast::decl_component* dc = util::vector_at(par->decl_components, 0)
			as type ast::decl_component** @;

		if (dc->init != NULL as type ast::exp*) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot have an initializer for a function parameter.",
				dc->metadata, 0, 0, false);
			return NULL as type ast::typ*;
		}

		if (dc->typ == NULL as type ast::typ*) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Function parameter must have its type explicitly specified.",
				dc->metadata, 0, 0, false);
			return NULL as type ast::typ*;
		}

		if (par->is_static || par->is_const) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot have a 'static' or 'const' declaration for a function parameter.",
				par->metadata, 0, 0, false);
			return NULL as type ast::typ*;
		}

		unsigned int prev_index;

		type ast::typ* tmp_dc_typ = typ_copy(dc->typ);
		type ast::typ* par_typ = tck_typ(ctx, tmp_dc_typ);
		if (par_typ == NULL as type ast::typ*)
			return NULL as type ast::typ*;
		ast::typ_delete(tmp_dc_typ);

		if (!tck_decl(fsymtab, par, false))
			return NULL as type ast::typ*;

		if (orig_ft != NULL as type ast::fn_typ*) {
			type ast::decl* orig_par = util::vector_at(sv->ast_node.function->parameters, i)
				as type ast::decl** @;
			type ast::decl_component* odc = util::vector_at(orig_par->decl_components, 0)
				as type ast::decl_component** @;
			type ast::typ* orig_typ = typ_copy(odc->typ);

			if (!typ_compare(ctx, par_typ, orig_typ, true, false)) {
				type util::string* err_str = util::string_init(
					"Incompatible types for function parameter #");
				util::utoustr(i + 1, err_str);
				util::string_catc(err_str, "; the current type specified is '");
				typ_human_readable(par_typ, err_str);
				util::string_catc(err_str, "' but the original type specified was '");
				typ_human_readable(orig_typ, err_str);
				util::string_catc(err_str, "'.");

				unsigned int curr_start_index = (par_typ->metadata->start - par->metadata->start);
				unsigned int curr_end_index = (par_typ->metadata->end - par->metadata->start) - 1;
				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					par->metadata, curr_start_index, curr_end_index, true);
				util::string_delete(err_str);
				
				err_str = util::string_init("Parameter #");
				util::utoustr(i + 1, err_str);
				util::string_catc(err_str, " originally specified here.");
				unsigned int orig_start_index = (orig_typ->metadata->start - orig_par->metadata->start);
				unsigned int orig_end_index = (orig_typ->metadata->end - orig_par->metadata->start) - 1;
				util::report_ast_metadata(util::error_kind::NOTE,
					util::string_data(err_str),
					orig_par->metadata, orig_start_index, orig_end_index, true);
				util::string_delete(err_str);

				return NULL as type ast::typ*;
			}

			ast::typ_delete(orig_typ);

			if (!attribute_compare(orig_par->attribute, par->attribute)) {
				type util::string* err_str = util::string_init("Incompatible attribute specification for function parameter #");
				util::utoustr(i + 1, err_str);
				util::string_catc(err_str, ".");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					par->metadata, 0, 0, false);
				util::string_delete(err_str);

				err_str = util::string_init("Parameter #");
				util::utoustr(i + 1, err_str);
				util::string_catc(err_str, " originally specified here.");
				util::report_ast_metadata(util::error_kind::NOTE,
					util::string_data(err_str),
					orig_par->metadata, 0, 0, false);
				util::string_delete(err_str);

				return NULL as type ast::typ*;
			}
		}

		util::vector_append(par_typs, par_typ$ as byte*);
	}

	if (!is_lambda) {
		if (orig_ft == NULL as type ast::fn_typ*) {
			type ast::typ_qualifiers* res_ft_tqs = mut_non_pointer_typ_qualifiers_init();
			type ast::fn_typ* res_ft = ast::fn_typ_init(
				par_typs, f->is_variadic, new_return_typ);
			type ast::typ* res_typ = ast::typ_fn_typ_init(res_ft_tqs,
				res_ft, NULL as type ast::metadata*);

			char* f_cname = n_strdup(f->name->text);
			type symtab_value* nsv =
				symtab_value_fun_init(ctx, is_fwd, false, f->name, res_typ, f);
			util::ht_set(ctx->symbols, f_cname as byte*, nsv as byte*);

			type util::vector* svs = NULL as type util::vector*;
			if (util::ht_get(ctx->global->symtab_info->which.ufcs_table, f_cname as byte*, svs$ as byte**))
				util::vector_append(svs, nsv$ as byte*);
			else {
				svs = util::vector_init(sizeof{type symtab_value*}, util::no_free);
				util::vector_append(svs, nsv$ as byte*);
				util::ht_set(ctx->global->symtab_info->which.ufcs_table,
					f_cname as byte*, svs as byte*);
			}

			sv = nsv;
		}
		else
			util::vector_delete(par_typs);

		f->symtab_value = sv;

		if (is_fwd) {
			symtab_delete(fsymtab);
			return typ_copy(sv->typ);
		}

		util::maybe_ice(f->capture_list == NULL as type util::vector*,
			"tck_function",
			"Expected a NULL capture list for a normal function!");

		if (!tck_function_helper_body(fsymtab, sv, f, new_return_typ))
			return NULL as type ast::typ*;

		return typ_copy(sv->typ);
	}
	else {
		static unsigned int counter = 0;

		type ast::typ* final_fn_typ = ast::typ_fn_typ_init(
			mut_non_pointer_typ_qualifiers_init(),
		 	ast::fn_typ_init(par_typs, f->is_variadic, new_return_typ),
			NULL as type ast::metadata*);

		if (f->capture_list != NULL as type util::vector*) {
			for (unsigned int i = 0; i < util::vector_size(f->capture_list); i++) {
				type ast::capture_data* cd = util::vector_at(f->capture_list, i)
					as type ast::capture_data** @;
				unsigned int ident_index = (cd->metadata->end - cd->metadata->start) - 1;

				type symtab_value* conflict_check = symtab_lookup_unqualified(fsymtab, cd->ident, true, true);
				if (conflict_check != NULL as type symtab_value*) {
					util::report_ast_metadata(util::error_kind::ERROR,
						"Capture list identifier conflicts with an existing name.",
						cd->metadata, ident_index, ident_index, true);
					util::report_token(util::error_kind::NOTE,
						"Originally declared here.", conflict_check->name);
					return NULL as type ast::typ*;
				}

				type symtab_value* sv = symtab_lookup_unqualified(ctx, cd->ident, false, true);
				type symtab* sv_ctx = symtab_lookup_unqualified_ctx(ctx, cd->ident, false, true);

				if (sv == NULL as type symtab_value*
					|| sv_ctx == NULL as type symtab*) {
					util::report_ast_metadata(util::error_kind::ERROR,
						"Could not find symbol here.",
						cd->metadata, ident_index, ident_index, true);
					return NULL as type ast::typ*;
				}

				if (sv->kind == symtab_value_kind::NAMESPACE
					|| sv->kind == symtab_value_kind::STRUCT
					|| sv->kind == symtab_value_kind::UNION
					|| sv->kind == symtab_value_kind::ENUM
					|| sv->kind == symtab_value_kind::VARIANT
					|| sv->kind == symtab_value_kind::TYPE_ALIAS) {
					util::report_ast_metadata(util::error_kind::ERROR,
						"Can only capture functions or variables.",
						cd->metadata, ident_index, ident_index, true);
					util::report_token(util::error_kind::NOTE,
						"Symbol in question declared here.", sv->name);
					return NULL as type ast::typ*;
				}

				if (sv_ctx->kind == symtab_kind::NAMESPACE
					|| sv_ctx->kind == symtab_kind::GLOBAL) {
					util::report_ast_metadata(util::error_kind::ERROR,
						"Do not need to capture a global- or namespace- scoped variables.",
						cd->metadata, ident_index, ident_index, true);
					util::report_token(util::error_kind::NOTE,
						"Symbol in question declared here.", sv->name);
					return NULL as type ast::typ*;
				}

				type ast::typ* captured_typ = typ_copy(sv->typ);
				if (is_primitive(captured_typ, ast::primitive_kind::VOID)
					&& cd->is_pointer) {
					util::report_ast_metadata(util::error_kind::ERROR,
						"Cannot take the address of a 'void' type.",
						cd->metadata, ident_index, ident_index, true);
					util::report_token(util::error_kind::NOTE,
						"Symbol in question declared here.", sv->name);
					return NULL as type ast::typ*;
				}

				util::maybe_ice(!util::vector_empty(captured_typ->typ_qualifiers->qualifiers),
					"tck_function",
					"Expected a non-empty qualifiers list for the type of a captured variable!");

				if (cd->is_pointer) {
					util::vector_at(captured_typ->typ_qualifiers->qualifiers, 0)
						as bool* @ = cd->is_pointee_mut;

					util::vector_insert(captured_typ->typ_qualifiers->qualifiers, 0,
						cd->is_mut$ as byte*);
				}
				else {
					bool* tmp = util::vector_at(captured_typ->typ_qualifiers->qualifiers, 0)
						as bool*;
					tmp@ = cd->is_mut;
				}

				type symtab_value* new_sv = symtab_value_decl_init(fsymtab, false,
					false, cd->ident, captured_typ, cd->ident);
				char* cname = n_strdup(cd->ident->text);
				util::ht_set(fsymtab->symbols, cname as byte*, new_sv as byte*);

				cd->symtab_value = new_sv;
			}
		}

		type ast::typ* final_typ = ast::typ_lambda_typ_init(
			mut_non_pointer_typ_qualifiers_init(),
			ast::lambda_typ_init(final_fn_typ, counter++,
				f->capture_list != NULL as type util::vector*
					|| (f->capture_list != NULL as type util::vector*
						&& util::vector_empty(f->capture_list))),
			NULL as type ast::metadata*);

		util::maybe_ice(!is_fwd,
			"tck_function",
			"A lambda cannot be forward-declared!");

		if (!tck_function_helper_body(fsymtab, NULL as type symtab_value*, f, new_return_typ))
			return NULL as type ast::typ*;

		return typ_copy(final_typ);
	}
}

func type ast::typ* tck_function(type symtab* ctx, type ast::function* f) {
	type symtab_value* sv = NULL as type symtab_value*;
	bool is_lambda = f->name == NULL as type lex::token*;

	if (!is_lambda)
		sv = symtab_lookup_unqualified(ctx, f->name, true, true);
	type ast::fn_typ* orig_ft = NULL as type ast::fn_typ*;
	bool is_fwd = f->body == NULL as type ast::stmt*;

	if (sv != NULL as type symtab_value*) {
		if (sv->kind != symtab_value_kind::FUN) {
			util::report_token(util::error_kind::ERROR,
				"Duplicate symbol found here.", f->name);
			util::report_token(util::error_kind::NOTE,
				"Originally declared here.", sv->name);
			return NULL as type ast::typ*;
		}
		
		if (!sv->fwd && !is_fwd) {
			util::report_token(util::error_kind::ERROR,
				"Duplicate function definition here.", f->name);
			util::report_token(util::error_kind::NOTE,
				"Originally defined here.", sv->name);
			return NULL as type ast::typ*;
		}

		util::maybe_ice(sv->typ != NULL as type ast::typ*
			&& sv->typ->kind == ast::typ_kind::FN_TYP
			&& sv->ast_node_kind == symtab_value_ast_node_kind::FUN,
			"tck_function", "Expected a non-NULL function type for a function here!");

		orig_ft = sv->typ->which.fn_typ;
		if (orig_ft->is_variadic != f->is_variadic) {
			util::report_token(util::error_kind::ERROR,
				"Function variadic-ity differs from the one originally specified.",
				f->name);
			util::report_token(util::error_kind::NOTE,
				"Originally specified here.", sv->name);
			return NULL as type ast::typ*;
		}
	}

	if (f->attribute != NULL as type ast::attribute*) {
		type util::hash_table* attrs = tck_attribute(ctx, ast::attribute_kind::FUNCTION, f->attribute);
		if (attrs == NULL as type util::hash_table*)
			return NULL as type ast::typ*;
		f->attribute->attrs = attrs;
	}

	type ast::typ* return_typ = tck_typ(ctx, f->return_typ);
	if (return_typ == NULL as type ast::typ*)
		return NULL as type ast::typ*;

	if (orig_ft != NULL as type ast::fn_typ*) {
		if (!typ_compare(ctx, orig_ft->return_typ, return_typ, true, false)) {
			type util::string* err_str = util::string_init(
				"Function return type does not match between the current specification ('");
			typ_human_readable(f->return_typ, err_str);
			util::string_catc(err_str, "') and the one originally specified ('");
			typ_human_readable(orig_ft->return_typ, err_str);
			util::string_catc(err_str, "').");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				f->return_typ->metadata, 0, 0, false);
			util::report_ast_metadata(util::error_kind::NOTE,
				"Original return type specified here.",
				sv->ast_node.function->return_typ->metadata, 0, 0, false);
			util::string_delete(err_str);

			return NULL as type ast::typ*;
		}

		if (!attribute_compare(f->attribute, sv->ast_node.function->attribute)) {
			util::report_token(util::error_kind::ERROR,
				"Incompatible attribute specification between the current function declaration and the one originally specified.",
				f->name);

			util::report_token(util::error_kind::NOTE,
				"Originally declared here.",
				sv->ast_node.function->name);
			return NULL as type ast::typ*;
		}
	}

	if (!is_lambda && orig_ft != NULL as type ast::fn_typ*) {
		if (util::vector_size(orig_ft->parameter_typs)
			!= util::vector_size(f->parameters)) {
			util::report_token(util::error_kind::ERROR,
				"Different number of parameters between the current and original function specification.",
				f->name);
			util::report_token(util::error_kind::NOTE,
				"Original specification here.", sv->name);
			return NULL as type ast::typ*;
		}
	}

	return tck_function_helper(ctx, f, sv, orig_ft, is_fwd, return_typ);
}

func[static] bool tck_function_helper_body(type symtab* fsymtab,
	type symtab_value* sv, type ast::function* f, type ast::typ* new_return_typ) {
	bool is_lambda = f->name == NULL as type lex::token*;

	util::maybe_ice(f->body != NULL as type ast::stmt*,
		"tck_function_helper", "Expected a non-NULL function body at this point!");

	if (!is_lambda) {
		util::maybe_ice(sv->members == NULL as type symtab*,
			"tck_function_helper", "Expected no symtab present at this point!");
		sv->fwd = false;
		sv->members = fsymtab;
	}

	fsymtab->symtab_info = symtab_info_fun_init(f, typ_copy(new_return_typ));

	if (!prepopulate_stmt_labels(fsymtab, f->body))
		return false;

	util::maybe_ice(f->body->kind == ast::stmt_kind::COMPOUND,
		"tck_function_helper", "Expected a compound statement for a function body!");
	for (unsigned int i = 0; i < util::vector_size(f->body->which.compound->stmts); i++) {
		type ast::stmt* s = util::vector_at(f->body->which.compound->stmts, i)
			as type ast::stmt** @;

		if (!tck_stmt(fsymtab, s))
			return false;
	} 

	f->body->which.compound->ctx = fsymtab;
	f->ctx = fsymtab;

	return true;
}

} } // namespace neutrino::tck
