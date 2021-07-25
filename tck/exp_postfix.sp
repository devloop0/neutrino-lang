import "tck/exp_helpers.hsp"

import <"std/io">
import <"std/lib">

import "tck/tck.hsp"
import "ast/ast.hsp"
import "tck/symtab.hsp"
import "util/error.hsp"
import "lex/token.hsp"
import "tck/util.hsp"
import "util/lib.hsp"
import "util/hash_table.hsp"
import "util/generic_funcs.hsp"
import "util/base.hsp"

using std::io::printf;
using std::lib::NULL;
using neutrino::util::n_strdup;

namespace neutrino { namespace tck {

func bool tck_exp_postfix(type symtab* ctx, type ast::postfix* p) {
	if (!tck_exp(ctx, p->base))
		return false;

	type ast::typ* base_typ = p->base->typ;

	switch (p->kind) {
	case ast::postfix_kind::INCREMENT:
	case ast::postfix_kind::DECREMENT: {
		char* op_name = NULL as char*;
		if (p->kind == ast::postfix_kind::INCREMENT)
			op_name = "increment";
		else
			op_name = "decrement";

		if (!is_numeric(base_typ)) {
			type util::string* err_str = util::string_init("Expected a numeric type to ");
			util::string_catc(err_str, op_name);
			util::string_catc(err_str, "; found '");
			typ_human_readable(base_typ, err_str);
			util::string_catc(err_str, "' instead.");

			unsigned int token_index = (p->metadata->end - p->metadata->start) - 1;
			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, token_index, token_index, true);
			util::string_delete(err_str);

			return false;
		}
		else if (p->base->value_kind != ast::value_kind::LVALUE) {
			type util::string* err_str = util::string_init("Expected an lvalue to ");
			util::string_catc(err_str, op_name);
			util::string_catc(err_str, ".");

			unsigned int token_index = (p->metadata->end - p->metadata->start) - 1;
			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, token_index, token_index, true);
			util::string_delete(err_str);

			return false;
		}
		else if (!is_mut(base_typ->typ_qualifiers) || p->base->is_constant) {
			type util::string* err_str = util::string_init("Expected a 'mut', non-constant value to ");
			util::string_catc(err_str, op_name);
			util::string_catc(err_str, ".");

			unsigned int token_index = (p->metadata->end - p->metadata->start) - 1;
			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, token_index, token_index, true);
			util::string_delete(err_str);

			return false;
		}

		p->typ = typ_copy(base_typ);
		p->value_kind = ast::value_kind::RVALUE;
		p->is_constant = false;
		return true;
	}
		break;
	case ast::postfix_kind::AT: {
		if (!is_pointer(base_typ)) {
			type util::string* err_str = util::string_init("Cannot dereference a non-pointer type; found '");
			typ_human_readable(base_typ, err_str);
			util::string_catc(err_str, "'.");

			unsigned int token_index = (p->metadata->end - p->metadata->start) - 1;
			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, token_index, token_index, true);
			util::string_delete(err_str);

			return false;
		}

		p->typ = typ_copy(base_typ);
		util::vector_remove(p->typ->typ_qualifiers->qualifiers, 0);
		p->value_kind = ast::value_kind::LVALUE;
		p->is_constant = false;
		return true;
	}
		break;
	case ast::postfix_kind::INDEX: {
		if (!is_pointer(base_typ)) {
			type util::string* err_str = util::string_init("Cannot index into a non-pointer type; found '");
			typ_human_readable(base_typ, err_str);
			util::string_catc(err_str, "'.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, 0, (p->base->metadata->end - p->metadata->start) - 1, true);
			util::string_delete(err_str);

			return false;
		}

		if (!tck_exp(ctx, p->which.index))
			return false;

		if (!is_integral(p->which.index->typ)) {
			type util::string* err_str = util::string_init("Index type must be integral; found '");
			typ_human_readable(p->which.index->typ, err_str);
			util::string_catc(err_str, "'.");

			unsigned int start_index = (p->which.index->metadata->start - p->metadata->start),
				end_index = (p->metadata->end - p->metadata->start) - 2;
			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, start_index, end_index, true);
			util::string_delete(err_str);

			return false;
		}

		p->typ = typ_copy(base_typ);
		util::vector_remove(p->typ->typ_qualifiers->qualifiers, 0);
		p->value_kind = ast::value_kind::LVALUE;
		p->is_constant = false;
		return true;
	}
		break;
	case ast::postfix_kind::ADDRESS: {
		if (p->base->value_kind != ast::value_kind::LVALUE) {
			unsigned int token_index = (p->metadata->end - p->metadata->start) - 1;
			util::report_ast_metadata(util::error_kind::ERROR,
				"Expected an lvalue to take the address of.",
				p->metadata, token_index, token_index, true);
			return false;
		}
		else if (is_primitive(base_typ, ast::primitive_kind::VOID)) {
			unsigned int token_index = (p->metadata->end - p->metadata->start) - 1;
			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot take the address of a 'void' type.",
				p->metadata, token_index, token_index, true);
			return false;
		}
		else if (p->base->is_constant) {
			unsigned int token_index = (p->metadata->end - p->metadata->start) - 1;
			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot take the address of a constant expression.",
				p->metadata, token_index, token_index, true);
			return false;
		}

		p->typ = typ_copy(base_typ);
		{
			bool is_mut = true;
			util::vector_insert(p->typ->typ_qualifiers->qualifiers,
				0, is_mut$ as byte*);
		}
		p->value_kind = ast::value_kind::RVALUE;
		p->is_constant = false;
		return true;
	}
		break;
	case ast::postfix_kind::FUNCTION_CALL: {
		type util::vector* args = p->which.arguments;

		if (is_pointer(base_typ)
			|| (base_typ->kind != ast::typ_kind::FN_TYP
				&& base_typ->kind != ast::typ_kind::LAMBDA)) {
			type util::string* err_str = util::string_init("Expected a function/lambda type for a function call; found '");
			typ_human_readable(base_typ, err_str);
			util::string_catc(err_str, "' instead.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str), p->metadata,
				0, (p->base->metadata->end - p->metadata->start) - 1, true);
			return false;
		}

		type ast::fn_typ* ft = NULL as type ast::fn_typ*;
		if (base_typ->kind == ast::typ_kind::FN_TYP)
			ft = base_typ->which.fn_typ;
		else {
			type ast::typ* tmp = base_typ->which.lambda_typ->signature;
			util::maybe_ice(tmp->kind == ast::typ_kind::FN_TYP,
				"tck_exp_postfix",
				"Expected a function type for the signature of a lambda type!");

			ft = tmp->which.fn_typ;
		}

		unsigned int num_args = util::vector_size(args),
			num_pars = util::vector_size(ft->parameter_typs);

		if (!ft->is_variadic) {
			if (num_args != num_pars) {
				type util::string* err_str = util::string_init("Incorrect arity found for a call to a non-variadic function. Expected ");
				util::utoustr(num_pars, err_str);
				util::string_catc(err_str, " argument(s), but found ");
				util::utoustr(num_args, err_str);
				util::string_catc(err_str, ".");

				unsigned int opar_index = p->base->metadata->end - p->metadata->start;
				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, opar_index, opar_index, true);
				util::string_delete(err_str);

				err_str = util::string_init("Type of provided function was: '");
				typ_human_readable(base_typ, err_str);
				util::string_catc(err_str, "'.");
				util::report_ast_metadata(util::error_kind::NOTE,
					util::string_data(err_str),
					p->metadata, 0, (p->base->metadata->end - p->metadata->start) - 1, true);
				util::string_delete(err_str);
				return false;
			}
		}
		else {
			if (num_args < num_pars) {
				type util::string* err_str = util::string_init("Too few arguments found for a call to a variadic function. Expected at least ");
				util::utoustr(num_pars, err_str);
				util::string_catc(err_str, " argument(s), but found ");
				util::utoustr(num_args, err_str);
				util::string_catc(err_str, ".");
				
				unsigned int opar_index = p->base->metadata->end - p->metadata->start;
				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, opar_index, opar_index, true);
				util::string_delete(err_str);

				err_str = util::string_init("Type of provided function was: '");
				typ_human_readable(base_typ, err_str);
				util::string_catc(err_str, "'.");
				util::report_ast_metadata(util::error_kind::NOTE,
					util::string_data(err_str),
					p->metadata, 0, (p->base->metadata->end - p->metadata->start) - 1, true);
				util::string_delete(err_str);
				return false;
			}
		}

		for (unsigned int i = 0; i < num_args; i++) {
			type ast::exp* arg = util::vector_at(args, i) as type ast::exp** @;

			if (!tck_exp(ctx, arg))
				return false;

			if (i < num_pars) {
				type ast::typ* par_typ = util::vector_at(ft->parameter_typs, i)
					as type ast::typ** @;

				if (!typ_compare(ctx, arg->typ, par_typ, false, true)) {
					type util::string* err_str = util::string_init("Incompatible type provided for argument #");
					util::utoustr(i + 1, err_str);
					util::string_catc(err_str, "; found '");
					typ_human_readable(arg->typ, err_str);
					util::string_catc(err_str, "' but expected '");
					typ_human_readable(par_typ, err_str);
					util::string_catc(err_str, "'.");

					util::report_ast_metadata(util::error_kind::ERROR,
						util::string_data(err_str),
						arg->metadata, 0, 0, false);
					util::string_delete(err_str);

					err_str = util::string_init("Type of provided function was '");
					typ_human_readable(base_typ, err_str);
					util::string_catc(err_str, "'.");
					util::report_ast_metadata(util::error_kind::NOTE,
						util::string_data(err_str),
						p->base->metadata, 0, 0, false);
					util::string_delete(err_str);

					return false;
				}
			}
		}

		p->typ = typ_copy(ft->return_typ);
		p->value_kind = ast::value_kind::RVALUE;
		p->is_constant = false;
		return true;
	}
		break;
	case ast::postfix_kind::DOT_INDEX:
	case ast::postfix_kind::ARROW_INDEX:
	case ast::postfix_kind::DOT:
	case ast::postfix_kind::ARROW: {
		type ast::member_data* md = p->which.member_data;
		unsigned int accessor_index = p->base->metadata->end - p->metadata->start;

		type ast::typ_qualifiers* base_tqs = base_typ->typ_qualifiers;
		if ((p->kind == ast::postfix_kind::DOT
				|| p->kind == ast::postfix_kind::DOT_INDEX)
			&& util::vector_size(base_tqs->qualifiers) != 1) {
			type util::string* err_str = util::string_init(
				"Expected a non-pointer type for the '.' member accessor; found '");
			typ_human_readable(base_typ, err_str);
			util::string_catc(err_str, "'.");
			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, accessor_index, accessor_index, true);
			return false;
		}
		else if ((p->kind == ast::postfix_kind::ARROW
			|| p->kind == ast::postfix_kind::ARROW_INDEX)
			&& util::vector_size(base_tqs->qualifiers) != 2) {
			type util::string* err_str = util::string_init(
				"Expected a (single) pointer type for the '->' member accessor; found '");
			typ_human_readable(base_typ, err_str);
			util::string_catc(err_str, "'.");
			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, accessor_index, accessor_index, true);
			return false;
		}

		if (p->kind == ast::postfix_kind::ARROW_INDEX
			|| p->kind == ast::postfix_kind::DOT_INDEX) {
			if (base_typ->kind != ast::typ_kind::TUP) {
				type util::string* err_str = util::string_init("Expected a tuple type for a tuple accessor; found '");
				typ_human_readable(base_typ, err_str);
				util::string_catc(err_str, "' instead.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, 0, (p->base->metadata->end - p->metadata->start) - 1, true);
				util::string_delete(err_str);
				return false;
			}

			util::maybe_ice(md->member->kind == lex::token_kind::INTEGER,
				"tck_exp_postfix",
				"Expected an integer for a tuple index access.");

			type util::vector* tup_typ = base_typ->which.tup_typ;

			unsigned int u = md->member->value.integral_value;

			if (u >= util::vector_size(tup_typ)) {
				type util::string* err_str = util::string_init("Expected an integer in range of a tuple type for a tuple accessor; found an index of '");
				util::utoustr(u, err_str);
				util::string_catc(err_str, "' for a tuple of size '");
				util::utoustr(util::vector_size(tup_typ), err_str);
				util::string_catc(err_str, "' with type '");
				typ_human_readable(base_typ, err_str);
				util::string_catc(err_str, "'.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, accessor_index, (p->metadata->end - p->metadata->start) - 1, true);
				util::string_delete(err_str);
				return false;
			}

			md->is_resolved_ufcs = false;
			md->symtab_value = NULL as type symtab_value*;

			p->typ = typ_copy(util::vector_at(tup_typ, u) as type ast::typ** @);
			set_mut(p->typ->typ_qualifiers, true);
			p->is_constant = false;
			p->value_kind = ast::value_kind::LVALUE;
			return true;
		}

		util::maybe_ice(md->member->kind == lex::token_kind::IDENT,
			"tck_exp_postfix",
			"Expected an identifier for a (UFCS) member access.");

		unsigned int mem_index = (p->metadata->end - p->metadata->start)
			- (md->is_ufcs ? 2 : 1);

		if (base_typ->kind == ast::typ_kind::AGGREGATE_NAME) {
			type ast::qualified_identifier* qi = base_typ->which.aggregate_name;

			type symtab_value* sv = symtab_lookup_qualified(qi->ref_ctx, qi->full_name,
				NULL as type ast::metadata*, false);

			util::maybe_ice(sv != NULL as type symtab_value*,
				"tck_exp_postfix",
				"Expected a valid type name to look up from for a member!");

			if ((sv->kind != symtab_value_kind::STRUCT
				&& sv->kind != symtab_value_kind::UNION)
					&& !md->is_ufcs) {
				type util::string* err_str = util::string_init("For a non-UFCS lookup, expected a struct/union type; found '");
				typ_human_readable(sv->typ, err_str);
				util::string_catc(err_str, "' instead.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, 0, (p->base->metadata->end - p->metadata->start) - 1, true);
				util::string_delete(err_str);
				return false;
			}

			if (sv->fwd) {
				type util::string* err_str = util::string_init("Cannot use an accessor on a forward-declared type; found '");
				typ_human_readable(sv->typ, err_str);
				util::string_catc(err_str, "' instead.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, 0, (p->base->metadata->end - p->metadata->start) - 1, true);
				util::report_token(util::error_kind::NOTE,
					"Symbol originally declared here.", sv->name);
				util::string_delete(err_str);

				return false;
			}

			type symtab_value* mem_lookup = symtab_lookup_unqualified(sv->members, md->member, true, true);
			if (mem_lookup != NULL as type symtab_value*
				&& mem_lookup->ast_node_kind == symtab_value_ast_node_kind::MEMBER) {
				md->is_resolved_ufcs = false;
				md->symtab_value = mem_lookup;
				
				p->typ = typ_copy(mem_lookup->typ);
				p->value_kind = ast::value_kind::LVALUE;
				p->is_constant = false;
				return true;
			}

			if (!md->is_ufcs) {
				type util::string* err_str = util::string_init("Could not find member '");
				util::string_catc(err_str, md->member->text);
				util::string_catc(err_str, "' in a non-UFCS lookup.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, mem_index, mem_index, true);
				util::report_token(util::error_kind::NOTE,
					"Symbol requested in this context.", sv->name);
				util::string_delete(err_str);
				return false;
			}
		}
		else if (!md->is_ufcs) {
			type util::string* err_str = util::string_init("Expected a struct/union aggregate for a non-UFCS accessor; found '");
			typ_human_readable(base_typ, err_str);
			util::string_catc(err_str, "' instead.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, 0, (p->base->metadata->end - p->metadata->start) - 1, true);
			util::string_delete(err_str);
			return false;
		}

		util::maybe_ice(md->is_ufcs,
			"tck_exp_postfix",
			"Expected a UFCS lookup at this point!");

		type util::vector* orig_svs = NULL as type util::vector*;
		type util::vector* svs = NULL as type util::vector*;
		if (util::ht_get(ctx->global->symtab_info->which.ufcs_table,
			md->member->text as byte*, orig_svs$ as byte**)) {
			svs = util::vector_init(sizeof{type symtab_value*},
				util::no_free);

			for (unsigned int i = 0; i < util::vector_size(orig_svs); i++) {
				type symtab_value* curr_check = util::vector_at(orig_svs, i)
					as type symtab_value** @;
				type ast::typ* curr_typ = curr_check->typ;

				util::maybe_ice(curr_check->kind == symtab_value_kind::FUN
					&& curr_check->ast_node_kind == symtab_value_ast_node_kind::FUN
					&& curr_typ->kind == ast::typ_kind::FN_TYP,
					"tck_exp_postfix",
					"Expected a function type for a ufcs 'fun' lookup!");

				type ast::fn_typ* curr_ft = curr_typ->which.fn_typ;
				if (!util::vector_empty(curr_ft->parameter_typs)) {
					type ast::typ* first = util::vector_at(curr_ft->parameter_typs, 0)
						as type ast::typ** @;

					if (typ_compare(ctx, base_typ, first, false, true)) {
						bool add = false;
						if (!is_pointer(base_typ) && base_typ->kind == ast::typ_kind::PRIMITIVE) {
							if (!is_pointer(first) && first->kind == ast::typ_kind::PRIMITIVE
								&& first->which.primitive == base_typ->which.primitive)
								add = true;
							else
								add = false;
						}
						else
							add = true;

						if (add)
							util::vector_append(svs, curr_check$ as byte*);
					}
				}
			}
		}
		else {
			orig_svs = NULL as type util::vector*;
			svs = NULL as type util::vector*;
		}


		if (orig_svs == NULL as type util::vector*
			|| svs == NULL as type util::vector*
			|| util::vector_empty(svs)) {
			type util::string* err_str = util::string_init("Could not resolve UFCS lookup for '");
			util::string_catc(err_str, md->member->text);
			util::string_catc(err_str, "' here with starting type: '");
			typ_human_readable(base_typ, err_str);
			util::string_catc(err_str, "'.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, mem_index, mem_index + 1, true);
			util::string_delete(err_str);
			return false;
		}

		constexpr unsigned int CANDIDATE_LIMIT = 4;

		if (util::vector_size(svs) > 1) {
			type util::string* err_str = util::string_init("Found ");
			util::utoustr(util::vector_size(svs), err_str);
			util::string_catc(err_str, " possible candidates for a UFCS call from here with starting type: '");
			typ_human_readable(base_typ, err_str);
			util::string_catc(err_str, "' (showing at most ");
			util::utoustr(CANDIDATE_LIMIT, err_str);
			util::string_catc(err_str, ").");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				p->metadata, mem_index, mem_index + 1, true);
			util::string_delete(err_str);

			for (unsigned int i = 0; i < util::vector_size(svs) && i < CANDIDATE_LIMIT; i++) {
				type symtab_value* curr_sv = util::vector_at(svs, i)
					as type symtab_value** @;

				type util::string* err_str = util::string_init("Candidate #");
				util::utoustr(i + 1, err_str);
				util::string_catc(err_str, " here with type: '");
				typ_human_readable(curr_sv->typ, err_str);
				util::string_catc(err_str, "'.");

				util::report_token(util::error_kind::NOTE,
					util::string_data(err_str), curr_sv->name);
				util::string_delete(err_str);
			}

			return false;
		}

		type symtab_value* ufcs_sv = util::vector_at(svs, 0)
			as type symtab_value** @;
		util::vector_delete(svs);
		util::maybe_ice(ufcs_sv->typ != NULL as type ast::typ*
			&& ufcs_sv->typ->kind == ast::typ_kind::FN_TYP,
			"tck_exp_postfix",
			"Expected a UFCS lookup to yield a function type!");

		md->is_resolved_ufcs = true;
		md->symtab_value = ufcs_sv;

		if (p->next != NULL as type ast::postfix*
			&& p->next->kind == ast::postfix_kind::FUNCTION_CALL) {
			p->typ = typ_copy(ufcs_sv->typ);
			type ast::fn_typ* ft = p->typ->which.fn_typ;
			util::vector_remove(ft->parameter_typs, 0);

			p->is_constant = false;
			p->value_kind = ast::value_kind::RVALUE;
			return true;
		}
		else {
			type ast::fn_typ* ft = ufcs_sv->typ->which.fn_typ;
			if (util::vector_size(ft->parameter_typs) != 1) {
				type util::string* err_str = util::string_init("For a UFCS lookup with no function call, expected a function with 1 argument; found '");
				typ_human_readable(ufcs_sv->typ, err_str);
				util::string_catc(err_str, "' instead.");

				util::report_ast_metadata(util::error_kind::ERROR,
					util::string_data(err_str),
					p->metadata, mem_index, mem_index + 1, true);
				util::string_delete(err_str);
				return false;
			}

			p->typ = typ_copy(ft->return_typ);
			p->is_constant = false;
			p->value_kind = ast::value_kind::RVALUE;
			return true;
		}

		util::ice("tck_exp_postfix",
			"For a UFCS member lookup, this should be unreachable!");
	}
		break;
	case ast::postfix_kind::AS: {
		type ast::typ* ct = p->which.as_typ;

		type ast::typ* nct = tck_typ(ctx, ct);
		if (nct == NULL as type ast::typ*)
			return false;

		if (is_primitive(base_typ, ast::primitive_kind::VOID)
			&& !is_primitive(nct, ast::primitive_kind::VOID)) {
			unsigned int start_index = 0,
				end_index = (p->base->metadata->end - p->metadata->start) - 1;

			util::report_ast_metadata(util::error_kind::ERROR,
				"Cannot cast from a 'void' type to a non-'void' type.",
				p->metadata, start_index, end_index, true);
			return false;
		}

		p->typ = nct;
		p->value_kind = ast::value_kind::RVALUE;
		p->is_constant = p->base->is_constant;
		return true;
	}
		break;
	default:
		util::ice("tck_exp_postfix",
			"Unrecognized postfix_kind while tck'ing!");
	}

	return false;
}

} } // namespace neutrino::tck
