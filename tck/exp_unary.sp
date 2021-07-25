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
import "util/base.hsp"
import "util/generic_funcs.hsp"

using std::io::printf;
using std::lib::NULL;
using neutrino::util::n_strdup;

namespace neutrino { namespace tck {

func bool tck_exp_unary(type symtab* ctx, type ast::unary* u) {
	switch (u->kind) {
	case ast::unary_kind::INCREMENT:
	case ast::unary_kind::DECREMENT: {
		if (!tck_exp(ctx, u->which.base))
			return false;

		char* op_name = NULL as char*;
		if (u->kind == ast::unary_kind::INCREMENT)
			op_name = "increment";
		else
			op_name = "decrement";

		type ast::typ* base_typ = u->which.base->typ;

		if (!is_numeric(base_typ)) {
			type util::string* err_str = util::string_init("Expected a numeric type to ");
			util::string_catc(err_str, op_name);
			util::string_catc(err_str, "; found '");
			typ_human_readable(base_typ, err_str);
			util::string_catc(err_str, "' instead.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				u->metadata, 0, 0, true);
			util::string_delete(err_str);

			return false;
		}
		else if (u->which.base->value_kind != ast::value_kind::LVALUE) {
			type util::string* err_str = util::string_init("Expected an lvalue to ");
			util::string_catc(err_str, op_name);
			util::string_catc(err_str, ".");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				u->metadata, 0, 0, true);
			util::string_delete(err_str);

			return false;
		}
		else if (!is_mut(base_typ->typ_qualifiers) || u->which.base->is_constant) {
			type util::string* err_str = util::string_init("Expected a 'mut', non-constant value to ");
			util::string_catc(err_str, op_name);
			util::string_catc(err_str, ".");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				u->metadata, 0, 0, true);
			util::string_delete(err_str);

			return false;
		}

		u->typ = typ_copy(base_typ);
		u->value_kind = ast::value_kind::LVALUE;
		u->is_constant = false;
		return true;
	}
		break;
	case ast::unary_kind::PLUS:
	case ast::unary_kind::MINUS: {
		if (!tck_exp(ctx, u->which.base))
			return false;

		type ast::typ* base_typ = u->which.base->typ;

		char* op_name = NULL as char*;
		if (u->kind == ast::unary_kind::PLUS)
			op_name = "plus";
		else
			op_name = "minus";

		if (u->kind == ast::unary_kind::PLUS
			&& !is_pointer(base_typ)
			&& base_typ->kind == ast::typ_kind::LAMBDA
			&& !base_typ->which.lambda_typ->has_capture_list) {
			u->typ = typ_copy(base_typ->which.lambda_typ->signature);
			set_mut(u->typ->typ_qualifiers, true);
			u->value_kind = ast::value_kind::RVALUE;
			u->is_constant = u->which.base->is_constant;
			return true;
		}

		if (!is_numeric(base_typ)) {
			type util::string* err_str = util::string_init("Expected a numeric ");
			if (u->kind == ast::unary_kind::PLUS)
				util::string_catc(err_str, "or lambda (with no capture list) ");
			util::string_catc(err_str, "operand to take a unary ");
			util::string_catc(err_str, op_name);
			util::string_catc(err_str, " of; found '");
			typ_human_readable(base_typ, err_str);
			util::string_catc(err_str, "'.");
			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				u->metadata, 0, 0, true);
			util::string_delete(err_str);
			return false;
		}

		u->typ = typ_copy(base_typ);
		set_mut(u->typ->typ_qualifiers, true);
		u->value_kind = ast::value_kind::RVALUE;
		u->is_constant = u->which.base->is_constant;
		return true;
	}
		break;
	case ast::unary_kind::SIZEOF_TYP:
	case ast::unary_kind::SIZEOF_EXP:
	case ast::unary_kind::ALIGNOF_TYP:
	case ast::unary_kind::ALIGNOF_EXP: {
		bool use_exp = u->kind == ast::unary_kind::SIZEOF_EXP
			|| u->kind == ast::unary_kind::ALIGNOF_EXP;

		if (use_exp) {
			if (!tck_exp(ctx, u->which.of_exp))
				return false;
		}
		else {
			type ast::typ* check = tck_typ(ctx, u->which.of_typ);
			if (check == NULL as type ast::typ*)
				return false;
			ast::typ_delete(check);
		}

		u->typ = ast::typ_primitive_init(
			mut_non_pointer_typ_qualifiers_init(),
			ast::primitive_kind::UNSIGNED_INT, NULL as type ast::metadata*);
		u->value_kind = ast::value_kind::RVALUE;
		u->is_constant = true;
		return true;
	}
		break;
	case ast::unary_kind::NOT: {
		if (!tck_exp(ctx, u->which.base))
			return false;

		type ast::typ* base_typ = u->which.base->typ;

		if (!is_primitive(base_typ, ast::primitive_kind::BOOL)) {
			type util::string* err_str = util::string_init("Expected a boolean type to take the logical negation of; found '");
			typ_human_readable(base_typ, err_str);
			util::string_catc(err_str, "'.");
			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				u->metadata, 0, 0, true);
			util::string_delete(err_str);
			return false;
		}

		u->typ = typ_copy(base_typ);
		set_mut(u->typ->typ_qualifiers, true);
		u->value_kind = ast::value_kind::RVALUE;
		u->is_constant = u->which.base->is_constant;
		return true;
	}
		break;
	case ast::unary_kind::CMPL: {
		if (!tck_exp(ctx, u->which.base))
			return false;

		type ast::typ* base_typ = u->which.base->typ;

		if (!is_integral(base_typ)) {
			type util::string* err_str = util::string_init("Expected an integral type to take the complement of; found '");
			typ_human_readable(base_typ, err_str);
			util::string_catc(err_str, "' instead.");
			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				u->metadata, 0, 0, true);
			util::string_delete(err_str);
			return false;
		}

		u->typ = typ_copy(base_typ);
		set_mut(u->typ->typ_qualifiers, true);
		u->value_kind = ast::value_kind::RVALUE;
		u->is_constant = u->which.base->is_constant;
		return true;
	}
		break;
	case ast::unary_kind::DELETE: {
		if (!tck_exp(ctx, u->which.delete_exp))
			return false;

		type ast::typ* d_typ = u->which.delete_exp->typ;
		if (!is_pointer(d_typ)) {
			type util::string* err_str = util::string_init("Expected a pointer type to 'delete'; found '");
			typ_human_readable(d_typ, err_str);
			util::string_catc(err_str, "' instead.");
			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				u->metadata, 1, (u->metadata->end - u->metadata->start) - 1, true);
			util::string_delete(err_str);

			return false;
		}

		u->typ = ast::typ_primitive_init(
			mut_non_pointer_typ_qualifiers_init(),
			ast::primitive_kind::VOID, NULL as type ast::metadata*);
		u->value_kind = ast::value_kind::RVALUE;
		u->is_constant = false;
		return true;
	}
		break;
	case ast::unary_kind::NEW:
	case ast::unary_kind::RESV:
	case ast::unary_kind::STK: {
		type ast::alloc_info* ai = u->which.alloc_info;
		bool ce = u->kind != ast::unary_kind::NEW;

		type ast::typ* naityp = tck_typ(ctx, ai->typ);
		if (naityp == NULL as type ast::typ*)
			return false;

		if (naityp->kind == ast::typ_kind::PRIMITIVE
			&& naityp->which.primitive == ast::primitive_kind::VOID) {
			type util::string* err_str = util::string_init("Cannot allocate (pointer-to) void types; found '");
			typ_human_readable(ai->typ, err_str);
			util::string_catc(err_str, "'.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				u->metadata,
				(ai->typ->metadata->start - u->metadata->start),
				(ai->typ->metadata->end - u->metadata->start) - 1, true);
			util::string_delete(err_str);
			return false;
		}

		if (!tck_exp(ctx, ai->exp))
			return false;

		if (!is_integral(ai->exp->typ)) {
			type util::string* err_str = util::string_init("Expected an integral type for the number of elements to allocate; found '");
			typ_human_readable(ai->exp->typ, err_str);
			util::string_catc(err_str, "' instead.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				u->metadata,
				(ai->exp->metadata->start - u->metadata->start),
				(ai->exp->metadata->end - u->metadata->start) - 1, true);
			return false;
		}

		if (ce) {
			if (!ai->exp->is_constant) {
				util::report_ast_metadata(util::error_kind::ERROR,
					"Expected a constant expression for the number of elements to allocate using 'resv' or 'stk'.",
					u->metadata,
					(ai->exp->metadata->start - u->metadata->start),
					(ai->exp->metadata->end - u->metadata->start) - 1, true);
				return false;
			}
		}

		u->typ = naityp;
		{
			bool is_mut = true;
			util::vector_insert(u->typ->typ_qualifiers->qualifiers,
				0, is_mut$ as byte*);
		}
		u->value_kind = ast::value_kind::RVALUE;
		u->is_constant = u->kind == ast::unary_kind::RESV;
		return true;
	}
		break;
	default:
		util::ice("tck_exp_unary",
			"Unrecognized unary_kind while tck'ing!");
	}

	return false;
}

} } // namespace neutrino::tck
