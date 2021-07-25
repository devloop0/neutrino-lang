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

func bool tck_exp_binary(type symtab* ctx, type ast::binary* b) {
	bool is_assign = b->op == ast::binary_op_kind::EQ
		|| b->op == ast::binary_op_kind::MULT_EQ
		|| b->op == ast::binary_op_kind::DIV_EQ
		|| b->op == ast::binary_op_kind::MOD_EQ
		|| b->op == ast::binary_op_kind::ADD_EQ
		|| b->op == ast::binary_op_kind::SUB_EQ
		|| b->op == ast::binary_op_kind::SHL_EQ
		|| b->op == ast::binary_op_kind::SHR_EQ
		|| b->op == ast::binary_op_kind::BAND_EQ
		|| b->op == ast::binary_op_kind::BXOR_EQ
		|| b->op == ast::binary_op_kind::BOR_EQ,
		is_cmp = b->op == ast::binary_op_kind::LT
			|| b->op == ast::binary_op_kind::GT
			|| b->op == ast::binary_op_kind::GTE
			|| b->op == ast::binary_op_kind::LTE
			|| b->op == ast::binary_op_kind::EQ_EQ
			|| b->op == ast::binary_op_kind::NE;

	if (!tck_exp(ctx, b->lhs))
		return false;
	if (!tck_exp(ctx, b->rhs))
		return false;

	if (is_assign) {
		if (b->lhs->value_kind != ast::value_kind::LVALUE) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Expected an lvalue to assign to.",
				b->metadata, 0, (b->lhs->metadata->end - b->metadata->start) - 1, true);
			return false;
		}

		if (!is_mut(b->lhs->typ->typ_qualifiers)) {
			type util::string* err_str = util::string_init("Expected a 'mut' expression to assign to; found '");
			typ_human_readable(b->lhs->typ, err_str);
			util::string_catc(err_str, "'.");

			util::report_ast_metadata(util::error_kind::ERROR, util::string_data(err_str),
				b->metadata, 0, (b->lhs->metadata->end - b->metadata->start) - 1, true);
			util::string_delete(err_str);
			return false;
		}
	}

	char* op_desc = is_assign ? "assignment" :
		is_cmp ? "comparison" : "binary";

	bool check_integral = false;
	switch (b->op) {
	case ast::binary_op_kind::MOD:
	case ast::binary_op_kind::SHL:
	case ast::binary_op_kind::SHR:
	case ast::binary_op_kind::BAND:
	case ast::binary_op_kind::BOR:
	case ast::binary_op_kind::BXOR:
	case ast::binary_op_kind::MOD_EQ:
	case ast::binary_op_kind::SHL_EQ:
	case ast::binary_op_kind::SHR_EQ:
	case ast::binary_op_kind::BAND_EQ:
	case ast::binary_op_kind::BOR_EQ:
	case ast::binary_op_kind::BXOR_EQ:
		check_integral = true;
	case ast::binary_op_kind::ADD:
	case ast::binary_op_kind::SUB:
	case ast::binary_op_kind::MULT:
	case ast::binary_op_kind::DIV:
	case ast::binary_op_kind::ADD_EQ:
	case ast::binary_op_kind::SUB_EQ:
	case ast::binary_op_kind::MULT_EQ:
	case ast::binary_op_kind::DIV_EQ:
	case ast::binary_op_kind::LT:
	case ast::binary_op_kind::GT:
	case ast::binary_op_kind::LTE:
	case ast::binary_op_kind::GTE: {
		fn bool(type ast::typ*) check_func = check_integral
			? is_integral : is_numeric;
		char* op_typ_desc = check_integral ? "integral" : "numeric";

		if (!check_func(b->lhs->typ) || !check_func(b->rhs->typ)) {
			type util::string* err_str = util::string_init("Expected ");
			util::string_catc(err_str, op_typ_desc);
			util::string_catc(err_str, " operand types for the ");
			util::string_catc(err_str, op_desc);
			util::string_catc(err_str, " operator '");
			util::string_catc(err_str, b->op_token->text);
			util::string_catc(err_str, "'; found '");
			typ_human_readable(b->lhs->typ, err_str);
			util::string_catc(err_str, "' and '");
			typ_human_readable(b->rhs->typ, err_str);
			util::string_catc(err_str, "' instead.");

			unsigned int token_index = (b->lhs->metadata->end - b->metadata->start);
				util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				b->metadata, token_index, token_index, true);
			util::string_delete(err_str);
			return false;
		}

		if (is_cmp) {
			b->value_kind = ast::value_kind::RVALUE;
			b->typ = ast::typ_primitive_init(mut_non_pointer_typ_qualifiers_init(),
				ast::primitive_kind::BOOL, NULL as type ast::metadata*);
		}
		else if (is_assign) {
			b->value_kind = ast::value_kind::LVALUE;
			b->typ = typ_copy(b->lhs->typ);
		}
		else {
			b->value_kind = ast::value_kind::RVALUE;
			unsigned int k = 0x0;

			type util::hash_table* typ_prom_ht = prim_typ_promotion_table();

			util::maybe_ice(b->lhs->typ->kind == ast::typ_kind::PRIMITIVE
				&& b->rhs->typ->kind == ast::typ_kind::PRIMITIVE,
				"tck_exp_binary", "Expected numeric operands for a binary operation here!");

			unsigned int prec1, prec2;
			util::maybe_ice(util::ht_get(typ_prom_ht, b->lhs->typ->which.primitive as byte*, prec1$ as byte**)
				&& util::ht_get(typ_prom_ht, b->rhs->typ->which.primitive as byte*, prec2$ as byte**),
				"tck_exp_binary", "Expected to find primitive types in the type promotion hierarchy here!");

			b->typ = ast::typ_primitive_init(mut_non_pointer_typ_qualifiers_init(),
				prec1 < prec2 ? b->lhs->typ->which.primitive : b->rhs->typ->which.primitive,
				NULL as type ast::metadata*);
			util::ht_delete(typ_prom_ht);
		}
		b->is_constant = b->lhs->is_constant && b->rhs->is_constant && !is_assign;
		return true;
	}
		break;
	case ast::binary_op_kind::EQ_EQ:
	case ast::binary_op_kind::NE:
	case ast::binary_op_kind::EQ: {
		bool directionality = b->op == ast::binary_op_kind::EQ;
		if (!typ_compare(ctx, b->rhs->typ, b->lhs->typ, false, directionality)) {
			type util::string* err_str = util::string_init("Expected compatible operand types for the ");
			util::string_catc(err_str, op_desc);
			util::string_catc(err_str, " operator '");
			util::string_catc(err_str, b->op_token->text);
			util::string_catc(err_str, "'; found '");
			typ_human_readable(b->lhs->typ, err_str);
			util::string_catc(err_str, "' and '");
			typ_human_readable(b->rhs->typ, err_str);
			util::string_catc(err_str, "' instead.");

			unsigned int token_index = b->lhs->metadata->end - b->metadata->start;
			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				b->metadata, token_index, token_index, true);
			util::string_delete(err_str);
			return false;
		}

		if (is_assign) {
			b->typ = typ_copy(b->lhs->typ);
			b->value_kind = ast::value_kind::LVALUE;
			return true;
		}

		util::maybe_ice(is_cmp,
			"tck_exp_binary",
			"Expected '==' or '!=' here at this point!");

		b->typ = ast::typ_primitive_init(mut_non_pointer_typ_qualifiers_init(),
			ast::primitive_kind::BOOL, NULL as type ast::metadata*);
		b->value_kind = ast::value_kind::RVALUE;
		b->is_constant = b->lhs->is_constant && b->rhs->is_constant && !is_assign;
		return true;
	}
		break;
	case ast::binary_op_kind::LAND:
	case ast::binary_op_kind::LOR: {
		if (!is_primitive(b->lhs->typ, ast::primitive_kind::BOOL)
			|| !is_primitive(b->rhs->typ, ast::primitive_kind::BOOL)) {
			type util::string* err_str = util::string_init("Expected boolean operand types for the logical binary operator '");
			util::string_catc(err_str, b->op_token->text);
			util::string_catc(err_str, "'; found '");
			typ_human_readable(b->lhs->typ, err_str);
			util::string_catc(err_str, "' and '");
			typ_human_readable(b->rhs->typ, err_str);
			util::string_catc(err_str, "' instead.");

			unsigned int token_index = b->lhs->metadata->end - b->metadata->start;
			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				b->metadata, token_index, token_index, true);
			util::string_delete(err_str);
			return false;
		}

		b->typ = ast::typ_primitive_init(mut_non_pointer_typ_qualifiers_init(),
			ast::primitive_kind::BOOL, NULL as type ast::metadata*);
		b->value_kind = ast::value_kind::RVALUE;
		b->is_constant = b->lhs->is_constant && b->rhs->is_constant;
		return true;
	}
		break;
	default:
		util::ice("tck_exp_binary",
			"Unrecognized binary_kind while tck'ing!");
	}

	return false;
}

} } // namespace neutrino::tck
