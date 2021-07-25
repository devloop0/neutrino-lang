import "tck/tck.hsp"

import <"std/io">
import <"std/lib">
import <"std/string">

import "util/error.hsp"
import "util/generic_funcs.hsp"
import "util/hash_table.hsp"
import "tck/symtab.hsp"
import "ast/ast.hsp"

using std::io::printf;
using std::lib::NULL;
using std::string::strcmp;

namespace neutrino { namespace tck {

func type util::hash_table* tck_attribute(type symtab* ctx, unsigned int which, type ast::attribute* a) {
	type util::hash_table* attrs = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);

	char* which_name = NULL as char*;
	switch (which) {
	case ast::attribute_kind::DECL:
		which_name = "declaration";
		break;
	case ast::attribute_kind::USING:
		which_name = "using";
		break;
	case ast::attribute_kind::USING_NAMESPACE:
		which_name = "using namespace";
		break;
	case ast::attribute_kind::IMPORT:
		which_name = "import";
		break;
	case ast::attribute_kind::NAMESPACE_ALIAS:
		which_name = "namespace alias";
		break;
	case ast::attribute_kind::NAMESPACE:
		which_name = "namespace";
		break;
	case ast::attribute_kind::FUNCTION:
		which_name = "function";
		break;
	case ast::attribute_kind::STRUCT:
		which_name = "struct";
		break;
	case ast::attribute_kind::UNION:
		which_name = "union";
		break;
	case ast::attribute_kind::VARIANT:
		which_name = "variant";
		break;
	case ast::attribute_kind::ENUM:
		which_name = "enum";
		break;
	case ast::attribute_kind::MEMBER:
		which_name = "member";
		break;
	default:
		util::ice("tck_attribute",
			"Unrecognized attribute_kind while tck'ing!");
	}

	type util::hash_table* attr_name_map = util::ht_init(
		util::str_hash, util::str_eq,
		util::no_free, util::no_free);
	type util::hash_table* attr_has_value_map = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);

	util::ht_set(attr_name_map, "public" as byte*, ast::attribute_value_kind::PUBLIC as byte*);
	util::ht_set(attr_has_value_map, ast::attribute_value_kind::PUBLIC as byte*, 0x0 as byte*);
	util::ht_set(attr_name_map, "extern" as byte*, ast::attribute_value_kind::EXTERN as byte*);
	util::ht_set(attr_has_value_map, ast::attribute_value_kind::EXTERN as byte*, lex::token_kind::STRING as byte*);

	type util::hash_table* attr_pos = util::ht_init(
		util::uint_hash, util::uint_eq,
		util::no_free, util::no_free);

	for (unsigned int i = 0, pos = 2; i < util::vector_size(a->attrs_list); i++, pos++) {
		type ast::attribute_value* av = util::vector_at(a->attrs_list, i)
			as type ast::attribute_value*;

		char* av_name = av->key->text;
		unsigned int has_value = av->value != NULL as type lex::token*
			? av->value->kind : 0x0;
		unsigned int attr_length = has_value != 0x0 ? 3 : 1;

		type ast::metadata m;
		m.token_stream = a->metadata->token_stream;
		m.start = a->metadata->start + pos;
		m.end = a->metadata->start + pos + attr_length;

		unsigned int k;
		if (!util::ht_get(attr_name_map, av_name as byte*, k$ as byte**)) {
			type util::string* err_str = util::string_init("Unrecognized attribute key '");
			util::string_catc(err_str, av_name);
			util::string_catc(err_str, "' in attribute specification.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				a->metadata, pos, pos, true);
			util::string_delete(err_str);
			return NULL as type util::hash_table*;
		}

		unsigned int prev_pos;
		if (util::ht_get(attr_pos, k as byte*, prev_pos$ as byte**)) {
			type util::string* err_str = util::string_init("Attribute '");
			util::string_catc(err_str, av_name);
			util::string_catc(err_str, "' declared twice in specification.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				a->metadata, pos, pos, true);
			util::string_delete(err_str);

			util::report_ast_metadata(util::error_kind::NOTE,
				"Originally declared here.",
				a->metadata, prev_pos, prev_pos, true);
			return NULL as type util::hash_table*;
		}

		bool valid = false;
		switch (k) {
		case ast::attribute_value_kind::PUBLIC:
			valid = which == ast::attribute_kind::DECL
				|| which == ast::attribute_kind::USING
				|| which == ast::attribute_kind::USING_NAMESPACE
				|| which == ast::attribute_kind::NAMESPACE_ALIAS
				|| which == ast::attribute_kind::IMPORT
				|| which == ast::attribute_kind::NAMESPACE
				|| which == ast::attribute_kind::FUNCTION
				|| which == ast::attribute_kind::STRUCT
				|| which == ast::attribute_kind::UNION
				|| which == ast::attribute_kind::VARIANT
				|| which == ast::attribute_kind::MEMBER
				|| which == ast::attribute_kind::ENUM;
			break;
		case ast::attribute_value_kind::EXTERN:
			valid = which == ast::attribute_kind::FUNCTION;
			break;
		default:
			util::ice("tck_attribute",
				"Unrecognized attribute_value_kind while tck'ing!");
		}

		if (!valid) {
			type util::string* err_str = util::string_init("Invalid attribute '");
			util::string_catc(err_str, av_name);
			util::string_catc(err_str, "' on a '");
			util::string_catc(err_str, which_name);
			util::string_catc(err_str, "' construct.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				a->metadata, pos, pos, true);
			util::string_delete(err_str);
			return NULL as type util::hash_table*;
		}

		unsigned int should_have_value;
		util::maybe_ice(util::ht_get(attr_has_value_map, k as byte*, should_have_value$ as byte**),
			"tck_attribute",
			"Expected a valid attribute_value_kind to check for whether it expects a value!");
		if (should_have_value != has_value) {
			type util::string* err_str = util::string_init("Attribute '");
			util::string_catc(err_str, av_name);
			util::string_catc(err_str, "' should");
			util::string_catc(err_str, should_have_value != 0x0 ? " " : " not ");
			util::string_catc(err_str, "have a");
			util::string_catc(err_str, should_have_value == lex::token_kind::STRING
				? " string " : (should_have_value == lex::token_kind::INTEGER
					? "n integral " : " "));
			util::string_catc(err_str, "value.");

			util::report_ast_metadata(util::error_kind::ERROR,
				util::string_data(err_str),
				a->metadata, pos, pos + (attr_length - 1), true);
			util::string_delete(err_str);
			return NULL as type util::hash_table*;
		}

		switch (k) {
		case ast::attribute_value_kind::PUBLIC: {
			type symtab* iter = ctx;
			while (iter != NULL as type symtab*) {
				if (iter->kind == symtab_kind::FUN) {
					util::report_ast_metadata(util::error_kind::ERROR,
						"Local variables cannot be made 'public'.",
						a->metadata, pos, pos + (attr_length - 1), true);
					return NULL as type util::hash_table*;
				}

				iter = iter->parent;
			}
		}
			break;
		case ast::attribute_value_kind::EXTERN: {
			if (strcmp(av->value->value.string_value, "C") != 0) {
				util::report_ast_metadata(util::error_kind::ERROR,
					"Expected an 'extern' attribute to specify a \"C\" external linkage.",
					a->metadata, pos, pos + (attr_length - 1), true);
				return NULL as type util::hash_table*;
			}
		}
			break;
		default:
			util::ice("tck_attribute", "Unrecognized attribute_value_kind while tck'ing!");
		}

		util::ht_set(attrs, k as byte*, av->value as byte*);
		util::ht_set(attr_pos, k as byte*, pos as byte*);

		pos += attr_length;
	}

	util::ht_delete(attr_name_map);
	util::ht_delete(attr_pos);
	return attrs;
}

} } // namespace neutrino::tck
