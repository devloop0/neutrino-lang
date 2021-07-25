import "tck/tck.hsp"

import <"std/io">
import <"std/lib">

import "ast/ast.hsp"
import "util/base.hsp"
import "util/vector.hsp"
import "util/string.hsp"
import "util/error.hsp"
import "tck/symtab.hsp"
import "lex/token.hsp"
import "tck/util.hsp"

using neutrino::util::n_strdup;
using std::io::printf;
using std::lib::NULL;

namespace neutrino { namespace tck {

func bool tck_decl(type symtab* ctx, type ast::decl* d, bool ce) {
	if (d->attribute != NULL as type ast::attribute*) {
		type util::hash_table* attrs = tck_attribute(ctx, ast::attribute_kind::DECL, d->attribute);
		if (attrs == NULL as type util::hash_table*)
			return false;
		d->attribute->attrs = attrs;
	}

	for (unsigned int i = 0; i < util::vector_size(d->decl_components); i++) {
		type ast::decl_component* dc = util::vector_at(d->decl_components, i)
			as type ast::decl_component** @;

		type ast::exp* init = dc->init;

		type ast::typ* deduced = NULL as type ast::typ*;
		if (dc->typ != NULL as type ast::typ*) {
			deduced = tck_typ(ctx, dc->typ);
			if (deduced == NULL as type ast::typ*)
				return false;
		}
		
		if ((deduced == NULL as type ast::typ* || d->is_const)
			&& init == NULL as type ast::exp*) {
			util::report_ast_metadata(util::error_kind::ERROR,
				"Expected a non-'const' type qualifier and an explicit type for a declaration without an initializer.",
				dc->metadata, 0, 0, false);
			return false;
		}

		if (d->is_const || d->is_static)
			ce = true;

		if (init != NULL as type ast::exp*) {
			if (!tck_exp(ctx, init))
				return false;

			type ast::typ* ededuced = typ_copy(init->typ);

			if (deduced != NULL as type ast::typ*) {
				if (d->is_const && is_mut(deduced->typ_qualifiers)) {
					util::report_ast_metadata(util::error_kind::ERROR,
						"Cannot declare a constant with a 'mut' type.",
						dc->metadata,
						(deduced->metadata->start - dc->metadata->start),
						(deduced->metadata->end - dc->metadata->start) - 1,
						true);
					return false;
				}
				if (!typ_compare(ctx, ededuced, deduced, false, true)) {
					type util::string* err_str = util::string_init("Incompatible types between a declaration's explicitly stated type and its initializer; found '");
					typ_human_readable(deduced, err_str);
					util::string_catc(err_str, "' and '");
					typ_human_readable(ededuced, err_str);
					util::string_catc(err_str, "' respectively.");

					unsigned int token_index = deduced->metadata->end - dc->metadata->start;
					util::report_ast_metadata(util::error_kind::ERROR,
						util::string_data(err_str),
						dc->metadata, token_index, token_index, true);
					util::string_delete(err_str);
					return false;
				}
			}
			else
				deduced = ededuced;

			if (ce) {
				if (!init->is_constant) {
					util::report_ast_metadata(util::error_kind::ERROR,
						"Expected an initializer to be constant in this context.",	
						init->metadata, 0, 0, false);
					return false;
				}
			}
		}

		util::maybe_ice(deduced != NULL as type ast::typ*,
			"tck_decl",
			"Expected a concrete type for a declaration at this point.");

		if (!tck_pat(ctx, dc->is_const ? pat_ctx_kind::CONST : pat_ctx_kind::DECL, dc->pat, deduced, d->attribute))
			return false;
	}

	return true;
}

} } // namespace neutrino::tck
