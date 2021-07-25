import "util/error.hsp"

import <"std/io">
import <"std/lib">

import "ast/ast.hsp"
import "lex/token.hsp"
import "lex/lexer.hsp"
import "util/string.hsp"
import "util/vector.hsp"
import "util/generic_funcs.hsp"
import "util/math.hsp"
import "util/error_helpers.hsp"

using std::io::printf;
using std::lib::abort;
using std::lib::NULL;

namespace neutrino { namespace util {

func void report_ast_metadata(unsigned int kind, char* msg,
	type ast::metadata* m, unsigned int pos_start
	unsigned int pos_end, bool separate_highlight) {
	if (!is_reporting_enabled()) return;

	char* err_type = error_kind2str(kind),
		color_esc = error_kind2color_esc(kind);
	error_kind_update_count(kind);

	maybe_ice(m->start != m->end, "report_ast_metadata",
		"Expected at least a single token to report an error for!");
	maybe_ice(pos_start < m->end - m->start
		&& pos_end < m->end - m->start, "report_ast_metadata",
		"Expected valid token positions to point to!");

	type lex::token* ref_start_tok = vector_at(m->token_stream, m->start)
		as type lex::token** @,
		ref_end_tok = vector_at(m->token_stream, m->end - 1)
			as type lex::token** @,
		ref_pos_start_tok = vector_at(m->token_stream, m->start + pos_start)
			as type lex::token** @,
		ref_pos_end_tok = vector_at(m->token_stream, m->start + pos_end)
			as type lex::token** @;
	unsigned int true_end_pos = lex::token_pos(ref_end_tok)
		+ lex::token_length(ref_end_tok);

	unsigned int ref_pos_start = lex::token_pos(ref_pos_start_tok),
		ref_pos_end = lex::token_pos(ref_pos_end_tok)
			+ lex::token_length(ref_pos_end_tok);
	
	type lex::lexer* ref_lexer = ref_start_tok->ctx;

	print_error_header(kind, msg, ref_lexer,
		lex::token_start_line(ref_pos_start_tok),
		lex::token_start_col(ref_pos_start_tok));

	char* src = string_data(lex::lexer_source(ref_lexer));
	unsigned int src_len = string_length(lex::lexer_source(ref_lexer));

	unsigned int max_line_num_len = ulen10(lex::token_end_line(ref_end_tok));

	unsigned int curr_line_printed = 0, lines_printed = 0;
	unsigned int i = m->start;
	while (i < m->end && lines_printed < LINES_LIMIT) {
		type lex::token* curr = vector_at(m->token_stream, i)
			as type lex::token** @;
		unsigned int curr_line = lex::token_start_line(curr),
			curr_start_pos = lex::token_pos(curr);

		if (curr_line_printed != curr_line) {
			if (curr_line_printed != 0) {
				if (curr_line - curr_line_printed > (LINES_LIMIT - lines_printed))
					break;
				while (lines_printed < LINES_LIMIT && curr_line_printed + 1 < curr_line) {
					print_line_num_prefix(curr_line_printed + 1, max_line_num_len);
					printf("%s\n");
					print_empty_prefix(max_line_num_len);
					printf("%s\n");

					lines_printed++;
					curr_line_printed++;
				}

				if (lines_printed >= LINES_LIMIT)
					break;
			}
			print_line_num_prefix(curr_line, max_line_num_len);

			unsigned int true_start = curr_start_pos, start_i = i;
			while (true_start >= 1 && src[true_start - 1] != '\n')
				true_start--;
			unsigned int highlight_off = curr_start_pos - true_start;
			if (highlight_off > ERR_CTX_LIMIT) {
				highlight_off = ERR_CTX_LIMIT;
				true_start = curr_start_pos - ERR_CTX_LIMIT;
			}

			while (i < m->end) {
				type lex::token* curr = vector_at(m->token_stream, i)
					as type lex::token** @;
				if (lex::token_start_line(curr) != curr_line)
					break;
				i++;
			}
			unsigned int end_i = i;

			unsigned int j = curr_start_pos;
			while (j < src_len && j < true_end_pos && src[j] != '\n')
				j++;

			unsigned int len = j - curr_start_pos;
			unsigned int num_to_highlight = len;
			if (num_to_highlight > HIGHLIGHT_CTX_LIMIT)
				num_to_highlight = HIGHLIGHT_CTX_LIMIT;

			print_src_helper(true_start, highlight_off, num_to_highlight,
				len > HIGHLIGHT_CTX_LIMIT ? 0 : ERR_CTX_LIMIT,
				len > HIGHLIGHT_CTX_LIMIT, src, src_len);

			print_empty_prefix(max_line_num_len);

			bool true_start_sep = ref_pos_start <= true_start && true_start < ref_pos_end,
				curr_start_sep = ref_pos_start <= curr_start_pos
					&& curr_start_pos < ref_pos_end;
			if (start_i != m->start) {
				if (true_start_sep && separate_highlight)
					printf("%s%s", yellow_esc, bold_esc);
				else
					printf("%s", green_esc);
			}

			unsigned int iter_i = start_i;
			type lex::token* curr_tok = util::vector_at(m->token_stream, iter_i)
				as type lex::token** @,
				start_tok = curr_tok,
				prev_tok = NULL as type lex::token*,
				next_tok = NULL as type lex::token*;
			if (pos_start != 0) {
				prev_tok = util::vector_at(m->token_stream, m->start + pos_start - 1)
					as type lex::token** @;
			}
			if (pos_end != (m->end - m->start) - 1) {
				next_tok = util::vector_at(m->token_stream, m->start + pos_end + 1)
					as type lex::token** @;
			}

			for (unsigned int j = 0; j < highlight_off; j++) {
				if (true_start + j == ref_pos_end)
					printf("%s%s", reset_esc, green_esc);

				bool underline = false;
				while (true_start + j >= lex::token_pos(curr_tok) + lex::token_length(curr_tok)
					&& iter_i < end_i) {
					curr_tok = util::vector_at(m->token_stream, iter_i++)
						as type lex::token** @;
				}
				if (lex::token_pos(curr_tok) <= true_start + j
					&& true_start + j < lex::token_pos(curr_tok) + lex::token_length(curr_tok)) {
					underline = true;
				}

				if (src[true_start + j] == '\t') {
					for (unsigned int k = 0; k < TAB_WIDTH; k++)
						printf(start_i != m->start && underline ? "~" : " ");
				}
				else printf(start_i != m->start && underline ? "~" : " ");
			}

			if (curr_start_sep && separate_highlight)
				printf("%s%s", yellow_esc, bold_esc);
			else
				printf("%s", green_esc);
			for (unsigned int j = 0; j < num_to_highlight; j++) {
				if (curr_start_pos + j == ref_pos_end)
					printf("%s%s", reset_esc, green_esc);

				bool underline = false;
				while (curr_start_pos + j >= lex::token_pos(curr_tok) + lex::token_length(curr_tok)
					&& iter_i < end_i) {
					curr_tok = util::vector_at(m->token_stream, iter_i++)
						as type lex::token** @;
				}
				if (lex::token_pos(curr_tok) <= curr_start_pos + j
					&& curr_start_pos + j < lex::token_pos(curr_tok) + lex::token_length(curr_tok)) {
					underline = true;
				}
				else {
					if (ref_pos_start <= curr_start_pos + j && curr_start_pos + j < ref_pos_end)
						underline = true;
					else if (separate_highlight) {
						if (prev_tok != NULL as type lex::token* && next_tok == NULL as type lex::token*)
							underline = curr_start_pos + j < lex::token_pos(prev_tok) + lex::token_length(prev_tok);
						else if (prev_tok == NULL as type lex::token* && next_tok != NULL as type lex::token*)
							underline = curr_start_pos + j >= lex::token_pos(next_tok);
						else if (prev_tok != NULL as type lex::token* && next_tok != NULL as type lex::token*) {
							underline = curr_start_pos + j < lex::token_pos(prev_tok) + lex::token_length(prev_tok)
								|| curr_start_pos + j >= lex::token_pos(next_tok);
						}
						else
							underline = false;
					}
					else
						underline = true;
				}

				if (ref_pos_start == j + curr_start_pos) {
					if (separate_highlight)
						printf("%s%s%s",  reset_esc, yellow_esc, bold_esc);
					printf(underline ? "^" : " ");
				}
				else
					printf(underline ? "~" : " ");
			}
			printf("%s\n", reset_esc);

			curr_line_printed = curr_line;
			lines_printed++;
		}
	}

	printf("\n");
}

} } // namespace neutrino::util
