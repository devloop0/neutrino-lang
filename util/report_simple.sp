import "util/error.hsp"

import <"std/io">
import <"std/lib">

import "lex/token.hsp"
import "lex/lexer.hsp"
import "util/string.hsp"
import "util/vector.hsp"
import "util/generic_funcs.hsp"
import "util/math.hsp"
import "util/error_helpers.hsp"

using std::io::printf;
using std::lib::abort;

namespace neutrino { namespace util {

func void report_str(unsigned int kind, const char* msg,
	type lex::lexer* l,
	unsigned int start_line, unsigned int end_line,
	unsigned int start_col, unsigned int end_col,
	unsigned int start_pos, unsigned int len) {
	if (!is_reporting_enabled()) return;

	error_kind_update_count(kind);
	print_error_header(kind, msg, l, start_line, start_col);

	unsigned int num_to_highlight = len;
	if (num_to_highlight > HIGHLIGHT_CTX_LIMIT)
		num_to_highlight = HIGHLIGHT_CTX_LIMIT;

	unsigned int max_line_num_len = ulen10(start_line);

	print_line_num_prefix(start_line, max_line_num_len);

	char* src = string_data(lex::lexer_source(l));
	unsigned int src_len = string_length(lex::lexer_source(l));

	unsigned int true_start = start_pos;
	while (true_start >= 1 && src[true_start - 1] != '\n')
		true_start--;
	unsigned int caret_offset = start_pos - true_start;
	if (caret_offset > ERR_CTX_LIMIT) {
		caret_offset = ERR_CTX_LIMIT;
		true_start = start_pos - ERR_CTX_LIMIT;
	}

	print_src_helper(true_start, caret_offset, num_to_highlight,
		len > HIGHLIGHT_CTX_LIMIT ? 0 : ERR_CTX_LIMIT,
		len > HIGHLIGHT_CTX_LIMIT, src, src_len);

	print_empty_prefix(max_line_num_len);

	for (unsigned int i = 0; i < caret_offset; i++) {
		if (src[true_start + i] == '\t') {
			for (unsigned int i = 0; i < TAB_WIDTH; i++)
				printf(" ");
		}
		else printf(" ");
	}

	printf("%s^", green_esc);
	for (unsigned int i = 1; i < num_to_highlight; i++) printf("~");
	printf("%s", reset_esc);

	printf("\n\n");
}

func void report_token(unsigned int kind, const char* msg,
	type lex::token* tok) {
	report_str(kind, msg, lex::token_ctx(tok),
		lex::token_start_line(tok), lex::token_end_line(tok),
		lex::token_start_col(tok), lex::token_end_col(tok),
		lex::token_pos(tok), lex::token_length(tok));
}

} } // namespace neutrino::util
