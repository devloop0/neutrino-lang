import "util/error_helpers.hsp"

import <"std/io">

import "util/error.hsp"

using std::io::printf;

namespace neutrino { namespace util {

func char* error_kind2str(unsigned int k) {
	switch (k) {
	case error_kind::ERROR:
		return "Error";
	case error_kind::WARNING:
		return "Warning";
	case error_kind::NOTE:
		return "Note";
	default:
		ice("error_kind2str", "Unrecognized error_kind here!");
	}
}

func char* error_kind2color_esc(unsigned int k) {
	switch (k) {
	case error_kind::ERROR:
		return red_esc;
	case error_kind::WARNING:
		return purple_esc;
	case error_kind::NOTE:
		return blue_esc;
	default:
		ice("error_kind2color_esc",
			"Unrecognized error_kind here!");
	}
}

func void print_error_header(unsigned int kind, const char* msg,
	type lex::lexer* l, unsigned int start_line, unsigned int start_col) {
	char* err_type = error_kind2str(kind),
		color_esc = error_kind2color_esc(kind);

	printf("%s%s%s%s", color_esc, bold_esc, err_type, reset_esc);
	printf("[%s%s:%u:%u%s]: %s%s%s\n",
		underline_esc, string_data(lex::lexer_file_name(l)),
		start_line, 
		start_col, reset_esc,
		bold_esc, msg, reset_esc);
}

func void print_src_helper(unsigned int true_start,
	unsigned int highlight_off, unsigned int num_to_highlight,
	unsigned int post_highlight, bool cut_off,
	char* src, unsigned int src_len) {
	for (unsigned int i = true_start; i < src_len
		&& i < true_start + highlight_off + num_to_highlight
			+ post_highlight
		&& src[i] != '\n'; i++) {
		if (src[i] == '\t') {
			for (unsigned int i = 0; i < TAB_WIDTH; i++)
				printf(" ");
		}
		else printf("%c", src[i]);
	}
	if (cut_off)
		printf("%s...%s", bold_esc, reset_esc);
	printf("\n");
}

func void print_line_num_prefix(unsigned int curr_line,
	unsigned int max_line_num_len) {
	printf("%s  ", bold_esc);
	int num_chars = printf("%u", curr_line);
	for (unsigned int i = num_chars; i < max_line_num_len; i++)
		printf(" ");
	printf(" | %s", reset_esc);
}

func void print_empty_prefix(unsigned int max_line_num_len) {
	printf("%s  ", bold_esc);
	for (int i = 0; i < max_line_num_len; i++)
		printf(" ");
	printf(" | %s", reset_esc);
}

} } // namespace neutrino::util
