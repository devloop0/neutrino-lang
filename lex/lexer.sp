import "lex/lexer.hsp"

import <"std/lib">
import <"std/io">
import <"std/ctype">

import "lex/token.hsp"
import "util/error.hsp"
import "util/string.hsp"
import "util/base.hsp"
import "util/math.hsp"
import "util/hash_table.hsp"
import "util/generic_funcs.hsp"

using std::ctype::isalnum;
using std::ctype::isalpha;
using std::ctype::isdigit;
using std::ctype::isxdigit;
using std::io::printf;
using std::lib::NULL;
using neutrino::util::n_malloc;
using neutrino::util::n_free;

namespace neutrino { namespace lex {

func[static] type util::hash_table* init_kw2id_map() {
	type util::hash_table* kw2id = util::ht_init(
		util::str_hash, util::str_eq,
		util::no_free, util::no_free);
	util::ht_set(kw2id, "void" as byte*, token_kind::VOID as byte*);
	util::ht_set(kw2id, "bool" as byte*, token_kind::BOOL as byte*);
	util::ht_set(kw2id, "byte" as byte*, token_kind::BYTE as byte*);
	util::ht_set(kw2id, "char" as byte*, token_kind::CHAR as byte*);
	util::ht_set(kw2id, "short" as byte*, token_kind::SHORT as byte*);
	util::ht_set(kw2id, "int" as byte*, token_kind::INT as byte*);
	util::ht_set(kw2id, "long" as byte*, token_kind::LONG as byte*);
	util::ht_set(kw2id, "float" as byte*, token_kind::FLOAT as byte*);
	util::ht_set(kw2id, "double" as byte*, token_kind::DOUBLE as byte*);
	util::ht_set(kw2id, "_" as byte*, token_kind::UNDERSCORE as byte*);

	util::ht_set(kw2id, "signed" as byte*, token_kind::SIGNED as byte*);
	util::ht_set(kw2id, "unsigned" as byte*, token_kind::UNSIGNED as byte*);
	util::ht_set(kw2id, "mut" as byte*, token_kind::MUT as byte*);
	util::ht_set(kw2id, "static" as byte*, token_kind::STATIC as byte*);
	util::ht_set(kw2id, "const" as byte*, token_kind::CONST as byte*);

	util::ht_set(kw2id, "type" as byte*, token_kind::TYPE as byte*);
	util::ht_set(kw2id, "struct" as byte*, token_kind::STRUCT as byte*);
	util::ht_set(kw2id, "union" as byte*, token_kind::UNION as byte*);
	util::ht_set(kw2id, "enum" as byte*, token_kind::ENUM as byte*);
	util::ht_set(kw2id, "fn" as byte*, token_kind::FN as byte*);
	util::ht_set(kw2id, "variant" as byte*, token_kind::VARIANT as byte*);

	util::ht_set(kw2id, "as" as byte*, token_kind::AS as byte*);
	util::ht_set(kw2id, "new" as byte*, token_kind::NEW as byte*);
	util::ht_set(kw2id, "delete" as byte*, token_kind::DELETE as byte*);
	util::ht_set(kw2id, "stk" as byte*, token_kind::STK as byte*);
	util::ht_set(kw2id, "resv" as byte*, token_kind::RESV as byte*);
	util::ht_set(kw2id, "sizeof" as byte*, token_kind::SIZEOF as byte*);
	util::ht_set(kw2id, "alignof" as byte*, token_kind::ALIGNOF as byte*);
	util::ht_set(kw2id, "true" as byte*, token_kind::TRUE as byte*);
	util::ht_set(kw2id, "false" as byte*, token_kind::FALSE as byte*);

	util::ht_set(kw2id, "let" as byte*, token_kind::LET as byte*);
	util::ht_set(kw2id, "using" as byte*, token_kind::USING as byte*);
	util::ht_set(kw2id, "namespace" as byte*, token_kind::NAMESPACE as byte*);
	util::ht_set(kw2id, "import" as byte*, token_kind::IMPORT as byte*);
	util::ht_set(kw2id, "include" as byte*, token_kind::INCLUDE as byte*);
	util::ht_set(kw2id, "module" as byte*, token_kind::MODULE as byte*);
	util::ht_set(kw2id, "asm" as byte*, token_kind::ASM as byte*);
	util::ht_set(kw2id, "defer" as byte*, token_kind::DEFER as byte*);
	util::ht_set(kw2id, "err_defer" as byte*, token_kind::ERR_DEFER as byte*);
	util::ht_set(kw2id, "fun" as byte*, token_kind::FUN as byte*);

	util::ht_set(kw2id, "if" as byte*, token_kind::IF as byte*);
	util::ht_set(kw2id, "else" as byte*, token_kind::ELSE as byte*);
	util::ht_set(kw2id, "while" as byte*, token_kind::WHILE as byte*);
	util::ht_set(kw2id, "for" as byte*, token_kind::FOR as byte*);
	util::ht_set(kw2id, "break" as byte*, token_kind::BREAK as byte*);
	util::ht_set(kw2id, "continue" as byte*, token_kind::CONTINUE as byte*);
	util::ht_set(kw2id, "do" as byte*, token_kind::DO as byte*);
	util::ht_set(kw2id, "switch" as byte*, token_kind::SWITCH as byte*);
	util::ht_set(kw2id, "case" as byte*, token_kind::CASE as byte*);
	util::ht_set(kw2id, "default" as byte*, token_kind::DEFAULT as byte*);
	util::ht_set(kw2id, "return" as byte*, token_kind::RETURN as byte*);
	util::ht_set(kw2id, "err_return" as byte*, token_kind::ERR_RETURN as byte*);
	util::ht_set(kw2id, "goto" as byte*, token_kind::GOTO as byte*);
	util::ht_set(kw2id, "label" as byte*, token_kind::LABEL as byte*);
	util::ht_set(kw2id, "match" as byte*, token_kind::MATCH as byte*);
	
	return kw2id;
}

func type lexer* lexer_init(type util::string* file_name, unsigned int uid,
	type util::string* src) {
	type lexer* l = n_malloc(sizeof{type lexer})
		as type lexer*;
	l->file_name = file_name;
	l->src = src;
	l->pos = 0;
	l->line = l->col = 1;
	l->kw2id = init_kw2id_map();
	l->unique_identifier = uid;
	return l;
}

func void lexer_delete(type lexer* l) {
	util::ht_delete(l->kw2id);
	n_free(l as byte*);
}

func type util::string* lexer_source(type lexer* l) {
	return l->src;
}

func type util::string* lexer_file_name(type lexer* l) {
	return l->file_name;
}

func unsigned int lexer_unique_identifier(type lexer* l) {
	return l->unique_identifier;
}

func[static] void advance(type lexer* l, unsigned int* len) {
	char* src = util::string_data(l->src);
	unsigned int src_len = util::string_length(l->src);

	if (l->pos >= src_len) return;

	if (len != NULL as unsigned int*) len@++;
	if (src[l->pos] == '\n') {
		l->line++;
		l->col = 1;
	}
	else l->col++;

	l->pos++;
}

func[static] bool skip_whitespace(type lexer* l) {
	if (l->pos >= util::string_length(l->src))
		return true;

	char* src = util::string_data(l->src);
	unsigned int src_len = util::string_length(l->src);

	while (l->pos < src_len && (src[l->pos] == ' '
		|| src[l->pos] == '\t' || src[l->pos] == '\n')) {
		advance(l, NULL as unsigned int*);
	}

	return true;
}

func[static] bool skip_comments(type lexer* l) {
	if (l->pos >= util::string_length(l->src))
		return true;

	char* src = util::string_data(l->src);
	unsigned int src_len = util::string_length(l->src);

	if (!skip_whitespace(l))
		return false;

	while (true) {
		if (l->pos >= src_len) break;

		if (src[l->pos] == '#') {
			while (l->pos < src_len && src[l->pos] != '\n')
				advance(l, NULL as unsigned int*);

			advance(l, NULL as unsigned int*);
		}
		else if (l->pos + 1 < src_len && (src[l->pos] == '/'
			&& src[l->pos + 1] == '/')) {
			advance(l, NULL as unsigned int*);
			advance(l, NULL as unsigned int*);

			while (l->pos < src_len && src[l->pos] != '\n')
				advance(l, NULL as unsigned int*);

			advance(l, NULL as unsigned int*);
		}
		else if (l->pos + 1 < src_len && (src[l->pos] == '/'
			&& src[l->pos + 1] == '*')) {
			unsigned int start_pos = l->pos, start_line = l->line,
				start_col = l->col;

			advance(l, NULL as unsigned int*);
			advance(l, NULL as unsigned int*);
			bool closed = false;
			while (l->pos < src_len) {
				if (l->pos + 1 < src_len
					&& src[l->pos] == '*'
					&& src[l->pos + 1] == '/') {
					advance(l, NULL as unsigned int*);
					advance(l, NULL as unsigned int*);
					closed = true;
					break;
				}

				advance(l, NULL as unsigned int*);
			}

			if (!closed) {
				util::report_str(util::error_kind::ERROR,
					"Unclosed multi-line comment starting here.",
					l, start_line, l->line,
					start_col, l->col,
					start_pos, 2);
				return false;
			}
		}
		else
			break;

		if (!skip_whitespace(l))
			return false;
	}

	return true;
}

func[static] type token* handle_identifier(type lexer* l) {
	char* src = util::string_data(l->src);
	unsigned int src_len = util::string_length(l->src);

	if (l->pos >= src_len)
		return NULL as type token*;

	unsigned int start_col = l->col, start_line = l->line,
		start_pos = l->pos;

	if (!isalpha(src[l->pos]) && src[l->pos] != '_')
		return NULL as type token*;

	unsigned int len = 1;
	advance(l, NULL as unsigned int*);
	while (l->pos < src_len && (src[l->pos] == '_'
		|| isalnum(src[l->pos]))) {
		advance(l, len$);
	}

	return token_basic_init(l, start_line, l->line,
		start_col, l->col, start_pos, len, token_kind::IDENT);
}

func[static] unsigned int handle_suffix(type lexer* l, bool integral) {
	char* src = util::string_data(l->src);
	unsigned int src_len = util::string_length(l->src);

	if (l->pos >= src_len)
		return 0;

	switch (src[l->pos]) {
	case 'u':
	case 'U': {
		advance(l, NULL as unsigned int*);
		if (!integral) return 0;

		if (l->pos >= src_len) return 0;

		char ch = src[l->pos];
		advance(l, NULL as unsigned int*);
		switch (ch) {
		case 'i':
		case 'I':
			return suffix_kind::UNSIGNED_INT;
		case 's':
		case 'S':
			return suffix_kind::UNSIGNED_SHORT;
		case 'l':
		case 'L':
			return suffix_kind::UNSIGNED_LONG;
		}
		return 0;
	}
		break;
	case 'f':
	case 'F': {
		advance(l, NULL as unsigned int*);
		if (integral) return 0;

		return suffix_kind::FLOAT;
	}
		break;
	case 'd':
	case 'D': {
		advance(l, NULL as unsigned int*);
		if (integral) return 0;

		return suffix_kind::DOUBLE;
	}
		break;
	case 's':
	case 'S': {
		advance(l, NULL as unsigned int*);
		if (!integral) return 0;

		return suffix_kind::SIGNED_SHORT;
	}
		break;
	case 'l':
	case 'L': {
		advance(l, NULL as unsigned int*);
		if (!integral) return 0;

		return suffix_kind::SIGNED_LONG;
	}
		break;
	}
	return 0;
}

func[static] type token* maybe_handle_decimal(type lexer* l) {
	char* src = util::string_data(l->src);
	unsigned int src_len = util::string_length(l->src);

	if (l->pos >= src_len)
		return NULL as type token*;

	unsigned int start_col = l->col, start_line = l->line,
		start_pos = l->pos;

	type token* ret = NULL as type token*;

	double dec = 0., dec_scale = 10.;
	bool dec_hit = false, exp_hit = false, hex = false, exp_pos = false;
	unsigned int len = 0, exp = 0;

	if (l->pos + 1 < src_len && src[l->pos] == '0'
		&& (src[l->pos + 1] == 'x' || src[l->pos + 1] == 'X')) {
		advance(l, len$), advance(l, len$);

		dec_scale = 16.;
		hex = true;
	}

	double divider = dec_scale;

	while (l->pos < src_len) {
		if (src[l->pos] == '\'')
			advance(l, len$);
		else if (hex && isxdigit(src[l->pos])) {
			char ch = src[l->pos];

			double digit = 0.;
			if ('0' <= ch && ch <= '9')
				digit = ch - '0';
			else if ('a' <= ch && ch <= 'f')
				digit = (ch - 'a') + 10;
			else if ('A' <= ch && ch <= 'F')
				digit = (ch - 'A') + 10;
			else {
				util::ice("maybe_handle_decimal",
					"Unexpected hex digit!");
			}

			if (exp_hit) {
				if (!isdigit(ch)) {
					ret = token_invalid_init(l,
						start_line, l->line,
						start_col, l->col,
						start_pos, len);
					util::report_token(util::error_kind::ERROR,
						"Invalid exponent for hexadecimal floating-point literal.", ret);
					break;
				}

				exp *= 10, exp += ch - '0';
			}
			else if (dec_hit)
				dec += digit / divider, divider *= dec_scale;
			else
				dec *= dec_scale, dec += digit;

			advance(l, len$);
		}
		else if (!hex && isdigit(src[l->pos])) {
			char ch = src[l->pos];
			
			double digit = ch - '0';
			
			if (exp_hit)
				exp *= 10, exp += ch - '0';
			else if (dec_hit)
				dec += digit / divider, divider *= dec_scale;
			else
				dec *= dec_scale, dec += digit;

			advance(l, len$);
		}
		else if (src[l->pos] == '.') {
			if (l->pos + 1 < src_len && isalpha(src[l->pos + 1]))
				break;

			advance(l, len$);

			if (exp_hit) {
				ret = token_invalid_init(l,
					start_line, l->line,
					start_col, l->col,
					start_pos, len);
				util::report_token(util::error_kind::ERROR,
					"Invalid numeric literal here.", ret);
				break;
			}
			else if (dec_hit)
				break;

			dec_hit = true;
		}
		else if ((hex && (src[l->pos] == 'p' || src[l->pos] == 'P'))
			|| (!hex && (src[l->pos] == 'e' || src[l->pos] == 'E'))) {
			advance(l, len$);

			if (l->pos < src_len && isdigit(src[l->pos])) {
				exp_pos = true;
				exp = src[l->pos] - '0';
				advance(l, len$);
			}
			else if (l->pos + 1 < src_len
				&& (src[l->pos] == '+' || src[l->pos] == '-')
				&& isdigit(src[l->pos + 1])) {
				exp_pos = src[l->pos] == '+';
				exp = src[l->pos + 1] - '0';
				advance(l, len$), advance(l, len$);
			}
			else {
				ret = token_invalid_init(l,
					start_line, l->line,
					start_col, l->col,
					start_pos, len);
				util::report_token(util::error_kind::ERROR,
					"Invalid numeric literal here.", ret);
				break;
			}

			exp_hit = true;
		}
		else if (isalpha(src[l->pos])) {
			advance(l, len$);

			ret = token_invalid_init(l,
				start_line, l->line,
				start_col, l->col,
				start_pos, len);
			util::report_token(util::error_kind::ERROR,
				"Invalid floating-point literal here.", ret);
			break;
		}
		else
			break;

	}

	if (exp_hit) {
		double exp_scale = util::fipow(dec_scale, exp);
		if (exp_pos) dec *= exp_scale;
		else dec /= exp_scale;
	}

	unsigned int suffix = suffix_kind::NONE;
	if (l->pos < src_len && src[l->pos] == '_') {
		advance(l, len$);

		bool integral = !exp_hit && !dec_hit;
		unsigned int curr_pos = l->pos;
		suffix = handle_suffix(l, integral);
		len += l->pos - curr_pos;

		if ((suffix == 0 || (l->pos < src_len && (
				isalnum(src[l->pos]) || src[l->pos] == '_')))
			&& ret == NULL as type token*) {
			ret = token_invalid_init(l,
				start_line, l->line,
				start_col, l->col,
				start_pos, len);
			util::report_token(util::error_kind::ERROR,
				"Invalid suffix on floating-point literal here.", ret);
			return ret;
		}
	}

	if (ret == NULL as type token*) {
		if (exp_hit || dec_hit) {
			ret = token_literal_decimal_init(l,
				start_line, l->line,
				start_col, l->col,
				start_pos, len,
				token_kind::DECIMAL, dec, suffix);
		}
		else {
			unsigned int v = dec;
			ret = token_literal_int_init(l,
				start_line, l->line,
				start_col, l->col,
				start_pos, len,
				token_kind::INTEGER, v, suffix);
		}
	}
		
	return ret;
}

func[static] type token* maybe_handle_number_start(type lexer* l) {
	char* src = util::string_data(l->src);
	unsigned int src_len = util::string_length(l->src);

	if (l->pos >= src_len)
		return NULL as type token*;

	unsigned int start_col = l->col, start_line = l->line,
		start_pos = l->pos;

	type token* ret = NULL as type token*;
	switch (src[l->pos]) {
	case '.': {
		advance(l, NULL as unsigned int*);
		if (l->pos + 1 < src_len && src[l->pos] == '.'
			&& src[l->pos + 1] == '.') {
			advance(l, NULL as unsigned int*);
			advance(l, NULL as unsigned int*);
			ret = token_basic_init(l, start_line, l->line,
				start_col, l->col, start_pos, 3,
				token_kind::DOT_DOT_DOT);
			break;
		}
		else if (l->pos >= src_len
			|| (l->pos < src_len && !isdigit(src[l->pos]))) {
			ret = token_basic_init(l, start_line, l->line,
				start_col, l->col, start_pos, 1,
				token_kind::DOT);
			break;
		}

		l->pos = start_pos;
		l->col = start_col;
	}
	case '0': {
		bool fallthrough = true;
		if (src[l->pos] != '.') {
			fallthrough = false;
			advance(l, NULL as unsigned int*);

			unsigned int tmp_val = 0, len = 1;
			bool is_octal = false;

			if (l->pos < src_len && (src[l->pos] == 'x'
				|| src[l->pos] == 'X')) {
				advance(l, len$);

				while (l->pos < src_len
					&& (isxdigit(src[l->pos])
						|| src[l->pos] == '\'')) {
					if (src[l->pos] == '\'') {
						advance(l, len$);
						continue;
					}

					char ch = src[l->pos];
					unsigned int curr_val;
					if ('0' <= ch && ch <= '9')
						curr_val = (ch - '0');
					else if ('a' <= ch && ch <= 'f')
						curr_val = (ch - 'a') + 10;
					else if ('A' <= ch && ch <= 'F')
						curr_val = (ch - 'A') + 10;
					else
						util::ice("lexer",
							"Unrecognized hex digit!");

					tmp_val <<= 4;
					tmp_val |= curr_val;

					advance(l, len$);
				}

				if (l->pos < src_len
					&& (src[l->pos] == '.'
						|| src[l->pos] == 'p')) {
					fallthrough = true;
				}
			}
			else if (l->pos < src_len && (src[l->pos] == 'b'
				|| src[l->pos] == 'B')) {
				advance(l, len$);

				while (l->pos < src_len
					&& (src[l->pos] == '0'
						|| src[l->pos] == '1'
						|| src[l->pos] == '\'')) {
					if (src[l->pos] == '\'') {
						advance(l, len$);
						continue;
					}

					char ch = src[l->pos];
					tmp_val <<= 1;
					tmp_val |= (ch - '0');

					advance(l, len$);
				}

				if (l->pos < src_len && isdigit(src[l->pos])) {
					advance(l, len$);

					ret = token_invalid_init(l,
						start_line, l->line,
						start_col, l->col,
						start_pos, len);
					util::report_token(util::error_kind::ERROR,
						"Invalid binary literal.", ret);
					break;
				}
			}
			else if (l->pos < src_len && (src[l->pos] == 'o'
				|| src[l->pos] == 'O'
				|| isdigit(src[l->pos]))) {
				if (!isdigit(src[l->pos]))
					advance(l, len$);
				is_octal = true;

				while (l->pos < src_len
					&& (('0' <= src[l->pos]
						&& src[l->pos] <= '7')
						|| src[l->pos] == '\'')) {
					if (src[l->pos] == '\'') {
						advance(l, len$);
						continue;
					}

					char ch = src[l->pos];

					tmp_val <<= 3;
					tmp_val |= (ch - '0');

					advance(l, len$);
				}

				if (l->pos < src_len && isdigit(src[l->pos])) {
					advance(l, len$);

					ret = token_invalid_init(l,
						start_line, l->line,
						start_col, l->col,
						start_pos, len);
					util::report_token(util::error_kind::ERROR,
						"Invalid octal literal.", ret);
					break;
				}
			}
			else if (isalnum(src[l->pos])) {
				advance(l, len$);

				ret = token_invalid_init(l,
					start_line, l->line,
					start_col, l->col,
					start_pos, len);
				util::report_token(util::error_kind::ERROR,
					"Invalid start of integral literal.", ret);
				break;
			}

			if (fallthrough) {
				l->pos = start_pos;
				l->col = start_col;
			}
			else if (l->pos < src_len
				&& isalnum(src[l->pos])) {
				advance(l, len$);

				ret = token_invalid_init(l,
					start_line, l->line,
					start_col, l->col,
					start_pos, len);
				util::report_token(util::error_kind::ERROR,
					"Invalid integral literal.", ret);
				break;
			}
			else if (l->pos < src_len && src[l->pos] == '_') {
				advance(l, len$);

				unsigned int curr_pos = l->pos;
				unsigned int suffix = handle_suffix(l, true);
				len += l->pos - curr_pos;
				if (suffix == 0
					|| (l->pos < src_len
						&& (isalnum(src[l->pos])
							|| src[l->pos] == '_'))) {
					advance(l, len$);

					ret = token_invalid_init(l,
						start_line, l->line,
						start_col, l->col,
						start_pos, len);
					util::report_token(util::error_kind::ERROR,
						"Invalid suffix on integral literal.", ret);
					break;
				}
			}
			else if (l->pos < src_len && src[l->pos] == '.') {
				if (is_octal && tmp_val == 0) {
					l->pos = start_pos;
					l->col = start_col;
					fallthrough = true;
				}
				else {
					advance(l, len$);

					ret = token_invalid_init(l,
						start_line, l->line,
						start_col, l->col,
						start_pos, len);
					util::report_token(util::error_kind::ERROR,
						"Cannot have a decimal literal here.", ret);
					break;
				}
			}

			if (!fallthrough) {
				ret = token_literal_int_init(l,
					start_line, l->line,
					start_col, l->col,
					start_pos, len, token_kind::INTEGER,
					tmp_val, suffix_kind::NONE);
				break;
			}
		}
	}
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		ret = maybe_handle_decimal(l);
		break;
	}

	return ret;
}

func[static] type token* handle_char(type lexer* l) {
	char* src = util::string_data(l->src);
	unsigned int src_len = util::string_length(l->src);

	if (l->pos >= src_len)
		return NULL as type token*;

	unsigned int start_pos = l->pos, start_col = l->col,
		start_line = l->line;

	util::maybe_ice(src[l->pos] == '\'', "handle_char",
		"Expected a \"'\" to start a character literal!");

	unsigned int len = 1;
	advance(l, NULL as unsigned int*);

	if (l->pos >= src_len) {
		type token* ret = token_invalid_init(l,
			start_line, l->line,
			start_col, l->col,
			start_pos, len);
		util::report_token(util::error_kind::ERROR,
			"Invalid character literal starting here.", ret);
		return ret;
	}

	if (src[l->pos] == '\\') {
		advance(l, len$);

		if (l->pos >= src_len) {
			type token* ret = token_invalid_init(l,
				start_line, l->line,
				start_col, l->col,
				start_pos, len);
			util::report_token(util::error_kind::ERROR,
				"Invalid character literal starting here.", ret);
			return ret;
		}

		unsigned int val = 0;
		switch (src[l->pos]) {
		case '\'':
			val = '\'';
			break;
		case '\"':
			val = '\"';
			break;
		case '\?':
			val = '\?';
			break;
		case '\\':
			val = '\\';
			break;
		case 'a':
			val = '\a';
			break;
		case 'b':
			val = '\b';
			break;
		case 'f':
			val = '\f';
			break;
		case 'n':
			val = '\n';
			break;
		case 'r':
			val = '\r';
			break;
		case 't':
			val = '\t';
			break;
		case 'v':
			val = '\v';
			break;
		// TODO: Handle octal escape sequences
		// TODO: Handle hex escape sequences
		default: {
			advance(l, len$);
			type token* ret = token_invalid_init(l,
				start_line, l->line,
				start_col, l->col,
				start_pos, len);
			util::report_token(util::error_kind::ERROR,
				"Invalid escape sequence in character literal starting here.", ret);
			return ret;
		}
			break;
		}

		advance(l, len$);
		if (l->pos >= src_len || src[l->pos] != '\'') {
			advance(l, len$);
			type token* ret = token_invalid_init(l,
				start_line, l->line,
				start_col, l->col,
				start_pos, len);
			util::report_token(util::error_kind::ERROR,
				"Expected a \"'\" to end a character literal.", ret);
			return ret;
		}

		advance(l, len$);
		return token_literal_int_init(l,
			start_line, l->line,
			start_col, l->col,
			start_pos, len,
			token_kind::CHARACTER, val, suffix_kind::NONE);
	}
	else {
		bool error = false;
		if (src[l->pos] == '\n') {
			advance(l, len$);
			error = true;
		}
		else if (src[l->pos] == '\'') {
			advance(l, len$);
			error = true;
		}
		
		if (error) {
			type token* ret = token_invalid_init(l,
				start_line, l->line,
				start_col, l->col,
				start_pos, len);
			util::report_token(util::error_kind::ERROR,
				"Invalid character literal starting here.", ret);
			return ret;
		}

		unsigned int val = src[l->pos];
		advance(l, len$);

		if (l->pos >= src_len || src[l->pos] != '\'') {
			advance(l, len$);
			type token* ret = token_invalid_init(l,
				start_line, l->line,
				start_col, l->col,
				start_pos, len);
			util::report_token(util::error_kind::ERROR,
				"Expected a \"'\" to end a character literal.", ret);
			return ret;
		}

		advance(l, len$);
		return token_literal_int_init(l,
			start_line, l->line,
			start_col, l->col,
			start_pos, len,
			token_kind::CHARACTER, val, suffix_kind::NONE);
	}
}

func[static] type token* handle_string(type lexer* l) {
	char* src = util::string_data(l->src);
	unsigned int src_len = util::string_length(l->src);

	if (l->pos >= src_len)
		return NULL as type token*;

	unsigned int start_pos = l->pos, start_col = l->col,
		start_line = l->line;

	util::maybe_ice(src[l->pos] == '\"', "handle_string",
		"Expected a '\"' to start a character literal!");

	unsigned int len = 1;
	advance(l, NULL as unsigned int*);
	unsigned int start_offset = l->pos;

	while (l->pos < src_len && src[l->pos] != '\"') {
		if (src[l->pos] == '\n') {
			advance(l, len$);
			type token* ret = token_invalid_init(l,
				start_line, l->line,
				start_col, l->col,
				start_pos, len);
			util::report_token(util::error_kind::ERROR,
				"Unclosed string literal starting here.", ret);
			return ret;
		}

		if (l->pos + 1 < src_len && src[l->pos] == '\\')
			advance(l, len$);

		advance(l, len$);
	}

	if (l->pos >= src_len) {
		type token* ret = token_invalid_init(l,
			start_line, l->line,
			start_col, l->col,
			start_pos, len);
		util::report_token(util::error_kind::ERROR,
			"Unclosed string literal starting here.", ret);
		return ret;
	}

	util::maybe_ice(src[l->pos] == '\"', "handle_string",
		"Expected a '\"' to end a string literal.");
	unsigned int true_str_len = len - 1;

	advance(l, len$);
	char* sval = util::string_csubstr(l->src, start_offset,
		true_str_len);
	return token_literal_string_init(l,
		start_line, l->line,
		start_col, l->col,
		start_pos, len,
		token_kind::STRING, sval, suffix_kind::NONE);
}

func type token* lex(type lexer* l) {
	if (!skip_whitespace(l))
		return NULL as type token*;
	if (!skip_comments(l))
		return NULL as type token*;

	if (l->pos >= util::string_length(l->src))
		return NULL as type token*;

	char* src = util::string_data(l->src);
	unsigned int src_len = util::string_length(l->src);

	unsigned int start_col = l->col, start_line = l->line,
		start_pos = l->pos;

	type token* ret = NULL as type token*;
	switch (src[l->pos]) {
	case '(':
	case ')':
	case '{':
	case '}':
	case '[':
	case ']':
	case '$':
	case ';':
	case '?':
	case ',':
	case '~':
	case '`':
	case '\\':
	case '@': {
		unsigned int k;
		switch (src[l->pos]) {
		case '`':
			k = token_kind::BACKTICK;
			break;
		case '\\':
			k = token_kind::BACKSLASH;
			break;
		case '(':
			k = token_kind::OPEN_PAR;
			break;
		case ')':
			k = token_kind::CLOSE_PAR;
			break;
		case '{':
			k = token_kind::OPEN_BRACE;
			break;
		case '}':
			k = token_kind::CLOSE_BRACE;
			break;
		case '[':
			k = token_kind::OPEN_BRACKET;
			break;
		case ']':
			k = token_kind::CLOSE_BRACKET;
			break;
		case '$':
			k = token_kind::DOLLAR;
			break;
		case ';':
			k = token_kind::SEMICOLON;
			break;
		case '?':
			k = token_kind::QUESTION_MARK;
			break;
		case ',':
			k = token_kind::COMMA;
			break;
		case '@':
			k = token_kind::AT;
			break;
		case '~':
			k = token_kind::TILDE;
			break;
		default:
			util::ice("lex",
				"Unhandled character.");
		}

		advance(l, NULL as unsigned int*);
		ret = token_basic_init(l, start_line, l->line,
			start_col, l->col, start_pos, 1, k);
	}
		break;
	case '&':
	case '|':
	case '+':
	case '-': {
		char curr = src[l->pos];
		advance(l, NULL as unsigned int*);

		unsigned int k, len = 1;
		switch (curr) {
		case '&':
			k = token_kind::AMP;
			break;
		case '|':
			k = token_kind::BAR;
			break;
		case '+':
			k = token_kind::PLUS;
			break;
		case '-':
			k = token_kind::MINUS;
			break;
		default:
			util::ice("lex", "Unhandled character");
		}

		if (l->pos < src_len) {
			if (src[l->pos] == curr) {
				len = 2;
				advance(l, NULL as unsigned int*);

				switch (curr) {
				case '&':
					k = token_kind::AMP_AMP;
					break;
				case '|':
					k = token_kind::BAR_BAR;
					break;
				case '+':
					k = token_kind::PLUS_PLUS;
					break;
				case '-':
					k = token_kind::MINUS_MINUS;
					break;
				default:
					util::ice("lex", "Unhandled character");
				}
			}
			else if (src[l->pos] == '=') {
				len = 2;
				advance(l, NULL as unsigned int*);

				switch (curr) {
				case '&':
					k = token_kind::AMP_EQ;
					break;
				case '|':
					k = token_kind::BAR_EQ;
					break;
				case '+':
					k = token_kind::PLUS_EQ;
					break;
				case '-':
					k = token_kind::MINUS_EQ;
					break;
				default:
					util::ice("lex", "Unhandled character");
				}
			}
			else if (curr == '-' && src[l->pos] == '>') {
				len = 2;
				advance(l, NULL as unsigned int*);

				k = token_kind::MINUS_GT;
			}
		}

		ret = token_basic_init(l, start_line, l->line,
			start_col, l->col, start_pos, len, k);
		
	}
		break;
	case '^':
	case '*':
	case '/':
	case '%': {
		char curr = src[l->pos];
		advance(l, NULL as unsigned int*);

		unsigned int k, len = 1;
		switch (curr) {
		case '*':
			k = token_kind::STAR;
			break;
		case '/':
			k = token_kind::SLASH;
			break;
		case '%':
			k = token_kind::PERCENT;
			break;
		case '^':
			k = token_kind::CARET;
			break;
		default:
			util::ice("lex", "Unhandled character.");
		}

		if (l->pos < src_len) {
			if (src[l->pos] == '=') {
				len = 2;
				advance(l, NULL as unsigned int*);

				switch (curr) {
				case '*':
					k = token_kind::STAR_EQ;
					break;
				case '/':
					k = token_kind::SLASH_EQ;
					break;
				case '%':
					k = token_kind::PERCENT_EQ;
					break;
				case '^':
					k = token_kind::CARET_EQ;
					break;
				default:
					util::ice("lex",
						"Unhandled character.");
				}
			}
		}

		ret = token_basic_init(l, start_line, l->line,
			start_col, l->col, start_pos, len, k);
	}
		break;
	case '>':
	case '<': {
		char curr = src[l->pos];
		advance(l, NULL as unsigned int*);

		unsigned int k = curr == '>'
			? token_kind::GT
			: token_kind::LT,
			len = 1;
		if (l->pos < src_len) {
			if (src[l->pos] == curr) {
				len = 2;
				advance(l, NULL as unsigned int*);
				k = curr == '>'
					? token_kind::GT_GT
					: token_kind::LT_LT;
				if (l->pos < src_len) {
					if (src[l->pos] == '=') {
						advance(l, NULL as unsigned int*);
						len = 3;
						k = curr == '>'
							? token_kind::GT_GT_EQ
							: token_kind::LT_LT_EQ;
					}
				}
			}
			else if (src[l->pos] == '=') {
				len = 2;
				advance(l, NULL as unsigned int*);
				k = curr == '>'
					? token_kind::GT_EQ
					: token_kind::LT_EQ;
			}
		}

		ret = token_basic_init(l, start_line, l->line,
			start_col, l->col, start_pos, len, k);
	}
		break;
	case '!':
	case '=': {
		char curr = src[l->pos];
		advance(l, NULL as unsigned int*);

		unsigned int k = curr == '!'
			? token_kind::EXCL
			: token_kind::EQ,
			len = 1;
		if (l->pos < src_len) {
			if (src[l->pos] == '=') {
				len = 2;
				advance(l, NULL as unsigned int*);

				k = curr == '!'
					? token_kind::EXCL_EQ
					: token_kind::EQ_EQ;
			}
		}

		ret = token_basic_init(l, start_line, l->line,
			start_col, l->col, start_pos, len, k);
	}
		break;
	case ':': {
		advance(l, NULL as unsigned int*);

		unsigned int k = token_kind::COLON, len = 1;
		if (l->pos < src_len) {
			if (src[l->pos] == ':') {
				advance(l, NULL as unsigned int*);

				len = 2;
				k = token_kind::COLON_COLON;
			}
		}

		ret = token_basic_init(l, start_line, l->line,
			start_col, l->col, start_pos, len, k);
	}
		break;
	case '.':
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		ret = maybe_handle_number_start(l);
		break;
	case '\'':
		ret = handle_char(l);
		break;
	case '\"':
		ret = handle_string(l);
		break;
	default: {
		type token* ident_check = handle_identifier(l);
		if (ident_check == NULL as type token*) {
			ret = token_invalid_init(l, l->line, l->line,
				l->col, l->col + 1, l->pos, 1);

			util::report_str(util::error_kind::ERROR,
				"Invalid character in source stream.",
				l, start_line, l->line,
				start_col, start_col + 1,
				start_pos, 1);

			advance(l, NULL as unsigned int*);
			break;
		}

		ret = ident_check;
		unsigned int k_check;
		char* txt = token_text(ret);
		if (util::ht_get(l->kw2id, txt as byte*, k_check$ as byte**))
			ret->kind = k_check;
	}
		break;
	}

	return ret;
}

} } // namespace neutrino::lex
