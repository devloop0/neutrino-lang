import "lex/token.hsp"

import <"std/io">
import <"std/string">

import "util/base.hsp"
import "util/error.hsp"
import "util/string.hsp"
import "lex/lexer.hsp"

using std::io::printf;
using std::string::strncpy;
using neutrino::util::n_malloc;
using neutrino::util::n_free;

namespace neutrino { namespace lex {

func[static] void set_token_text(type token* tok) {
	tok->text = n_malloc(sizeof{char} * (token_length(tok) + 1))
		as char*;

	char* src = util::string_data(lexer_source(
		token_ctx(tok)));
	strncpy(tok->text, src[token_pos(tok)]$, token_length(tok));
	tok->text[token_length(tok)] = '\0';
}

func type token* token_basic_init(
	type lexer* c, 
	unsigned int sln, unsigned int eln,
	unsigned int sc, unsigned int ec,
	unsigned int sp, unsigned int l,
	unsigned int k
) {
	type token* tok = n_malloc(sizeof{type token})
		as type token*;
	tok->ctx = c;
	tok->start_line_num = sln;
	tok->end_line_num = eln;
	tok->start_col = sc;
	tok->end_col = ec;
	tok->start_pos = sp;
	tok->length = l;
	
	tok->kind = k;
	tok->value_kind = value_kind::NONE;
	tok->suffix_kind = suffix_kind::NONE;
	set_token_text(tok);
	return tok;
}

func type token* token_literal_int_init(
	type lexer* c,
	unsigned int sln, unsigned int eln,
	unsigned int sc, unsigned int ec,
	unsigned int sp, unsigned int l,
	unsigned int k, unsigned int v, unsigned int sk
) {
	type token* tok = n_malloc(sizeof{type token})
		as type token*;
	tok->ctx = c;
	tok->start_line_num = sln;
	tok->end_line_num = eln;
	tok->start_col = sc;
	tok->end_col = ec;
	tok->start_pos = sp;
	tok->length = l;
	
	tok->kind = k;
	tok->value_kind = value_kind::INTEGRAL;
	tok->suffix_kind = sk;

	tok->value.integral_value = v;
	set_token_text(tok);
	return tok;
}

func type token* token_literal_decimal_init(
	type lexer* c,
	unsigned int sln, unsigned int eln,
	unsigned int sc, unsigned int ec,
	unsigned int sp, unsigned int l,
	unsigned int k, double v, unsigned int sk
) {
	type token* tok = n_malloc(sizeof{type token})
		as type token*;
	tok->ctx = c;
	tok->start_line_num = sln;
	tok->end_line_num = eln;
	tok->start_col = sc;
	tok->end_col = ec;
	tok->start_pos = sp;
	tok->length = l;
	
	tok->kind = k;
	tok->value_kind = value_kind::DECIMAL;
	tok->suffix_kind = sk;

	tok->value.decimal_value = v;
	set_token_text(tok);
	return tok;
}

func type token* token_literal_string_init(
	type lexer* c,
	unsigned int sln, unsigned int eln,
	unsigned int sc, unsigned int ec,
	unsigned int sp, unsigned int l,
	unsigned int k, char* s, unsigned int sk) {
	type token* tok = n_malloc(sizeof{type token})
		as type token*;
	tok->ctx = c;
	tok->start_line_num = sln;
	tok->end_line_num = eln;
	tok->start_col = sc;
	tok->end_col = ec;
	tok->start_pos = sp;
	tok->length = l;

	tok->kind = k;
	tok->value_kind = value_kind::STRING;
	tok->suffix_kind = sk;

	tok->value.string_value = s;
	set_token_text(tok);
	return tok;
}

func type token* token_invalid_init(
	type lexer* c,
	unsigned int sln, unsigned int eln,
	unsigned int sc, unsigned int ec,
	unsigned int sp, unsigned int l
) {
	type token* tok = n_malloc(sizeof{type token})
		as type token*;
	tok->ctx = c;
	tok->start_line_num = sln;
	tok->end_line_num = eln;
	tok->start_col = sc;
	tok->end_col = ec;
	tok->start_pos = sp;
	tok->length = l;

	tok->kind = token_kind::INVALID;
	tok->value_kind = value_kind::NONE;
	tok->suffix_kind = suffix_kind::NONE;

	set_token_text(tok);
	return tok;
}

func void token_delete(type token* tok) {
	if (tok->value_kind == value_kind::STRING)
		util::n_free(tok->value.string_value as byte*);
	util::n_free(tok->text as byte*);
	util::n_free(tok as byte*);
}

func type lexer* token_ctx(type token* tok) {
	return tok->ctx;
}

func unsigned int token_start_line(type token* tok) {
	return tok->start_line_num;
}

func unsigned int token_end_line(type token* tok) {
	return tok->end_line_num;
}

func unsigned int token_start_col(type token* tok) {
	return tok->start_col;
}

func unsigned int token_end_col(type token* tok) {
	return tok->end_col;
}

func unsigned int token_pos(type token* tok) {
	return tok->start_pos;
}

func unsigned int token_length(type token* tok) {
	return tok->length;
}

func unsigned int token_tkind(type token* tok) {
	return tok->kind;
}

func unsigned int token_vkind(type token* tok) {
	return tok->value_kind;
}

func unsigned int token_skind(type token* tok) {
	return tok->suffix_kind;
}

func type value_types* token_value(type token* tok) {
	return tok->value$;
}

func char* token_text(type token* tok) {
	return tok->text;
}

func void token_print(type token* tok) {
	char* src = util::string_data(lexer_source(
		token_ctx(tok)));
	printf("[\"%s\" ", util::string_data(lexer_file_name(tok->ctx)));

	if (token_start_line(tok) == token_end_line(tok))
		printf("%u:", token_start_line(tok));
	else {
		printf("%u-%u:", token_start_line(tok),
			token_end_line(tok));
	}
	printf("%u-%u ", token_start_col(tok), token_end_col(tok));

	printf("{txt: \"%s\"} ", token_text(tok));
	
	switch (tok->value_kind) {
	case value_kind::INTEGRAL:
		printf("{int value: %u/%d/0x%x} ",
			tok->value.integral_value,
			tok->value.integral_value,
			tok->value.integral_value);
		break;
	case value_kind::DECIMAL: {
		char* tmp = tok->value.decimal_value$ as char*;
		printf("{dec value: 0x");
		for (unsigned int i = 8; i > 0; i--) {
			unsigned int curr = tmp[i - 1];
			printf("%x%x", (curr >> 4) & 0xf, curr & 0xf);
		}
		printf("} ");
	}
		break;
	case value_kind::STRING:
		printf("{str value: <|%s|>} ",
			tok->value.string_value);
		break;
	}

	if (tok->suffix_kind != suffix_kind::NONE) {
		printf("{suffix: '");
		switch (tok->suffix_kind) {
		case suffix_kind::FLOAT:
			printf("f");
			break;
		case suffix_kind::DOUBLE:
			printf("d");
			break;
		case suffix_kind::UNSIGNED_INT:
			printf("ui");	
			break;
		case suffix_kind::UNSIGNED_LONG:
			printf("ul");
			break;
		case suffix_kind::SIGNED_LONG:
			printf("l");
			break;
		case suffix_kind::UNSIGNED_SHORT:
			printf("us");
			break;
		case suffix_kind::SIGNED_SHORT:
			printf("s");
			break;
		default:
			util::ice("print_token",
				"Invalid suffix_kind found!");
		}
		printf("'} ");
	}

	printf("0x%x]", token_tkind(tok));
}

} } // namespace neutrino::lex
