import "util/string.hsp"

import <"std/string">

import "util/base.hsp"
import "util/error.hsp"

using std::string::strlen;
using std::string::strcpy;
using std::string::strcat;
using std::string::strncpy;
using std::string::strcmp;

namespace neutrino { namespace util {

func type string* string_init(char* d) {
	type string* ret = n_malloc(sizeof{type string}) as type string*;
	ret->data = n_strdup(d);
	ret->length = strlen(d);
	return ret;
}

func type string* string_init_move(char* d) {
	type string* ret = n_malloc(sizeof{type string}) as type string*;
	ret->data = d;
	ret->length = strlen(d);
	return ret;
}

func unsigned int string_length(type string* s) {
	return s->length;
}

func char* string_data(type string* s) {
	return s->data;
}

func char string_at(type string* s, unsigned int at) {
	maybe_ice(at < s->length, "string_at", "Index out of bounds!");
	return s->data[at];
}

func void string_delete(type string* s) {
	n_free(s->data as byte*);
	n_free(s as byte*);
}

func type string* string_add(type string* s1, type string* s2) {
	char* str = n_malloc((s1->length + s2->length + 1)
		* sizeof{char}) as char*;
	strcpy(str, s1->data);
	strcat(str, s2->data);

	type string* ret = n_malloc(sizeof{type string}) as type string*;
	ret->data = str;
	ret->length = s1->length + s2->length;
	return ret;
}

func type string* string_cadd(const char* c, type string* s) {
	unsigned int c_len = strlen(c);
	char* str = n_malloc((s->length + c_len + 1)
		* sizeof{char}) as char*;
	strcpy(str, c);
	strcat(str, s->data);

	type string* ret = n_malloc(sizeof{type string}) as type string*;
	ret->data = str;
	ret->length = c_len + s->length;
	return ret;
}

func type string* string_addc(type string* s, const char* c) {
	unsigned int c_len = strlen(c);
	char* str = n_malloc((s->length + c_len + 1)
		* sizeof{char}) as char*;
	strcpy(str, s->data);
	strcat(str, c);

	type string* ret = n_malloc(sizeof{type string}) as type string*;
	ret->data = str;
	ret->length = c_len + s->length;
	return ret;
}

func type string* string_chadd(char c, type string* s) {
	char* tmp = stk char(2);
	tmp[0] = c, tmp[1] = '\0';
	return string_cadd(tmp, s);
}

func type string* string_addch(type string* s, char c) {
	char* tmp = stk char(2);
	tmp[0] = c, tmp[1] = '\0';
	return string_addc(s, tmp);
}

func void string_cat(type string* s1, type string* s2) {
	s1->data = n_realloc(s1->data as byte*,
		(s1->length + s2->length + 1) * sizeof{char}) as char*;
	strcat(s1->data, s2->data);
	s1->length += s2->length;
}

func void string_catc(type string* s, const char* c) {
	unsigned int c_len = strlen(c);
	s->data = n_realloc(s->data as byte*,
		(c_len + s->length + 1) * sizeof{char}) as char*;
	strcat(s->data, c);
	s->length += c_len;
}

func void string_ccat(const char* c, type string* s) {
	unsigned int c_len = strlen(c);
	char* n = n_malloc((c_len + s->length + 1) * sizeof{char})
		as char*;
	
	strcpy(n, c);
	strcat(n, s->data);
	
	n_free(s->data as byte*);
	s->data = n;
	s->length += c_len;
}

func void string_catch(type string* s, char c) {
	char* tmp = stk char(2);
	tmp[0] = c, tmp[1] = '\0';
	string_catc(s, tmp);
}

func void string_chcat(char c, type string* s) {
	char* tmp = stk char(2);
	tmp[0] = c, tmp[1] = '\0';
	string_ccat(tmp, s);
}

func char* string_csubstr(type string* s, unsigned int start,
	unsigned int length) {
	if (length == 0) return n_strdup("");

	maybe_ice(start < s->length, "string_substr",
		"Start index out of bounds!");
	maybe_ice(length <= s->length, "string_substr",
		"Desired substring length exceeds string length!");
	maybe_ice(start + length <= s->length, "string_substr",
		"Substring would run past end of string!");

	char* tmp = n_malloc((length + 1) * sizeof{char}) as char*;
	strncpy(tmp, s->data[start]$, length);
	tmp[length] = '\0';
	return tmp;
}

func type string* string_substr(type string* s, unsigned int start,
	unsigned int length) {
	return string_init_move(string_csubstr(s, start, length));
}

func int string_cmp(type string* s1, type string* s2) {
	return strcmp(s1->data, s2->data);
}

func int string_cmpc(type string* s, const char* c) {
	return strcmp(s->data, c);
}

func int string_ccmp(const char* c, type string* s) {
	return strcmp(c, s->data);
}

} } // namespace neutrino::util
