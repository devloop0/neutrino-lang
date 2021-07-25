import "util/generic_funcs.hsp"

import <"std/string">
import <"std/io">

import "util/base.hsp"
import "util/string.hsp"
import "util/vector.hsp"

using std::string::strcmp;
using std::io::printf;

namespace neutrino { namespace util {

func void no_free(byte* b) {}

func void generic_free(byte* b) {
	n_free(b);
}

func void str_free(byte* b) {
	char* c = b as char*;
	n_free(c as byte*);
}

func void ustr_free(byte* b) {
	type string* s = b as type string*;
	string_delete(s);
}

func void vector_free(byte* b) {
	type vector* v = b as type vector*;
	vector_delete(v);
}

func void deref_str_free(byte* b) {
	char* cb = b as char** @;
	n_free(cb as byte*);
}

func void deref_ustr_free(byte* b) {
	type string* sb = b as type string** @;
	string_delete(sb);
}

func void deref_vector_free(byte* b) {
	type vector* v = b as type vector** @;
	vector_delete(v);
}

func bool int_eq(const byte* a, const byte* b) {
	int ia = a as int, ib = b as int;
	return ia == ib;
}

func bool uint_eq(const byte* a, const byte* b) {
	unsigned int uia = a as unsigned int,
		uib = b as unsigned int;
	return uia == uib;
}

func bool str_eq(const byte* a, const byte* b) {
	return !strcmp(a as const char*, b as const char*) as bool;
}

func bool ustr_eq(const byte* a, const byte* b) {
	return !string_cmp(a as type string*, b as type string*) as bool;
}

func bool deref_int_eq(const byte* a, const byte* b) {
	int ia = a as int* @, ib = b as int* @;
	return ia == ib;
}

func bool deref_uint_eq(const byte* a, const byte* b) {
	unsigned int uia = a as unsigned int* @,
		uib = b as unsigned int* @;
	return uia == uib;
}

func bool deref_str_eq(const byte* a, const byte* b) {
	const char* ca = a as const char** @,
		cb = b as const char** @;
	return !strcmp(ca, cb) as bool;
}

func bool deref_ustr_eq(const byte* a, const byte* b) {
	type string* sa = a as type string** @,
		sb = b as type string** @;
	return !string_cmp(sa, sb) as bool;
}

func[static] unsigned int fnv1a_32(const byte* a,
	unsigned int limit, bool is_str) {
	unsigned int hash = 0x811c'9dc5;

	byte* iter = a as byte*;
	for (int i = 0; i < limit; i++) {
		if (is_str && !iter[i] as bool)
			break;

		unsigned int curr = iter[i];
		hash ^= (curr & 0xff);
		hash *= 0x0100'0193;
	}
	return hash;
}

static constexpr unsigned int STR_LIMIT = 10;

func unsigned int ustr_hash(const byte* a) {
	type string* s = a as type string*;
	return fnv1a_32(string_data(s) as byte*, STR_LIMIT, true);
}

func unsigned int str_hash(const byte* a) {
	const char* ca = a as const char*;
	return fnv1a_32(ca as byte*, STR_LIMIT, true);
}

func unsigned int uint_hash(const byte* a) {
	unsigned int ua = a as unsigned int;
	return fnv1a_32(ua$ as byte*, sizeof{unsigned int}, false);
}

func unsigned int int_hash(const byte* a) {
	int ia = a as int;
	return fnv1a_32(ia$ as byte*, sizeof{unsigned int}, false);
}

func void print_int(const byte* a) {
	printf("%d", a as int);
}

func void print_hex(const byte* a) {
	printf("%x", a as unsigned int);
}

func void print_uint(const byte* a) {
	printf("%u", a as unsigned int);
}

func void print_str(const byte* a) {
	printf("\"%s\"", a as const char*);
}

func void print_ustr(const byte* a) {
	type string* s = a as type string*;
	printf("\"%s\"", string_data(s));
}

func void deref_print_int(const byte* a) {
	printf("%d", a as int* @);
}

func void deref_print_hex(const byte* a) {
	printf("%x", a as unsigned int* @);
}

func void deref_print_uint(const byte* a) {
	printf("%u", a as unsigned int* @);
}

func void deref_print_str(const byte* a) {
	printf("\"%s\"", a as const char** @);
}

func void deref_print_ustr(const byte* a) {
	type string* s = a as type string** @;
	printf("\"%s\"", string_data(s));
}

} } // namespace neutrino::util
