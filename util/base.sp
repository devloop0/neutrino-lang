import "util/base.hsp"

import <"std/lib">
import <"std/string">

import "util/error.hsp"

using std::lib::malloc;
using std::lib::free;
using std::lib::realloc;
using std::lib::calloc;
using std::lib::NULL;
using std::string::strcpy;
using std::string::strcat;
using std::string::strlen;

namespace neutrino { namespace util {

func byte* n_malloc(unsigned int sz) {
	byte* ret = malloc(sz);
	maybe_ice(ret != NULL, "n_malloc", "malloc() failed!");
	return ret;
}

func void n_free(byte* b) {
	free(b);
}

func byte* n_realloc(byte* r, unsigned int sz) {
	byte* ret = realloc(r, sz);
	maybe_ice(ret != NULL, "n_realloc", "realloc() failed!");
	return ret;
}

func byte* n_calloc(unsigned int n, unsigned int sz) {
	byte* ret = calloc(n, sz);
	maybe_ice(ret != NULL, "n_calloc", "calloc() failed!");
	return ret;
}

func char* n_strdup(const char* str) {
	char* ret = n_malloc((strlen(str) + 1) * sizeof{char})
		as char*;
	return strcpy(ret, str);
}

} } // namespace neutrino::util
