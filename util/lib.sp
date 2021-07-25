import "util/lib.hsp"

import <"std/limits">
import <"std/string">

import "util/base.hsp"
import "util/math.hsp"
import "util/string.hsp"
import "util/error.hsp"

using neutrino::util::n_malloc;
using neutrino::util::n_free;
using std::limits::INT_MIN;
using std::string::strcpy;

namespace neutrino { namespace util {

func void inplace_rev_a(char* buf, unsigned int len) {
	for (unsigned int j = 0; j < len - 1; j++) {
		unsigned int fwd = j, rev = (len - 1) - j;
		if (fwd >= rev) break;

		char tmp = buf[fwd];
		buf[fwd] = buf[rev];
		buf[rev] = tmp;
	}
}

func void inplace_rev_ustr(type string* s) {
	inplace_rev_a(string_data(s), string_length(s));
}

func void itoa_na(int i, char* buf) {
	if (i == std::limits::INT_MIN) {
		strcpy(buf, "-2147483648");
		return;
	}
	else if (i == 0) {
		strcpy(buf, "0");
		return;
	}

	bool is_neg = i < 0;
	if (is_neg)
		i *= -1;

	unsigned int pos = 0;
	while (i != 0) {
		buf[pos++] = '0' + (i % 10);
		i /= 10;
	}

	if (is_neg)
		buf[pos++] = '-';
	buf[pos] = '\0';
	
	util::maybe_ice(pos != 0, "itoa_na",
		"Expected at least a single digit written here.");

	inplace_rev_a(buf, pos);
}

func void utoa_na(unsigned int u, char* buf) {
	if (u == 0) {
		strcpy(buf, "0");
		return;
	}

	unsigned int pos = 0;
	while (u != 0) {
		buf[pos++] = '0' + (u % 10);
		u /= 10;
	}
	buf[pos] = '\0';

	util::maybe_ice(pos != 0, "utoa_na",
		"Expected a single digit written here.");

	inplace_rev_a(buf, pos);
}

func char* itoa(int i) {
	unsigned int len = ulen10(i);
	char* ret = n_malloc(sizeof{char} * (len + 1 + 1)) as char*;
	itoa_na(i, ret);
	return ret;
}

func char* utoa(unsigned int u) {
	unsigned int len = ulen10(u);
	char* ret = n_malloc(sizeof{char} * (len + 1)) as char*;
	utoa_na(u, ret);
	return ret;
}

func void itoustr(int i, type string* s) {
	char* tmp = itoa(i);
	string_catc(s, tmp);
	n_free(tmp as byte*);
}

func void utoustr(unsigned int u, type string* s) {
	char* tmp = utoa(u);
	string_catc(s, tmp);
	n_free(tmp as byte*);
}

} } // namespace neutrino::util
