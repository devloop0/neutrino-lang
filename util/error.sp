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

namespace neutrino { namespace util {

static unsigned int REPORTING_STACK = 0;

func void ice(const char* fnc, const char* msg) {
	printf("ICE[%s]: %s\n", fnc, msg);
	abort();
}

func void maybe_ice(bool cond, const char* fnc, const char* msg) {
	if (!cond) ice(fnc, msg);
}

static type error_counts ec;

func unsigned int num_errors() {
	return ec.num_errors;
}

func unsigned int num_warnings() {
	return ec.num_warnings;
}

func unsigned int num_notes() {
	return ec.num_notes;
}

func void disable_reporting() {
	REPORTING_STACK++;
}

func void enable_reporting() {
	util::maybe_ice(REPORTING_STACK != 0,
		"enable_reporting", "Cannot enable reporting if it is already enabled");
	REPORTING_STACK--;
}

func bool is_reporting_enabled() {
	return REPORTING_STACK == 0;
}

func unsigned int total_messages() {
	return ec.num_errors + ec.num_warnings + ec.num_notes;
}

func void error_kind_update_count(unsigned int k) {
	switch(k) {
	case error_kind::ERROR:
		ec.num_errors++;
		break;
	case error_kind::WARNING:
		ec.num_warnings++;
		break;
	case error_kind::NOTE:
		ec.num_notes++;
		break;
	default:
		ice("error_kind_update_count",
			"Unrecognized error_kind here!");
	}
}

} } // namespace neutrino::util
