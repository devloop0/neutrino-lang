import "ast/ast.hsp"

import <"std/lib">

import "util/base.hsp"
import "util/vector.hsp"

using std::lib::NULL;
using neutrino::util::n_malloc;
using neutrino::util::n_free;

namespace neutrino { namespace ast {

func type metadata* metadata_init(type util::vector* ts,
	unsigned int s, unsigned int e) {
	type metadata* m = n_malloc(sizeof{type metadata})
		as type metadata*;
	m->token_stream = ts;
	m->start = s;
	m->end = e;
	return m;
}

func void metadata_delete(type metadata* m) {
	n_free(m as byte*);
}

func type metadata* metadata_copy(type metadata* m) {
	if (m == NULL as type metadata*)
		return NULL as type metadata*;
	return metadata_init(m->token_stream, m->start, m->end);
}

} } // namespace neutrino::ast
