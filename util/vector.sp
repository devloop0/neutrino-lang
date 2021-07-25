import "util/vector.hsp"

import <"std/lib">
import <"std/string">
import <"std/io">

import "util/base.hsp"
import "util/error.hsp"

using std::string::memcpy;
using std::string::memmove;
using std::io::printf;
using std::lib::NULL;

namespace neutrino { namespace util {

static constexpr unsigned int INIT_CAP = 8;
static constexpr float EXPAND_FACTOR = 1.5;

func type vector* vector_init(unsigned int elem_size, fn void(byte*) ff) {
	type vector* ret = n_malloc(sizeof{type vector})
		as type vector*;
	ret->elem_size = elem_size;
	ret->free_func = ff;
	ret->capacity = INIT_CAP;
	ret->size = 0;
	ret->buffer = n_malloc(ret->elem_size * ret->capacity);
	return ret;
}

func void vector_delete(type vector* v) {
	for (unsigned int i = 0; i < v->size; i++)
		v->free_func(v->buffer[i * v->elem_size]$);
	n_free(v as byte*);
}

func byte* vector_at(type vector* v, unsigned int i) {
	maybe_ice(0 <= i && i < v->size, "vector_at",
		"Index out of bounds!");
	return v->buffer[v->elem_size * i]$;
}

func unsigned int vector_elem_size(type vector* v) {
	return v->elem_size;
}

func[static] void vector_resize(type vector* v) {
	unsigned int new_capacity;
	if (v->size == v->capacity) {
		new_capacity = EXPAND_FACTOR * v->capacity;
	}
	else if (v->size < (v->capacity >> 1)) {
		new_capacity = v->capacity >> 1;
	}
	else return;

	v->capacity = new_capacity;
	v->buffer = n_realloc(v->buffer, v->capacity * v->elem_size);
}

func void vector_append(type vector* v, const byte* b) {
	vector_resize(v);

	memcpy(v->buffer[v->size * v->elem_size]$, b, v->elem_size);
	v->size++;
}

func unsigned int vector_size(type vector* v) {
	return v->size;
}

func byte* vector_data(type vector* v) {
	return v->buffer;
}

func void vector_insert(type vector* v, unsigned int at, const byte* b) {
	maybe_ice(0 <= at && at <= v->size, "vector_insert",
		"Index out of bounds!");
	vector_resize(v);

	unsigned int elems_to_move = (v->size - at);
	memmove(v->buffer[(at + 1) * v->elem_size]$,
		v->buffer[at * v->elem_size]$,
		elems_to_move * v->elem_size);
	memcpy(v->buffer[at * v->elem_size]$, b, v->elem_size);
	v->size++;
}

func bool vector_empty(type vector* v) {
	return v->size == 0;
}

func void vector_pop(type vector* v) {
	maybe_ice(v->size != 0, "vector_pop",
		"Expected a non-empty vector!");
	v->free_func(v->buffer[(v->size - 1) * v->elem_size]$);
	v->size--;
	vector_resize(v);
}

func void vector_remove(type vector* v, unsigned int at) {
	maybe_ice(0 <= at && at < v->size, "vector_remove",
		"Index out of bounds!");

	unsigned int elems_to_move = v->size - (at + 1);
	v->free_func(v->buffer[at * v->elem_size]$);
	memmove(v->buffer[at * v->elem_size]$,
		v->buffer[(at + 1) * v->elem_size]$,
		elems_to_move * v->elem_size);
	v->size--;
	vector_resize(v);
}

func byte* vector_find(type vector* v, const byte* b,
	fn bool(const byte*, const byte*) ef) {
	for (unsigned int i = 0; i < vector_size(v); i++) {
		byte* curr = vector_at(v, i);
		if (ef(b, curr)) return curr;
	}
	return NULL;
}

} } // namespace neutrino::util
