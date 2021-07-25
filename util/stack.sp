import "util/stack.hsp"

import "util/base.hsp"
import "util/error.hsp"
import "util/vector.hsp"

namespace neutrino { namespace util {

func type stack* stack_init(unsigned int elem_size, fn void(byte*) ff) {
	type stack* s = n_malloc(sizeof{type stack})
		as type stack*;
	s->buffer = vector_init(elem_size, ff);
	return s;
}

func void stack_delete(type stack* s) {
	vector_delete(s->buffer);
	n_free(s as byte*);
}

func byte* stack_at(type stack* s, unsigned int at) {
	maybe_ice(at < vector_size(s->buffer), "stack_at",
		"Index out of bounds!");
	return vector_at(s->buffer, at);
}

func byte* stack_top(type stack* s) {
	maybe_ice(stack_size(s) != 0, "stack_top",
		"Expected a non-empty stack to get its top element!");
	return stack_at(s, stack_size(s) - 1);
}

func byte* stack_data(type stack* s) {
	return vector_data(s->buffer);
}

func unsigned int stack_size(type stack* s) {
	return vector_size(s->buffer);
}

func void stack_push(type stack* s, const byte* b) {
	return vector_append(s->buffer, b);
}

func void stack_pop(type stack* s) {
	return vector_pop(s->buffer);
}

func unsigned int stack_elem_size(type stack* s) {
	return vector_elem_size(s->buffer);
}

func byte* stack_find(type stack* s, const byte* b,
	fn bool(const byte*, const byte*) ef) {
	return vector_find(s->buffer, b, ef);
}

} } // namespace neutrino::util
