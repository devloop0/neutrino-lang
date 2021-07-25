import "util/hash_set.hsp"

import <"std/lib">

import "util/hash_table.hsp"
import "util/base.hsp"
import "util/generic_funcs.hsp"

using std::lib::NULL;

namespace neutrino { namespace util {

func type hash_set* hs_init(
	fn unsigned int(const byte*) hf,
	fn bool(const byte*, const byte*) ef,
	fn void(byte*) kff
) {
	type hash_set* ret = n_malloc(sizeof{type hash_set})
		as type hash_set*;
	ret->ht = ht_init(hf, ef, kff, no_free);
	return ret;
}

func void hs_delete(type hash_set* hs) {
	ht_delete(hs->ht);
	n_free(hs as byte*);
}

func void hs_add(type hash_set* hs, byte* key) {
	byte* tmp = NULL;
	ht_set(hs->ht, key, tmp);
}

func bool hs_remove(type hash_set* hs, byte* key) {
	return ht_remove(hs->ht, key);
}

func bool hs_contains(type hash_set* hs, byte* key) {
	return ht_contains(hs->ht, key);
}

func unsigned int hs_size(type hash_set* hs) {
	return ht_size(hs->ht);
}

func type hash_set_iterator* hs_iter_init(type hash_set* hs) {
	type hash_set_iterator* hs_it = n_malloc(
		sizeof{type hash_set_iterator})
		as type hash_set_iterator*;
	hs_it->hs = hs;
	hs_it->ht_it = ht_iter_init(hs->ht);
	return hs_it;
}

func void hs_iter_delete(type hash_set_iterator* hs_it) {
	ht_iter_delete(hs_it->ht_it);
	n_free(hs_it as byte*);
}

func void hs_iter_advance(type hash_set_iterator* hs_it) {
	ht_iter_advance(hs_it->ht_it);
}

func void hs_iter_reset(type hash_set_iterator* hs_it) {
	ht_iter_reset(hs_it->ht_it);
}

func bool hs_iter_done(type hash_set_iterator* hs_it) {
	return ht_iter_done(hs_it->ht_it);
}

func byte* hs_iter_curr(type hash_set_iterator* hs_it) {
	return ht_iter_curr(hs_it->ht_it)->key;
}

} } // namespace neutrino::util
