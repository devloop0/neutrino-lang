import "util/hash_table.hsp"

import <"std/lib">
import <"std/string">
import <"std/io">

import "util/base.hsp"
import "util/error.hsp"

using std::lib::NULL;
using std::string::memset;
using std::io::printf;

namespace neutrino { namespace util {

static constexpr float MIN_LOAD_FACTOR = 0.2;
static constexpr float MAX_LOAD_FACTOR = 0.7;
static constexpr unsigned int INIT_CAP = 8;

func type hash_table* ht_init(
	fn unsigned int(const byte*) hf,
	fn bool(const byte*, const byte*) ef,
	fn void(byte*) kff,
	fn void(byte*) vff
) {
	type hash_table* ht = n_malloc(sizeof{type hash_table})
		as type hash_table*;
	ht->array = n_calloc(
		INIT_CAP, sizeof{type hash_table_entry});
	
	ht->size = 0;
	ht->capacity = INIT_CAP;
	ht->hash_func = hf;
	ht->eq_func = ef;
	ht->key_free_func = kff;
	ht->val_free_func = vff;
	return ht;
}

func void ht_delete(type hash_table* ht) {
	for (unsigned int i = 0; i < ht->capacity; i++) {
		type hash_table_entry* hte = ht->array[
			i * sizeof{type hash_table_entry}]$
			as type hash_table_entry*;
		if (hte->check) {
			ht->key_free_func(hte->key);
			ht->val_free_func(hte->value);
		}
	}

	n_free(ht->array);
	n_free(ht as byte*);
}

func unsigned int ht_size(type hash_table* ht) {
	return ht->size;
}

func[static] type hash_table_entry* ht_probe(type hash_table* ht,
	const byte* key, bool stop) {
	unsigned int hv = ht->hash_func(key);
	for (unsigned int i = 0; i < ht->capacity; i++) {
		unsigned int try = (hv + i * i) % ht->capacity;
		type hash_table_entry* curr = ht->array[
			try * sizeof{type hash_table_entry}]$
			as type hash_table_entry*;

		bool done = false;
		if (!curr->check && stop)
			done = true;
		else if (curr->check && ht->eq_func(key, curr->key))
			done = true;

		if (done) return curr;
	}

	return NULL as type hash_table_entry*;
}

func[static] void clear_ht_entry(type hash_table* ht,
	type hash_table_entry* hte) {
	if (hte->check) {
		ht->key_free_func(hte->key);
		ht->val_free_func(hte->value);
		hte->check = false;
	}
}

func[static] void set_ht_entry(type hash_table* ht,
	type hash_table_entry* hte, byte* key, byte* val) {
	clear_ht_entry(ht, hte);
	hte->key = key;
	hte->value = val;
	hte->check = true;
}

func[static] void ht_resize(type hash_table* ht, bool force_expand) {
	unsigned int new_capacity;

	unsigned int max_lf = ht->capacity * MAX_LOAD_FACTOR,
		min_lf = ht->capacity * MIN_LOAD_FACTOR;

	if (ht->size > max_lf || force_expand) {
		new_capacity = ht->capacity << 1;
	}
	else if (ht->size < min_lf) {
		if (ht->capacity <= INIT_CAP) return;

		new_capacity = ht->capacity >> 1;
	}
	else return;

	byte* old_array = ht->array;
	unsigned int old_capacity = ht->capacity;

	ht->array = n_calloc(
		new_capacity, sizeof{type hash_table_entry});
	ht->capacity = new_capacity;

	bool done = false;
	while (!done) {
		done = true;
		for (unsigned int i = 0; i < old_capacity; i++) {
			type hash_table_entry* hte = old_array[
				i * sizeof{type hash_table_entry}]$
				as type hash_table_entry*;
			if (hte->check) {
				type hash_table_entry* probed =
					ht_probe(ht, hte->key, true);
				if (probed == NULL as type hash_table_entry*) {
					done = false;
					break;
				}
				set_ht_entry(ht, probed, hte->key, hte->value);
			}
		}

		if (!done) {
			n_free(ht->array);
			ht->capacity <<= 1;
			ht->array = n_calloc(
				ht->capacity, sizeof{type hash_table_entry});
		}
	}

	n_free(old_array);
}

func void ht_set(type hash_table* ht, byte* key, byte* val) {
	while (true) {
		type hash_table_entry* probed = ht_probe(ht, key, true);

		if (probed != NULL as type hash_table_entry*) {
			bool update_size = !probed->check;
			set_ht_entry(ht, probed, key, val);
			if (update_size) ht->size++;
			break;
		}

		ht_resize(ht, true);
	}

	ht_resize(ht, false);
}

func bool ht_get(type hash_table* ht, byte* key, byte** out) {
	type hash_table_entry* probed = ht_probe(ht, key, false);
	if (probed == NULL as type hash_table_entry*)
		return false;

	if (!probed->check)
		return false;

	out@ = probed->value;
	return true;
}

func bool ht_contains(type hash_table* ht, byte* key) {
	type hash_table_entry* probed = ht_probe(ht, key, false);
	return probed != NULL as type hash_table_entry*
		&& probed->check;
}

func bool ht_remove(type hash_table* ht, byte* key) {
	type hash_table_entry* probed = ht_probe(ht, key, false);

	if (probed == NULL as type hash_table_entry*)
		return false;
	
	if (!probed->check)
		return false;

	clear_ht_entry(ht, probed);
	ht->size--;
	ht_resize(ht, false);
	return true;
}

func[static] void ht_iter_advance_to_next_valid(
	type hash_table_iterator* ht_it
) {
	if (ht_it->elem_index >= ht_size(ht_it->ht)) return;

	while (ht_it->array_index < ht_it->ht->capacity) {
		type hash_table_entry* hte = ht_it->ht->array[
			ht_it->array_index *
				sizeof{type hash_table_entry}]$
			as type hash_table_entry*;

		if (hte->check) break;
		ht_it->array_index++;
	}
}

func type hash_table_iterator* ht_iter_init(type hash_table* ht) {
	type hash_table_iterator* ht_it = n_malloc(sizeof{
		type hash_table_iterator})
		as type hash_table_iterator*;
	ht_it->array_index = 0;
	ht_it->elem_index = 0;
	ht_it->ht = ht;

	ht_iter_advance_to_next_valid(ht_it);
	return ht_it;
}

func void ht_iter_delete(type hash_table_iterator* ht_it) {
	n_free(ht_it as byte*);
}

func void ht_iter_advance(type hash_table_iterator* ht_it) {
	ht_it->elem_index++;
	ht_it->array_index++;
	ht_iter_advance_to_next_valid(ht_it);
}

func bool ht_iter_done(type hash_table_iterator* ht_it) {
	return ht_it->elem_index >= ht_size(ht_it->ht);
}

func void ht_iter_reset(type hash_table_iterator* ht_it) {
	ht_it->array_index = ht_it->elem_index = 0;
	ht_iter_advance_to_next_valid(ht_it);
}

func type hash_table_entry* ht_iter_curr(type hash_table_iterator* ht_it) {
	if (ht_iter_done(ht_it))
		return NULL as type hash_table_entry*;

	type hash_table_entry* hte = ht_it->ht->array[ht_it->array_index
			* sizeof{type hash_table_entry}]$
		as type hash_table_entry*;
	maybe_ice(hte->check, "ht_iter_curr", "Expected a valid hash table entry!");
	return hte;
}

func void ht_dump(type hash_table* ht,
	fn void(const byte*) print_key,
	fn void(const byte*) print_val
) {
	printf("%u: ", ht_size(ht));
	for (unsigned int i = 0; i < ht->capacity; i++) {
		type hash_table_entry* hte = ht->array[
			i * sizeof{type hash_table_entry}]$
			as type hash_table_entry*;
		if (hte->check) {
			printf("| ["), print_key(hte->key),
				printf(", "), print_val(hte->value),
				printf("] ");
		}
		else
			printf("| [-, -] ");
	}
	printf("|\n");
}

} } // namespace neutrino::util
