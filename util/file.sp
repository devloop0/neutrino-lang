import "util/file.hsp"

import <"std/lib">
import <"std/syscall">
import <"std/string">

import "util/base.hsp"
import "util/error.hsp"

using std::syscall::stat;
using std::syscall::direct_stat;
using std::syscall::direct_open;
using std::syscall::direct_read;
using std::syscall::direct_close;
using std::syscall::O_RDONLY;
using std::syscall::S_ISREG;
using std::syscall::S_ISDIR;
using std::syscall::PATH_MAX;
using std::syscall::direct_getcwd;
using std::lib::NULL;
using std::string::strlen;
using std::string::strcat;
using neutrino::util::n_malloc;
using neutrino::util::n_free;

namespace neutrino { namespace util {

func bool file_exists(const char* path) {
	type stat st;
	int ret_code = direct_stat(path, st$);

	if (ret_code == -1) return false;
	return ret_code == 0 && S_ISREG(st.st_mode);
}

func bool dir_exists(const char* path) {
	type stat st;
	int ret_code = direct_stat(path, st$);

	if (ret_code == -1) return false;
	return ret_code == 0 && S_ISDIR(st.st_mode);
}

func char* read_file(const char* path) {
	type stat st;
	int ret_code = direct_stat(path, st$);

	if (ret_code == -1) return NULL as char*;

	int fd = direct_open(path, O_RDONLY);
	if (fd == -1) return NULL as char*;

	char* ret = n_malloc((st.st_size + 1) * sizeof{char})
		as char*;

	ret_code = direct_read(fd, ret, st.st_size);
	if (ret_code < 0) return NULL as char*;
	ret[st.st_size as unsigned int] = 0;

	direct_close(fd);
	return ret;
}

func char* getcwd() {
	char* tmp = n_malloc(sizeof{char} * PATH_MAX) as char*;
	if (direct_getcwd(tmp, PATH_MAX) < 0) {
		n_free(tmp as byte*);
		return NULL as char*;
	}

	return tmp;
}

func unsigned int get_inode(const char* path) {
	type stat st;
	int ret_code = direct_stat(path, st$);

	if (ret_code == -1) return -1;

	return st.st_ino;
}

func char* pathcat(const char* first, const char* rest) {
	unsigned int first_len = strlen(first), rest_len = strlen(rest);

	unsigned int full_len = first_len;
	bool add_slash = first_len == 0 || first[first_len - 1] != '/';
	if (add_slash)
		full_len++;
	full_len += rest_len;
	full_len++;

	char* ret = n_malloc(sizeof{char} * full_len) as char*;
	strcat(ret, first);
	if (add_slash)
		strcat(ret, "/");
	strcat(ret, rest);
	return ret;
}

} } // namespace neutrino::util
