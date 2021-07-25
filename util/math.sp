import "util/math.hsp"

import "util/error.hsp"

namespace neutrino { namespace util {

func double fipow(double a, unsigned int n) {
	if (n == 0) return 1;
	else if (n == 1) return a;
	else if (n % 2 == 0) {
		double x = fipow(a, n / 2);
		return x * x;
	}
	double x = fipow(a, (n - 1) / 2);
	return x * x * a;
}

func unsigned int ulen10(unsigned int u) {
	unsigned int ret = 1;
	while ((u /= 10) != 0) ret++;

	return ret;
}

} } // namespace neutrino::util
