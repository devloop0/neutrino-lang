# Neutrino Programming Language

Neutrino is a new systems programming language that aims to be version 2.0 of the [Spectre](https://github.com/devloop0/spectre-lang) programming language. It therefore (obviously) takes heavy inspiration from Spectre (and therefore C and C++), but also languages like Rust, D, and Zig.

## Dependencies

- [Spectre](https://github.com/devloop0/spectre-lang)
- `make`
- `as`/`ld`

Note that since the Spectre compiler targets ARM (specifically ARMv7), to run the Neutrino compiler, you will need to have an ARM machine (the Spectre compiler's README details how you can do this).

I personally have been testing the compiler with a QEMU installation emulating a Raspberry Pi; I run my Spectre compiler from my x86-64 machine, generating ARM assembly and the assemble the code using a cross-assembler to generate ARM object files. Then, on my QEMU installation, I link the object files to generate an executable.

## Compiler Setup

Assuming you have already cloned this repository, then you can run the following the build the compiler (note, you may need to change the `SP` and `AS` variables at the beginning of the `Makefile` if you have different locations for your assembler and/or Spectre compiler).

```
$ make setup
$ make compile
$ make link
```

That's it! You may need to run `make link` on your target machine if you are cross-compiling.

## Running

Building the compiler should have produced a `build/neutrinoc` executable. As of now, there's not much the compiler can do other than typecheck your program, so let's look at some examples that exercise this.

### Example #1

Consider the following erroneous program:

```
namespace testing::types {
	type i32 = signed int;

	struct Point { x, y: i32; }
	struct Rect { a, b : Point; }
} // namespace testing::types

using namespace testing::types;

namespace testing::point_ops {

fun to_point(let (x, y) : (i32, i32)) : Point {
	return Point { .x = x, .y = y };
}

fun scale(let pt : Point, let s : i32) : Point {
	return Point { .x = pt.x * s, .y = pt.y * s };
}

} // namespace testing::point_ops

namespace testing::rect_ops {

fun to_rect(let (pt1, pt2) : (Point, Point)) : Rect {
	return Rect { .a = pt1, .b = pt2 };
}

fun diag_squared_length(let r : * Rect) : i32 {
	let Rect {
		.a: Point { .x: x0, .y: y0 },
		.b: Point { .x: x1, .y: y1 } } = r@;
	let xdist = x0 - x1, ydist = y0 - y1;
	return xdist * xdist + ydist * ydist;
}

} // namespace testing::rect_ops

fun main() : i32 {
	let t0 = (2, 1), t1 = (-2, -1);
	let a = t0.to_point!(), b = t1.to_point!();
	let result = (a.scale!(2), b.scale!(3))
		.to_rect!()
		.diag_squared_length!();
}
```

The syntax is mostly correct and demonstrates some highlights of the language, including:
- tuples and `struct`'s.
- pattern matching in a Rust-like way (e.g. with the `struct`de-structuring in the `diag_squared_length` function).
- Uniform function call syntax (UFCS), similar to D. More specifically, when a `.` or `->` member access is followed by an `!`, functions across all namespaces with the same name and matching first arguments is looked up.

The error here is that `diag_squared_length` expects a `* Rect` (a pointer to a `Rect`), but `.to_rect!()` returns a `Rect`. We need to either store the `Rect` and take its address or change the signature of `diag_squared_length`. Here is the error message outputted:
```
Error[/home/artoria/tmp/t.n:43:4]: Could not resolve UFCS lookup for 'diag_squared_length' here with starting type: 'Rect [::testing::types::Rect]'.
  41 |     let result = (a.scale!(2), b.scale!(3))
     |                  ~~~~~~~~~~~~~~~~~~~~~~~~~~
  42 |         .to_rect!()
     |         ~~~~~~~~~~~
  43 |         .diag_squared_length!();
     |         ~^~~~~~~~~~~~~~~~~~~~

```

Note the error message highlights exactly where the problem is with source context and descriptive type names as well.

### Example #2

Now consider a more functional (erroneous) example:

```
fun abort() : void;

namespace testing::types {

type i32 = signed int;
variant IntOption { None; Some : * mut i32; }

} // namespace testing::types

using namespace testing::types;

namespace testing::option_ops {

fun unwrap(let opt : IntOption) : * i32 {
	match (opt) {
		Some (x) : { return x; }
		_ : abort();
	}
}

} // namespace testing::option_ops;

fun main() : i32 {
	let mut x = 5;
	let opt = Some(x$);
	opt.unwrap!() @ ++;
}
```

The error here is that `unwrap` returns a `* i32` which is a constant pointer to constant bytes. However, in `main`, we dereference (`@`), and increment the result of the `unwrap` procedure, which is invalid, since we can dereference and change the pointee of a constant pointer.

Here is the error message for reference:
```
Error[/home/artoria/tmp/t.n:26:18]: Expected a 'mut', non-constant value to increment.
  26 |     opt.unwrap!() @ ++;
     |     ~~~~~~~~~~~~~~~ ^~
```

This example illustrates some more aspects of Neutrino:
- pattern matching via match statements
- sum types (`variant`'s) and their constructors

There is a lot more to be said about Neutrino, but hopefully this is a nice introduction to it!

## TODO

- At least finishing the IR code generator and actual backend code generator are the top priorities.
- Language-wise, there is a primitive inclusion mechanism (similar to C's `#include` statements), but it would be nice to have an actual module system in addition to this.
