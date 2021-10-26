# jinko
<div>
<img align="right" src="misc/logo_small.png" width="10%" height="10%" />

`jinko` is a small and safe interpreted language written in Rust, with integrated
unit tests, mocking and FFI.
</div>

![](https://github.com/cohenarthur/jinko/workflows/jinko-build-and-test/badge.svg?branch=master)
[![codecov](https://codecov.io/gh/CohenArthur/jinko/branch/master/graph/badge.svg?token=37RZPKA62K)](https://codecov.io/gh/CohenArthur/jinko)

While `jinko` can't do a whole lot right now, it's growing fast! You can look
at the roadmap in order to get an idea of what is in and what isn't.
Most of the syntax is implemented at this point, but might not yet produce
valid or convincing results.

Feel free to open an issue in order to get more information about the language
or its usage!

## Community

Feel free to come and chat about jinko on [matrix](https://matrix.to/#/#jinko-lang:matrix.org)!

## Programming in jinko

The standard library is currently very small! It implements basic utilites such as an
optional type or a string library.

Pull requests aiming at enhancing the stdlib are *very* welcome!

## Installation

### Installing from the release page

- Download a release tarball from the [releases page](https://github.com/CohenArthur/jinko/releases)
- Extract it somewhere temporarily: `tar xzf jinko-vx.x.x.tar.gz`

You can even use `/tmp/` or any directory that you'll remove later. Every important file
will be copied to its proper location.

- Run `./install.sh`
This will create a `.jinko` folder in your home directory, in which the binary and libraries
will get installed.

### Installing from source

- Simply run `./install.sh`
This will compile the interpreter in release mode and install it. A `.jinko` folder
will be created in your home directory, in which the binary and libraries
will get installed.

Do not forget to add `$HOME/.jinko/bins` to your path! This will enable you to launch
`jinko` from anywhere.

While `jinko` is not yet available through various distribution package managers, we'd
love to have this!

## Running

Launch the REPL using `jinko` or run a file using `jinko <file>`!

You can use various command line options. They are available when running `jinko -h`

## Contributing

Feel free to submit any observations, bug reports or questions as an [issue](https://github.com/cohenarthur/jinko/issues)

Checkout [CONTRIBUTING.md](CONTRIBUTING.md)if you'd like to write some code!

The code is organized according to the structure defined in [ARCHITECTURE.md](ARCHITECTURE.md).

I am open to mentoring requests and would love to assist you in getting started on the
language.

## Testing

To test jinko, simply run `cargo test && ./tests/func_tests.sh`. Note that in
order to run functional tests, you need to have [ft installed](https://github.com/CohenArthur/ft#installation)

## Requirements

* [ ] If a function does not return `void`, its return value should always be used.
* [x] Variables are immutable by default.
* [ ] Tests should be an integral part of the language
    * [ ] This includes unit testing
    * [ ] This includes mocking
* [x] The language must be simple

Check out jinko's [syntax](SYNTAX.md)!

For more information about how jinko is made, check out its [design](DESIGN.md).

## Thanks to

- [Skallwar](https://github.com/skallwar)
- [SanderJSA](https://github.com/sanderjsa)
- [n1tram1](https://github.com/n1tram1)
- [jh2k2](https://github.com/jh2k2)
- [IFcoltransG](https://github.com/ifcoltransg)

## License

Licensed under [GNU General Public License, version 2](LICENSE)
