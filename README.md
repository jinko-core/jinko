# jinko
<div>
<img align="right" src="misc/logo_small.png" width="10%" height="10%" />

`jinko` is a small and safe interpreted language written in Rust, with integrated
unit tests, mocking and FFI.
</div>

While `jinko` can't do a whole lot right now, it's growing fast! You can look
at the roadmap in order to get an idea of what is in and what isn't.
Most of the syntax is implemented at this point, but might not yet produce
valid or convincing results.

Feel free to open an issue in order to get more information about the language
or its usage!

## Programming in jinko

The lib standard is currently inexistant. `0.1.0` serves as a staple release, in
order to validate the design and the possibilities of the language.

## Installation

For now, installing jinko requires to use cargo's included package manager:

`cargo install --git https://github.com/cohenarthur/jinko`
will net you the latest version from git, using the `master` branch. This branch should
always be stable.

`cargo install jinko` will give you the latest release from `crates.io`. This is equivalent
to downloading a release from the [release page](https://github.com/cohenarthur/jinko/releases)

Once releases start getting interesting, packages for various distributions and
systems will be added, as well as a crate on `crates.io`

Launch the REPL using `jinko` or run a file using `jinko <file>`!

Feel free to submit any observations, bug reports or questions as an [issue](https://github.com/cohenarthur/jinko/issues)

## Contributing

In order to run the unit tests, run `cargo test`.
For functional tests, make sure that [`ft`](https://github.com/cohenarthur/ft) is installed
and run `tests/func_tests.sh` from the root of the repository

Code should be formatted using `cargo fmt`

Feel free to open up PRs and request a review, I'll be happy to do so!

I am available for mentoring requests! If an issue interests you, feel free to comment
on it and I'll be happy to mentor you through its resolution. Check out the
[good first issues](https://github.com/cohenarthur/jinko/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22)
for a solid list of starting points.

## Requirements

* [ ] If a function does not return `void`, its return value should always be used.
* [ ] You can wrap unsound code in `audit` blocks, to make it easier to review.
* [x] Variables are immutable by default.
* [ ] Tests should be an integral part of the language
    * [ ] This includes unit testing
    * [ ] This includes mocking
* [x] The language must be simple

Check out jinko's [syntax](SYNTAX.md)!

For more information about how jinko is made, checkout its [design](DESIGN.md).

## Thanks to

- [Skallwar](https://github.com/skallwar)
- [jh2k2](https://github.com/jh2k2)

## License

Licensed under [GNU General Public License, version 2](LICENSE)
