# jinko
`jinko` is a small and safe interpreted language written in Rust, with integrated
unit tests, mocking and FFI.

<p align="center">
<img src="misc/logo_small.png" />
</p>

## Requirements

* [ ] If a function does not return `void`, its return value should always be used.
* [ ] You can wrap unsound code in `audit` blocks, to make it easier to review.
* [ ] Variables are immutable by default.
* [ ] Tests should be an integral part of the language
    * [ ] This includes unit testing
    * [ ] This includes mocking
* [ ] The language must be simple

Check out jinko's [syntax](SYNTAX.md)!

For more information about how jinko is made, checkout its [design](DESIGN.md).
