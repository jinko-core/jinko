## Issues

### Mentoring

If you would like to work on `jinko`, but with guidance, mentoring is available. You'll
be guided through the codebase and will get a chance to work on something that interests
you. Feel free to leave an issue or contact me.

### What to work on

You can check out easy issues
[here](https://github.com/cohenarthur/jinko/issues?q=is%3Aopen+is%3Aissue+label%3A"good+first+issue").
They are marked with the `good first issue` label. They require some programming
knowledge, but no particular knowledge of the jinko codebase. Feel free to ask for advice
directly on the issues!

If you are interested by a particular concept (adding a JIT, adding an optimizer, C
transpilation...) feel free to open up an issue and start working on it. I'd be delighted
to talk about your idea.

### Claiming an issue

Drop a message in the issue asking to work on it. If no one is already assigned, then
you'll get it. If someone is already assigned to the issue but hasn't worked on it in a
while, then that assignment will be revoked and yours will be put through.

### Creating a pull request

Fork the project and create a new branch. Work on this branch, commiting as you will,
then submit a pull request to the `master` branch of the main project. Your work will
be reviewed and tested by the CI, and then merged into the project!
Feel free to assign people as reviewers. You can also mark a PR as a `Draft`, if you'd like
some early feedback or indications. Once your PR is out of `Draft`, it will be merged
if approved and if the tests pass.

## Documentation

You can build jinko's documentation like so:
```sh
cargo doc --open
```

In case it opens in the wrong browser, export the `BROWSER` variable prior to this command
```sh
export BROWSER=<your browser>
cargo doc --open
```

The documentation is a good starting point to dig deep into jinko's implementation.
Examples on how to use the different types and method, and how they relate to jinko code
are given.

The code you produce should be documented and tested as much as possible. Considering the
fact that `jinko` is a programming language, a lot of behaviors have to be tested
externally.
