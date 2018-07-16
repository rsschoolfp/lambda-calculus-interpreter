# lambda-calculus-interpreter

[![Build Status](https://travis-ci.org/rsschoolfp/lambda-calculus-interpreter.svg?branch=master)](https://travis-ci.org/rsschoolfp/lambda-calculus-interpreter)

## Testing

To run tests execute

    $ stack test

To re-run tests on file changes execute

    $ stack test --file-watch

To add a new test, create a module inside `tests` directory, this module should
export its tests as `TestTree` datatype. Then import this new module to
`tests/Spec.hs`, and add its tests to list of other tests in `main` (see
`ShadowingTest` for example).
