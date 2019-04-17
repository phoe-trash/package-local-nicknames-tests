# package-local-nicknames-tests

A public test suite for package local nicknames.

`package-local-nicknames-tests.lisp` should not be modified unless you are
adding implementation-specific behavior, such as package locks. To add your
implemementation, modify `package.lisp` to import the proper symbols from the
package of your implementation's choosing.

To run the tests, call `(package-local-nicknames-test:run)`.

This test code is based on the SBCL tests for package local nicknames.

## Current state

* SBCL: PASS
* CCL: PASS
* ABCL: PASS
* ECL: FAIL ([1](https://gitlab.com/embeddable-common-lisp/ecl/issues/466),
  [2](https://gitlab.com/embeddable-common-lisp/ecl/issues/467))

Other implementations are untested.

## License

Unlicense / public domain.
