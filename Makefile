export GUILE_LOAD_PATH = $(PWD)

.PHONY: test
test:
	guile test/run-test.scm test
