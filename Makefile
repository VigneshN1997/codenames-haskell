STACK       = stack

.PHONY: all test build clean distclean turnin

all: test

test: clean
	$(STACK) test --test-arguments="--num-threads 1"

clean:

distclean: clean
	$(STACK) clean
