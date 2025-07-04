
# Run the unit tests
.PHONY: test
test:
	scripts/test.sh

# Generate the documentation
.PHONY: doc
doc:
	scripts/doc.sh

.PHONY: test-emacs
test-emacs:
	guix shell emacs-no-x --container --preserve='^TERM$$' -- emacs -batch --load src/breeze.el --load tests/emacs/no-listener.el -f ert-run-tests-batch-and-exit


DOCKER_BUILD := DOCKER_BUILDKIT=1 docker build --progress=plain

# TODO set -o pipefail

# This is "generic" makefile target. To use it, tweak the variables
# and add it as a dependency.
.PHONY: build-within-container
build-within-container:
	$(DOCKER_BUILD) --target=$(TARGET) --output type=local,dest=$(or $(DEST),.) . 2>&1 | tee $(TARGET).log

# Generate the documentation. in a docker
dependencies.core: Dockerfile breeze.asd scripts/load-dependencies.lisp
	$(MAKE) build-within-container TARGET=dependencies.core

.PHONY: integration
integration: dependencies.core
	$(MAKE) build-within-container TARGET=integration-tests DEST=public

# Generate the documentation (the _public_ website)
.PHONY: public
public: dependencies.core
	$(MAKE) build-within-container TARGET=public DEST=public


.PHONY: list-warnings
list-warnings: public.log
	awk '/; compiling file "\/breeze\/src\//,/About to run tests for the packages/ { $$1=""; $$2=""; print }' $<


# Run some "integration tests" that generates some screenshots
# This is work-in-progress
.PHONY: demo
demo:
	scripts/demo/build-docker-image.sh

# Fix spelling
.PHONY: spell
spell:
	codespell --write-changes --interactive 3 --ignore-words scripts/ignore-words.txt $$(fd -e lisp) README.md notes.org docs/*.md src/breeze.el

.PHONY: watch
watch:
	( fd . -e lisp src/ tests/; echo breeze.asd ) | entr time scripts/test.sh
