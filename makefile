
# Run the unit tests
.PHONY: test
test:
	scripts/test.sh

# Generate the documentation
.PHONY: doc
doc:
	scripts/doc.sh

DOCKER_BUILD := DOCKER_BUILDKIT=1 docker build --progress=plain

.PHONY: build-within-container
build-within-container:
	$(DOCKER_BUILD) --target=$(TARGET) --output type=local,dest=$(or $(DEST),.) . 2>&1 | tee $(TARGET).log

# Generate the documentation. in a docker
dependencies.core: build-within-container
dependencies.core: TARGET=dependencies.core
dependencies.core: Dockerfile breeze.asd scripts/load-dependencies.lisp

.PHONY: integration
integration: TARGET=test
integration: DEST=public
integration: build-within-container dependencies.core

.PHONY: public
public: TARGET=public
public: DEST=public
public: build-within-container dependencies.core


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
