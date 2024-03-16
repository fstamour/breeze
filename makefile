
# Run the unit tests
test:
	scripts/test.sh

# Generate the documentation
.PHONY: doc
doc:
	scripts/doc.sh

# Generate the documentation. in a docker
dependencies.core: Dockerfile breeze.asd scripts/load-dependencies.lisp
	DOCKER_BUILDKIT=1 docker build --progress=plain --target=$@ --output type=local,dest=. .

.PHONY: public
public: dependencies.core
	DOCKER_BUILDKIT=1 docker build --progress=plain --target=$@ --output type=local,dest=public .

# Run some "integration tests" that generates some screenshots
# This is work-in-progress
demo:
	scripts/demo/build-docker-image.sh

demo-debug:
	scripts/demo/build-docker-image.sh --target debug -t breeze-demo:dev
	docker run -it --rm --name breeze-demo breeze-demo:dev bash

# Fix spelling
spell:
	codespell --write-changes --interactive 3 --ignore-words scripts/ignore-words.txt $$(fd -e lisp) README.md notes.org docs/*.md src/breeze.el

watch:
	( fd . -e lisp src/ tests/; echo breeze.asd ) | entr time scripts/test.sh

.PHONY: \
	test \
	demo \
	spell \
	watch
