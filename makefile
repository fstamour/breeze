.SHELL=sh

.PHONY: all-tests
all-tests: test test-emacs

# Run the unit tests
.PHONY: test
test:
	scripts/test.sh

# Generate the documentation
.PHONY: doc
doc:
	scripts/doc.sh

# Work in progress
# Create a container meant to be uploaded (using skopeo), and to be used in CI pipelines.
guix-container-for-ci.tar.gz: scripts/manifest.scm
	guix pack -f docker --manifest=$< --image-tag=$(subst .tar.gz,,$@) --root=$@ --save-provenance

emacs/breeze-autoloads.el: emacs/breeze.el
	emacs -batch -f loaddefs-generate-batch emacs/breeze-autoloads.el emacs

# use guix shell to run emacs in a container
# uses X11
.PHONY:
launch-emacs:
	guix shell \
		--manifest=scripts/manifest.scm \
		--container --preserve='^TERM$$' \
		--user=user \
		--preserve='^XAUTHORITY$$' --expose="$${XAUTHORITY}" --preserve='^DISPLAY$$' \
		--network \
		--no-cwd --expose=$$PWD=$$HOME/breeze \
		--expose=$$PWD/emacs/dot-emacs.el=$$HOME/.emacs \
		-- emacs

# Run **SOME** emacs test in a container, using guix
.PHONY: test-emacs
test-emacs:
	guix shell \
		--manifest=scripts/manifest.scm \
		--container --preserve='^TERM$$' \
		--user=user \
		--no-cwd --expose=$$PWD=$$HOME/breeze \
		-- emacs --batch \
		--load \~/breeze/emacs/breeze.el \
		--load \~/breeze/tests/emacs/no-listener.el \
		-f ert-run-tests-batch-and-exit


DOCKER_BUILD := DOCKER_BUILDKIT=1 docker build --progress=plain

# TODO set -o pipefail

# This is "generic" makefile target. To use it, tweak the variables
# (TARGET and DEST) and add it as a dependency.
# See the targets "dependencies.core" and "public" for examples
.PHONY: build-within-container
build-within-container:
	$(DOCKER_BUILD) --target=$(TARGET) --output type=local,dest=$(or $(DEST),.) . 2>&1 | tee $(TARGET).log

# Inside a container, create an sbcl core dump (for caching and faster
# loading (inside a container))
dependencies.core: Dockerfile breeze.asd scripts/load-dependencies.lisp
	$(MAKE) build-within-container TARGET=dependencies.core

.PHONY: integration
integration: dependencies.core
	$(MAKE) build-within-container TARGET=integration-tests DEST=public/integration-tests
	mv integration-tests.log public/integration-tests/

# Generate the documentation (the _public_ website)
.PHONY: public
public: dependencies.core
	$(MAKE) build-within-container TARGET=public DEST=public

.PHONY: list-warnings
list-warnings: public.log
	awk '/; compiling file "\/breeze\/src\//,/About to run tests for the packages/ { $$1=""; $$2=""; print }' $<

# Fix spelling
.PHONY: spell
spell:
	codespell --write-changes --interactive 3 --ignore-words scripts/ignore-words.txt $$(fd -e lisp) README.md notes.org docs/*.md src/breeze.el

# Run the tests on file change
.PHONY: watch
watch:
	# TODO include breeze.asd
	( fd . -e lisp src/ tests/; echo breeze.asd ) | entr -s 'make test && echo Tests done at $$(date)'

######################################################################
### Targets to help configure git remotes

.PHONY:
git-setup-remotes: git-add-codeberg git-add-gitlab git-add-github

.PHONY:
git-add-codeberg: name=codeberg
git-add-codeberg: url=ssh://git@codeberg.org/fstamour/breeze.git
git-add-codeberg: git-add-remote

.PHONY:
git-add-gitlab: name=gitlab
git-add-gitlab: url=git@gitlab.com:fstamour/breeze.git
git-add-gitlab: git-add-remote

.PHONY:
git-add-github: name=github
git-add-github: url=git@github.com:fstamour/breeze.git
git-add-github: git-add-remote

.PHONY:
git-add-remote:
	git remote add $(name) $(url) 2>/dev/null || git remote set-url $(name) $(url)
	git remote -v
