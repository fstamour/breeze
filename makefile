# use bash in order to use "-o pipefail"
SHELL=bash
# use "-o pipefail" to make sure commands that uses "| tee" to put
# logs into a file fails correctly.
.SHELLFLAGS= -e -u -o pipefail -c
# because guix uses unix epoch as modification times, this tells make
# to check the mtime of the symlink as well as the file it points to
# (not just the file), and take the most recent of both.
MAKEFLAGS += --check-symlink-times

.PHONY: all-tests
all-tests: test test-emacs-unit emacs/breeze-autoloads.el

# Run the unit tests
.PHONY: test
test:
	scripts/test-sbcl.sh

# Build the executable
.PHONY: build
build: build/brz

# TODO add the dependencies...
.PHONY: build/brz
build/brz:
	sbcl \
	--eval '(ql:quickload "breeze")' \
	--eval '(asdf:make "breeze/cli")' \
	--eval '(quit)'

# Generate the documentation
.PHONY: doc
doc:
	scripts/doc.sh

# List of lisp implementations available in guix
LISP_IMPLS := sbcl abcl ecl clisp clasp gcl ccl

# All CI container images
CI_IMAGES := $(LISP_IMPLS) emacs

######################################################################
# Create containers meant to be uploaded (using skopeo), and to be
# used in CI pipelines.
CONTAINER_PREFIX := breeze-ci
CONTAINERS := $(foreach img,$(CI_IMAGES),build/$(CONTAINER_PREFIX)-$(img).tar.gz)

$(CONTAINERS): build/$(CONTAINER_PREFIX)-%.tar.gz: scripts/manifest-%.scm
	mkdir -p build/
	rm -f $@
	rm -f load-$(CONTAINER_PREFIX)-$*
	guix pack -f docker --manifest=$< \
		-S /bin=bin \
		--image-tag=$(CONTAINER_PREFIX)-$* \
		--root=$@ \
		--save-provenance

.PHONY: build-all-ci-images
build-all-ci-images: $(CONTAINERS)

######################################################################
# Targets to load the container images built with guix into docker

LOAD_CONTAINERS := $(foreach img,$(CI_IMAGES),load-$(CONTAINER_PREFIX)-$(img))

LOAD_CONTAINER_WITNESSES := $(foreach target,$(LOAD_CONTAINERS),build/$(target))
$(LOAD_CONTAINER_WITNESSES): build/load-%: build/%.tar.gz
	docker load < $^
	touch $@

.PHONY: $(LOAD_CONTAINERS)
$(LOAD_CONTAINERS): %: build/%

.PHONY: load-all-ci-images
load-all-ci-images: $(LOAD_CONTAINERS)

######################################################################
# Run the tests in a docker container

# TODO generalize (e.g. rename the variable ~lisp~ to something more generic).
GUIX_CONTAINER_TESTS := $(foreach lisp,$(LISP_IMPLS),guix-container-test-$(lisp))
.PHONY: $(GUIX_CONTAINER_TESTS)
$(GUIX_CONTAINER_TESTS): guix-container-test-%: load-$(CONTAINER_PREFIX)-%
	docker run --rm -v $$PWD:/breeze $(CONTAINER_PREFIX)-$* /breeze/scripts/test-$*.sh

######################################################################
# Upload the container images to gitlab
UPLOAD_CI_IMAGES := $(foreach img,$(CI_IMAGES),upload-$(CONTAINER_PREFIX)-$(img))
.PHONY: $(UPLOAD_CI_IMAGES)
$(UPLOAD_CI_IMAGES): upload-%: build/%.tar.gz
	skopeo --insecure-policy copy docker-archive:$^ docker://registry.gitlab.com/fstamour/breeze/$*


######################################################################
# Run the tests in a guix shell container

GUIX_SHELL_TESTS := $(foreach lisp,$(LISP_IMPLS),guix-shell-test-$(lisp))
# $(info GUIX_SHELL_TESTS: $(GUIX_SHELL_TESTS))
.PHONY: $(GUIX-SHELL_TESTS)
$(GUIX_SHELL_TESTS): guix-shell-test-%: scripts/manifest-%.scm
	guix shell \
		--manifest=$^ \
		--container \
		--preserve='^TERM$$' \
		--user=user \
		--network \
		-- ./scripts/test-$*.sh

######################################################################
# Emacs

.PHONY: test-emacs-all
test-emacs-all: emacs/breeze-autoloads.el test-emacs-unit test-emacs-slime test-emacs-sly

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
		--expose=$$PWD/emacs/tests/dot-emacs.d=$$HOME/.emacs.d/ \
		-- emacs

# Run emacs unit tests in a guix container
#
# This target is probably faster than guix-container-test-emacs
# because it doesn't build a ***docker*** container.
.PHONY: test-emacs-unit
test-emacs-unit:
	guix shell \
		--manifest=scripts/manifest-emacs.scm \
		--container \
		--preserve='^TERM$$' \
		--user=user \
		--no-cwd --expose=$$PWD=$$HOME/breeze \
		-- breeze/scripts/test-emacs.sh

# Run the emacs tests in a docker container
# TODO this runs as root (unless docker is running root-less)
.PHONY: guix-container-test-emacs
guix-container-test-emacs: load-$(CONTAINER_PREFIX)-emacs
	docker run --rm -v $$PWD:/breeze $(CONTAINER_PREFIX)-emacs \
		/breeze/scripts/test-emacs.sh

# Run org-publish in a docker container
# TODO this runs as root (unless docker is running root-less)
.PHONY: guix-container-publish-docs
guix-container-publish-docs: load-$(CONTAINER_PREFIX)-emacs
	docker run --rm -v $$PWD:/breeze $(CONTAINER_PREFIX)-emacs \
		/breeze/scripts/publish-docs.sh

### Run the emacs integration (with sly or slime) tests

# sly is incompatible with slime, so we use this environment variable
# to choose which one to test.
export BREEZE_LISTENER

.PHONY: test-emacs-slime
test-emacs-slime: BREEZE_LISTENER=slime
test-emacs-slime: test-emacs-integration

.PHONY: test-emacs-sly
test-emacs-sly: BREEZE_LISTENER=sly
test-emacs-sly: test-emacs-integration

.PHONY: test-emacs-integration
test-emacs-integration:
	guix shell \
		--manifest=scripts/manifest.scm \
		--container \
		--preserve='^TERM$$' \
		--preserve='^BREEZE_LISTENER$$' \
		--user=user \
		--no-cwd \
		--expose=$$PWD=$$HOME/breeze \
		-- breeze/scripts/test-emacs-integration.sh

######################################################################

DOCKER_BUILD := DOCKER_BUILDKIT=1 docker build --progress=plain

# This is a "generic" makefile target. To use it, tweak the variables
# (TARGET and DEST) and add it as a dependency.  See the targets
# "dependencies.core" and "public" for examples
.PHONY: build-within-container
build-within-container:
	$(DOCKER_BUILD) --target=$(TARGET) --output type=local,dest=$(or $(DEST),/dev/null) . 2>&1 | tee $(TARGET).log

# Inside a container, create an sbcl core dump (for caching and faster
# loading (inside a container))
dependencies.core: Dockerfile breeze.asd scripts/load-dependencies.lisp
	$(MAKE) build-within-container TARGET=dependencies.core DEST=.

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

######################################################################

# Fix spelling
.PHONY: spell
spell:
	codespell --write-changes --interactive 3 --ignore-words scripts/ignore-words.txt $$(fd -e lisp) README.md notes.org docs/*.md src/breeze.el

######################################################################

# Run the tests on file change
.PHONY: watch
watch:
	( fd . -e lisp src/ tests/; echo breeze.asd ) | entr -s 'time make test && echo Tests done at $$(date)'

######################################################################
### Targets to help configure git remotes
#
# TODO would be nice if theses could add the "https" version of the
# URLs, for other users than the dev (me).

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

######################################################################

.PHONY: clean
clean:
	rm -f emacs/breeze-autoloads.el
	rm -f cores/dependencies.core
	rm -rf build/
