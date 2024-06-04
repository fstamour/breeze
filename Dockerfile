ARG LISP=sbcl
ARG LISP_VERSION=2.2.4

######################################################################
### Base layers, setup working directory and quicklisp
FROM docker.io/clfoundation/${LISP}:${LISP_VERSION} as base

RUN mkdir /breeze
WORKDIR /breeze


FROM base as quicklisp

RUN QUICKLISP_DIST_VERSION=latest QUICKLISP_ADD_TO_INIT_FILE=true /usr/local/bin/install-quicklisp

######################################################################
### Download all needed dependencies (for the main and the test
### systems).
FROM quicklisp as deps

COPY breeze.asd .
COPY scripts/load-dependencies.lisp scripts/load-dependencies.lisp

RUN sbcl --noinform --non-interactive \
    --load scripts/load-dependencies.lisp


FROM scratch as dependencies.core

COPY --from=deps /breeze/dependencies.core /dependencies.core

######################################################################
### Run the tests and generate some documentation
FROM base as test

COPY . .
RUN sbcl --core dependencies.core \
     --eval "(asdf:test-system '#:breeze)"


FROM alpine:3.18.4 as org-publish

RUN mkdir /breeze
WORKDIR /breeze

RUN apk add bash ca-certificates emacs

COPY . .
COPY --from=test /breeze/docs /breeze/docs

# RUN DEBIAN_FRONTEND=noninteractive apt update && apt install -yq emacs
RUN emacs -Q --batch --load scripts/org-publish-project.el --kill
RUN ls
RUN ls /breeze/public

FROM scratch as public

COPY --from=org-publish /breeze/public /


######################################################################
### This is where I left off

FROM deps as integration-tests

# Ok, the base image is old af... so I'm going to try to make an
# executable, and run it in an alpine-base layer that has a newer
# emacs... _Maybe_ alpine is not the best idea because of the
# libc... The base image uses debian, I should probably stick with it.
RUN apt update && apt install -y -q --force-yes emacs
RUN emacs -batch -l ert -l my-tests.el -f ert-run-tests-batch-and-exit
