######################################################################
### Base layers, setup working directory and quicklisp
# FROM docker.io/clfoundation/${LISP}:${LISP_VERSION} as base
FROM alpine:3.18.4 as base

RUN mkdir /breeze
WORKDIR /breeze


FROM base as quicklisp

RUN apk add sbcl
COPY scripts/quicklisp.lisp scripts/quicklisp.lisp
RUN sbcl --non-interactive \
     --load scripts/quicklisp.lisp \
     --eval "(quicklisp-quickstart:install)" \
     --eval "(ql-util:without-prompting (ql:add-to-init-file))"

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
FROM quicklisp as test

COPY . .
RUN sbcl --core dependencies.core \
     --eval "(asdf:test-system '#:breeze)"

FROM base as org-publish

RUN apk add bash ca-certificates emacs

COPY . .
COPY --from=test /breeze/docs /breeze/docs

RUN emacs -Q --batch --load scripts/org-publish-project.el --kill
RUN ls
RUN ls /breeze/public

FROM scratch as public

COPY --from=org-publish /breeze/public /


######################################################################
### This is where I left off

# FROM deps as integration-tests
# RUN emacs -batch -l ert -l my-tests.el -f ert-run-tests-batch-and-exit
