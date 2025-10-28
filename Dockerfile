######################################################################
### Base layers, setup working directory and quicklisp
# FROM docker.io/clfoundation/${LISP}:${LISP_VERSION} as base
FROM alpine:3.18.4 AS base

RUN mkdir /breeze
WORKDIR /breeze


FROM base AS quicklisp

RUN apk add sbcl
COPY scripts/quicklisp.lisp scripts/quicklisp.lisp
RUN sbcl --non-interactive \
     --load scripts/quicklisp.lisp \
     --eval "(quicklisp-quickstart:install)" \
     --eval "(ql-util:without-prompting (ql:add-to-init-file))"

######################################################################
### Download all needed dependencies (for the main and the test
### systems).
FROM quicklisp AS deps

COPY breeze.asd .
COPY scripts/load-dependencies.lisp scripts/load-dependencies.lisp

RUN sbcl --noinform --non-interactive \
    --load scripts/load-dependencies.lisp


FROM scratch AS dependencies.core

COPY --from=deps /breeze/dependencies.core /dependencies.core

######################################################################
### Run the tests and generate some documentation
FROM quicklisp AS test

COPY . .
RUN sbcl --core dependencies.core \
     --eval "(asdf:test-system '#:breeze)"

FROM base AS org-publish

RUN apk add bash ca-certificates emacs

COPY . .
COPY --from=test /breeze/docs /breeze/docs

RUN emacs -Q --batch --load scripts/org-publish-project.el --kill
RUN ls
RUN ls /breeze/public

FROM scratch AS public

COPY --from=org-publish /breeze/public /


######################################################################
### This is where I left off

FROM deps AS integration-tests-base

RUN apk add bash ca-certificates emacs
RUN apk add scrot screen ffmpeg xvfb-run
RUN sbcl --noinform --non-interactive --eval "(ql:quickload '#:swank)"

COPY . .

FROM integration-tests-base AS debug

# RUN apk add x11vnc
RUN echo screen -ls >> ~/.bash_history
RUN echo /breeze/scripts/demo/demo-recorder.sh >> ~/.bash_history
RUN echo screen -R >> ~/.bash_history

FROM integration-tests-base AS integration-tests-run

# TODO
# Run some test (fails because slime is not started)
# RUN emacs -batch -l ert -l /breeze/tests/emacs/breeze-test.el -f ert-run-tests-batch-and-exit

# TODO emacs (x11) complains that stdin is not a tty, might need to run it under screen
#
# -t file, --terminal=file
#                      Use specified file as the terminal instead of using stdin/stdout.  This must be the first argument specified in the command line.
#
# Run the demo
RUN scripts/demo/demo-recorder.sh

# Copy only the outputs
FROM scratch AS integration-tests
COPY --from=integration-tests-run /breeze/scripts/demo/output/* /

# for image in $(docker exec breeze-demo-recorder sh -c 'ls /breeze/scripts/demo/*.png'); do
#     name=$(basename $image)
#     docker cp breeze-demo-recorder:$image $output/$name
# done
