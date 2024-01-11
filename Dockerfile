ARG LISP=sbcl
ARG LISP_VERSION=2.2.4

FROM clfoundation/${LISP}:${LISP_VERSION} as base

RUN mkdir /breeze
WORKDIR /breeze


FROM base as quicklisp

RUN QUICKLISP_DIST_VERSION=latest QUICKLISP_ADD_TO_INIT_FILE=true /usr/local/bin/install-quicklisp


FROM quicklisp as deps

COPY breeze.asd .
COPY scripts/load-dependencies.lisp scripts/load-dependencies.lisp

RUN sbcl --noinform --non-interactive \
    --load scripts/load-dependencies.lisp


FROM scratch as dependencies.core

COPY --from=deps /breeze/dependencies.core /dependencies.core


FROM base as doc

COPY . .
RUN sbcl --core dependencies.core \
     --eval "(asdf:load-system '#:breeze/doc)" \
     --eval '(breeze.documentation::generate-documentation)'

FROM alpine:3.18.4 as org-publish

RUN mkdir /breeze
WORKDIR /breeze

RUN apk add bash ca-certificates emacs

COPY . .
COPY --from=doc /breeze/public /breeze/public

# RUN DEBIAN_FRONTEND=noninteractive apt update && apt install -yq emacs
RUN emacs -Q --batch --load scripts/org-publish-project.el --kill
RUN ls
RUN ls /breeze/public


FROM scratch as public

COPY --from=org-publish /breeze/public /
