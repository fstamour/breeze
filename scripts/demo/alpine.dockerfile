FROM alpine:3.15

RUN mkdir -p /breeze /tmp
WORKDIR /breeze

RUN apk add \
    sbcl \
    emacs \
    xvfb-run \
    ffmpeg \
    scrot

# TODO Install quicklisp

COPY . .
RUN sbcl --load /quicklisp/setup.lisp \
     --script scripts/demo/setup-demo.lisp

CMD [ "scripts/demo/demo-recorder.sh", "run" ]