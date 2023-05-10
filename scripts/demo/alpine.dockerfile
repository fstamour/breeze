FROM alpine:3.15 as base

RUN apk add sbcl


FROM base as dependencies

RUN apk add curl

WORKDIR /breeze

# Install quicklisp
COPY scripts/setup-quicklisp.lisp scripts/setup-quicklisp.lisp
RUN cd /tmp && \
    curl -L -o quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --noinform --non-interactive \
      --load quicklisp.lisp \
      --load /breeze/scripts/setup-quicklisp.lisp && \
    rm quicklisp.lisp

# Install some dependencies
COPY scripts/demo/setup-demo.lisp scripts/demo/setup-demo.lisp
COPY breeze.asd breeze.asd
RUN sbcl \
     --load ~/quicklisp/setup.lisp \
     --script scripts/demo/setup-demo.lisp

# Move swank installed by quicklisp to a "well-known" location.
RUN mv ~/quicklisp/$(dirname $(cat ~/quicklisp/dists/quicklisp/installed/systems/swank.txt)) /swank

FROM base as breeze

RUN apk add \
    emacs-x11 \
    xvfb-run \
    ffmpeg \
    scrot \
    screen \
    bash

RUN mkdir -p /breeze /tmp
WORKDIR /breeze

# Copy quicklisp folder
# COPY --from=dependencies /root/quicklisp /root/quicklisp
COPY --from=dependencies /swank /swank
COPY --from=dependencies /breeze/dependencies.core /breeze/dependencies.core

# Copy the sources
COPY . .

# CMD [ "scripts/demo/demo-recorder.sh" ]

FROM breeze as debug

RUN apk add x11vnc
RUN echo screen -ls >> ~/.bash_history
RUN echo /breeze/scripts/demo/demo-recorder.sh >> ~/.bash_history
RUN echo screen -R >> ~/.bash_history



# Run the demo
FROM breeze as demo
RUN scripts/demo/demo-recorder.sh

# Copy only the outputs
FROM scratch
COPY --from=demo /breeze/scripts/demo/output/* /
