#!/bin/bash
#
# This script is meant to be run as the main script _inside docker_
#
# It takes care of starting sbcl, load breeze, start swank, start xvfb
# and emacs.
#

set -xeuo pipefail

export DEMO_LISTENER_PORT=40050
export DEMO_OUTPUT_DIR=${DEMO_OUTPUT_DIR:-scripts/demo/output/}

mkdir -p ${DEMO_OUTPUT_DIR}

listener_ready_file="listener-ready"
director_logs=${DEMO_OUTPUT_DIR}/emacs-director.log

# Keeping that because I might want to demo something that uses
# quicklisp.
# Example of how to use quicklisp:
#      --load "/root/quicklisp/setup.lisp" \
#      --eval "(asdf:load-asd \"/breeze/breeze.asd\")" \
#      --eval "(ql:quickload '(#:breeze #:swank))" \

# Starting sbcl in a screen
screen -dm sbcl --noinform \
     --core dependencies.core \
     --eval "(asdf:load-asd \"/breeze/breeze.asd\")" \
     --eval "(asdf:load-system '#:breeze :verbose t :force-not (asdf:already-loaded-systems))" \
     --eval "(swank:create-server :dont-close t :port ${DEMO_LISTENER_PORT})" \
     --eval "(with-open-file (_ \"$listener_ready_file\" :if-does-not-exist :create))"

function wait_for_file() {
    while test ! -f $1
    do
        sleep 1
    done
    sleep 1
}

echo "Waiting for swank to startup in the background..."
wait_for_file $listener_ready_file
echo "Swank is up!"

emacs_args=(
    # Don't load any init file
    -Q
    # Load the emacs-director's loader
    -l scripts/emacs-director/util/director-bootstrap.el
    # Run in fullscreen
    --fullscreen
    # Run our demo
    # TODO move to scripts/scripts/demo.el
    -l scripts/demo.el
    # Trying to pass extra arguments
    base-demo
)

function emacs_x11() {
    export DISPLAY=:99

    # -s "-screen 0 1280x800x32"
    xvfb-run -e ${DEMO_OUTPUT_DIR}/xvfb-errors.log emacs "${emacs_args[@]}" &
    ## sleep 2
    ## tail -f $director_logs | sed '/END/q'

    # TODO This is Work in progress..
    # I think I should run the whole x11 stuff in screen
    if command x11vnc; then
        # TODO vnc
        x11vnc -o ${DEMO_OUTPUT_DIR}/x11vnc.log -display ${DISPLAY} -bg
        # -xkb
        # -noxrecord -noxfixes -noxdamage
        # -repeat -nopw
        # -wait 5
        # -permitfiletransfer -tightfilexfer
    fi
}

function emacs_tty() {
    # For debugging
    emacs -nw "${emacs_args[@]}"
}

function wait_for_emacs_to_stop() {
    while pgrep emacs
    do
        sleep 1
    done
}

function dump() (
    set +e
    echo
    find -name '*.log'
    echo
    find / -name 'breeze*.png'
    echo
    cat $director_logs
    echo
    cat ${DEMO_OUTPUT_DIR}/messages.log
    ls ${DEMO_OUTPUT_DIR}
)

trap "echo SIGINT; dump; sh" SIGINT # ^c

emacs_x11
# wait_for_file $director_logs

sleep 1
wait_for_emacs_to_stop
# emacsclient -nw

### below, it's all for testing

dump

# if the standard input is a terminal, then open a shell, for
# interactive debugging
if [ -t 0 ]; then
    bash
fi
