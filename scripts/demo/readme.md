This is heavily work-in-progress, but I've got a lot of parts going:

- Used nix to create a base docker image with pretty much everything I
could ever want to create a demo of breeze inside emacs. See
`build-docker-image.sh`

- The image can be run interactively with `run-demo-recorder.sh`

- [ ] Create a non-root user in the docker image.

- Using `xvfb-run emacs`, I can run emacs in a virtual frame buffer.

- xvfb-run creates a 640x480 buffer by default, it _might_ be okay for
  demo though. `-s "-screen 0 1280x800x32"`

- I can take screenshots using scrot. It doesn't need any special
  arguments, it creates a png screenshot with a timestamp and the
  resolution in the current directory.

- I thought I should call scrot and ffmpeg from inside emacs, because
  it's the only one that knows when stuff is initialized.

- I should be able to take videos using ffmpeg. `ffmpeg -y -r 30 -f
  x11grab -i :94.0 output.mp4`

- In order to take screenshots or capture: `export DISPLAY=:99`

- `../demo.sh` contains an example of how to start emacs with a
  specific script

- I can use emacs-director, to simulate a user that run commands in
  emacs. see `../demo.el`

- Maybe I could use imagemagick to add annotations to the screenshots