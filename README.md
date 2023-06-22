# Disastrous Flying Critters

This is a tic-80 game written in Fennel. It is publicly released on the Tic80 site and on Itch: [https://therabidbanana.itch.io/disastrous-flying-critters](https://therabidbanana.itch.io/disastrous-flying-critters)

To learn more about tic80 visit [https://tic80.com/](https://tic80.com).


## Enhanced Edition

The original version of this was built for Spring Lisp Game Jam 2023 - that version in a single giant file will stay available here: https://github.com/therabidbanana/tic-games/blob/5fcc80b45bf8d3227e6f1bc635db2ac738f87192/dfc.fnl

This version uses a build process with tq-bundler: https://github.com/scambier/TQ-Bundler

Run `make build` to create the finalized build.fnl file for code.

Run `make watch` to set up a watcher that rebuilds the build.fnl file and loads it into Tic80 (assumed to be on Mac installed in /Applications/)

To make a build you will need to use `make watch` and run commands from inside the virtual machine to export apps.


## Deployment

`make push-all` will push new builds for all supported tic80 platforms (currently web + mac/linux/windows)
