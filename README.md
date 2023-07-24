# Fennel Kit

This is a tic-80 game framework written in Fennel. It was extracted from work building Disastrous Flying Critters: [https://therabidbanana.itch.io/disastrous-flying-critters](https://therabidbanana.itch.io/disastrous-flying-critters)

To learn more about tic80 visit [https://tic80.com/](https://tic80.com).


## Building

This version uses a build process with tq-bundler: https://github.com/scambier/TQ-Bundler

Run `make build` to create the finalized build.fnl file for code.

Run `make watch` to set up a watcher that rebuilds the build.fnl file and loads it into Tic80 (assumed to be on Mac installed in /Applications/)

To make a build you will need to use `make watch` and run commands from inside the virtual machine to export apps.


## Deployment

`make push-all` will push new builds for all supported tic80 platforms (currently web + mac/linux/windows)
