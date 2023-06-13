build:
	tq-bundler run src/game.fnl src/main.fnl

watch:
	tq-bundler run src/game.fnl src/main.fnl --tic /Applications/tic80.app/Contents/MacOS/tic80
