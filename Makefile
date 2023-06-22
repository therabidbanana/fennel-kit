project = flying-critters
appname = Flying Critters
itchproj = therabidbanana/disastrous-flying-critters
tic = /Applications/tic80.app/Contents/MacOS/tic80

build: build-dir
	tq-bundler run src/game.fnl src/main.fnl

watch:
	tq-bundler run src/game.fnl src/main.fnl --tic ${tic}

build-dir:
	mkdir -p build

mac-raw: build
	${tic} --cli  --fs=. --cmd "load src/game.fnl & load src/build.fnl code & export mac build/${project}-mac"

mac-app: mac-raw
	rm -rf build/${project}.app &> /dev/null
	cp -R shells/mac/shell.app build/${project}.app
	cp build/${project}-mac build/${project}.app/Contents/MacOS/
	sed -e "s/PROJECT/${project}/" -e "s/APPNAME/${appname}/" shells/mac/shell.app/Contents/Info.plist > build/${project}.app/Contents/Info.plist

mac: mac-app
	zip -vr build/${project}.app.zip build/${project}.app -x "*.DS_Store"

win: build
	${tic} --cli  --fs=. --cmd "load src/game.fnl & load src/build.fnl code & export win build/${project}-win"

linux: build
	${tic} --cli  --fs=. --cmd "load src/game.fnl & load src/build.fnl code & export linux build/${project}-linux"

html: build
	${tic} --cli  --fs=. --cmd "load src/game.fnl & load src/build.fnl code & export html build/${project}-html"

tic: build
	${tic} --cli  --fs=. --cmd "load src/game.fnl & load src/build.fnl code & save build/${project}.tic"

png: build
	${tic} --cli  --fs=. --cmd "load src/game.fnl & load src/build.fnl code & save build/${project}.png"

clean:
	rm -rf build/

push-mac: mac
	 butler push build/${project}.app.zip ${itchproj}:mac

push-win: win
	 butler push build/${project}-win.exe ${itchproj}:win

push-linux: linux
	 butler push build/${project}-linux ${itchproj}:linux

push-html: html
	 butler push build/${project}-html.zip ${itchproj}:html

push-all: push-html push-mac push-win push-linux tic
	 butler push build/${project}.tic ${itchproj}:tic
