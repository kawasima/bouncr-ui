all: build

build:
	elm make src/Main.elm --output=elm.js
