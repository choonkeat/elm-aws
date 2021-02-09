test: build
	elm-verify-examples -r

build:
	find src -iname '*.elm' | while read x; do elm make $$x --output=/dev/null; done
