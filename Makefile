watch:
	@fswatch -o ./src | while read -r event; do \
	echo "rebuilding..." ;\
	elm make src/Main.elm --output main.js ;\
	done
build:
	elm make ./src/Main.elm --output ./docs/index.html
