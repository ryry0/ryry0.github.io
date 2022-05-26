# File extensions
SOURCE_EXT=elm

# Files
SOURCES=src/Main.elm

all:
	elm make $(SOURCES) --output=main.js

format:
	elm-format $(SOURCES)

reactor:
	elm reactor
