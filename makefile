# File extensions
SOURCE_EXT=elm

# Files
SOURCES=src/Main.elm

all:
	elm make $(SOURCES) --output=index2.html

format:
	elm-format $(SOURCES)

reactor:
	elm reactor
