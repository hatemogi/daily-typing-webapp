# Daily Typing Webapp Makefile

.PHONY: build serve dev clean help

# Default target
help:
	@echo "Available commands:"
	@echo "  make build  - Compile Elm to JavaScript"
	@echo "  make serve  - Start local HTTP server on port 8000"
	@echo "  make dev    - Build and serve in one command"
	@echo "  make clean  - Clean generated files"

# Build Elm application
build:
	elm make src/Main.elm --output=elm.js

# Start local HTTP server
serve:
	@echo "Starting server at http://localhost:8000"
	@echo "Press Ctrl+C to stop"
	python3 -m http.server 8000

# Development mode: build and serve
dev: build serve

# Clean generated files
clean:
	rm -f elm.js
	rm -rf elm-stuff/