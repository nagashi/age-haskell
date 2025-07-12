# Makefile for age calculator project

# Default target
all: README.md

# Update markdown file when Haskell source changes
README.md: app/Main.hs
	@echo "Updating flowchart in README.md..."
	@./update_flowchart.sh

# Build the Haskell program using Cabal
build: app/Main.hs
	@echo "Building with Cabal..."
	@cabal build

# Clean compiled files
clean:
	@echo "Cleaning compiled files..."
	@cabal clean

# Run the program using Cabal
run: build
	@echo "Running age calculator..."
	@cabal run age

# Watch for changes (requires fswatch on macOS)
watch:
	@echo "Watching for changes to app/Main.hs..."
	@fswatch -o app/Main.hs | xargs -n1 -I{} make README.md

# Help
help:
	@echo "Available targets:"
	@echo "  all       - Update markdown file (default)"
	@echo "  README.md - Update flowchart in markdown file"
	@echo "  build     - Build Haskell program with Cabal"
	@echo "  run       - Build and run the program"
	@echo "  clean     - Remove compiled files"
	@echo "  watch     - Watch for changes and auto-update"
	@echo "  help      - Show this help message"

.PHONY: all build clean run watch help
