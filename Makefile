# Makefile for age calculator project

# Default target
all: README.md

# Update markdown file when Haskell source changes
README.md: age.hs
	@echo "Updating flowchart in README.md..."
	@./update_flowchart.sh

# Compile the Haskell program
age: age.hs
	@echo "Compiling age.hs..."
	@ghc -o age age.hs

# Clean compiled files
clean:
	@echo "Cleaning compiled files..."
	@rm -f age age.hi age.o

# Run the program
run: age
	@echo "Running age calculator..."
	@./age

# Watch for changes (requires fswatch on macOS)
watch:
	@echo "Watching for changes to age.hs..."
	@fswatch -o age.hs | xargs -n1 -I{} make README.md

# Help
help:
	@echo "Available targets:"
	@echo "  all       - Update markdown file (default)"
	@echo "  README.md - Update flowchart in markdown file"
	@echo "  age       - Compile Haskell program"
	@echo "  run       - Compile and run the program"
	@echo "  clean     - Remove compiled files"
	@echo "  watch     - Watch for changes and auto-update"
	@echo "  help      - Show this help message"

.PHONY: all clean run watch help
