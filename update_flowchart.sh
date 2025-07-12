#!/bin/bash

# Script to extract Mermaid diagram from age.hs and update README.md

HASKELL_FILE="age.hs"
MARKDOWN_FILE="README.md"

# Extract the Mermaid diagram from the Haskell file
extract_mermaid() {
    sed -n '/MERMAID_DIAGRAM_START/,/MERMAID_DIAGRAM_END/p' "$HASKELL_FILE" | \
    sed '1d;$d' | \
    sed 's/^[[:space:]]*//' | \
    sed 's/^|[[:space:]]*//'
}

# Check if files exist
if [[ ! -f "$HASKELL_FILE" ]]; then
    echo "Error: $HASKELL_FILE not found"
    exit 1
fi

if [[ ! -f "$MARKDOWN_FILE" ]]; then
    echo "Error: $MARKDOWN_FILE not found"
    exit 1
fi

# Extract the diagram
MERMAID_CONTENT=$(extract_mermaid)

if [[ -z "$MERMAID_CONTENT" ]]; then
    echo "Error: No Mermaid diagram found in $HASKELL_FILE"
    echo "Make sure the diagram is wrapped with MERMAID_DIAGRAM_START and MERMAID_DIAGRAM_END comments"
    exit 1
fi

# Create a temporary file with the new content
TEMP_FILE=$(mktemp)

# Write everything before the mermaid block
sed -n '1,/^```mermaid$/p' "$MARKDOWN_FILE" > "$TEMP_FILE"

# Add the extracted mermaid content
echo "$MERMAID_CONTENT" >> "$TEMP_FILE"

# Write everything after the mermaid block
sed -n '/^```$/,$p' "$MARKDOWN_FILE" >> "$TEMP_FILE"

# Replace the original file
mv "$TEMP_FILE" "$MARKDOWN_FILE"

echo "Successfully updated $MARKDOWN_FILE with diagram from $HASKELL_FILE"
