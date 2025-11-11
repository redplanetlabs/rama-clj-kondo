#!/usr/bin/env bash
# Script to install pre-commit hook for cljfmt

set -e

HOOK_FILE=".git/hooks/pre-commit"

# Create the pre-commit hook
cat > "$HOOK_FILE" << 'EOF'
#!/usr/bin/env bash
# Pre-commit hook to run cljfmt on staged .clj and .edn files

# Get list of staged .clj and .edn files
FILES=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(clj|edn)$' || true)

if [ -n "$FILES" ]; then
  echo "Running cljfmt on staged files..."
  
  # Run cljfmt fix on each file
  for file in $FILES; do
    if [ -f "$file" ]; then
      clojure -M:cljfmt fix "$file"
      # Re-stage the file if it was modified
      git add "$file"
    fi
  done
  
  echo "cljfmt completed"
fi

exit 0
EOF

# Make the hook executable
chmod +x "$HOOK_FILE"

echo "Pre-commit hook installed at $HOOK_FILE"
