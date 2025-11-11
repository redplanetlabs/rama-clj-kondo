#!/usr/bin/env bash
# Script to install pre-push hook for clj-kondo

set -e

HOOK_FILE=".git/hooks/pre-push"

# Create the pre-push hook
cat > "$HOOK_FILE" << 'EOF'
#!/usr/bin/env bash
# Pre-push hook to run clj-kondo linting

echo "Running clj-kondo linting..."

# Run clj-kondo on exports and tests
if clj-kondo --lint clj-kondo.exports test; then
  echo "clj-kondo linting passed"
  exit 0
else
  echo "ERROR: clj-kondo found warnings or errors"
  echo "Please fix linting issues before pushing"
  exit 1
fi
EOF

# Make the hook executable
chmod +x "$HOOK_FILE"

echo "Pre-push hook installed at $HOOK_FILE"
