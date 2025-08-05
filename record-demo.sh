#!/bin/sh

# Script to record and convert the CPS debugger demo

CAST_FILE="demo/cps-debugger-demo.cast"
GIF_FILE="demo/cps-debugger-demo.gif"

echo "=== CPS Debugger Demo Recording Script ==="
echo
echo "This will:"
echo "1. Start recording with asciinema"
echo "2. Connect you to the tmux demo session"
echo "3. After you exit, convert to GIF"
echo
echo "Demo steps to perform:"
echo "  1. In 'intro' window: Press Enter to run Tree-IL demo"
echo "  2. Switch to 'repl' window: Show Guile REPL"
echo "  3. Switch to 'source' window: Show project structure"
echo "  4. Exit tmux (Ctrl-b d) to stop recording"
echo
echo "Press Enter to start recording..."
read

# Ensure tmux session exists
./tmux-demo.sh

# Start recording and attach to tmux
echo "Starting asciinema recording..."
asciinema rec -t "Guile CPS Debugger Demo" "$CAST_FILE" -c "tmux attach-session -t cps-demo"

echo
echo "Recording saved to: $CAST_FILE"

# Convert to GIF using agg
if command -v agg >/dev/null 2>&1; then
    echo "Converting to GIF..."
    agg "$CAST_FILE" "$GIF_FILE" \
        --font-size 14 \
        --line-height 1.4 \
        --speed 1.5 \
        --theme monokai
    echo "GIF saved to: $GIF_FILE"
else
    echo "agg not found. Install it to convert to GIF."
fi

# Create README for demo directory
cat > demo/README.md << 'EOF'
# CPS Debugger Demo

This directory contains demo recordings of the Guile CPS Debugger.

## Files

- `cps-debugger-demo.cast` - Original asciinema recording
- `cps-debugger-demo.gif` - Animated GIF version

## Viewing

To replay the asciinema recording:
```bash
asciinema play cps-debugger-demo.cast
```

Or view the GIF in any image viewer/browser.

## Recording a New Demo

Run from the project root:
```bash
./record-demo.sh
```
EOF

echo
echo "Demo recording complete!"
echo "Files created in demo/ directory"