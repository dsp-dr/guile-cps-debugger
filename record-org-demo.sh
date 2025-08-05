#!/bin/bash

# Script to record Org mode CPS debugger demo with asciinema in tmux

echo "Starting Org mode CPS Debugger demo recording..."
echo "This will create a new tmux session and record with asciinema"
echo ""

# Create new tmux session for recording
tmux new-session -d -s cps-demo-record

# Start asciinema recording in the tmux session
tmux send-keys -t cps-demo-record "asciinema rec --title 'Guile CPS Debugger - Org Mode Interactive Demo' --idle-time-limit 3 demo/org-mode-demo.cast" C-m
sleep 2

# Start emacs with the demo
tmux send-keys -t cps-demo-record "emacs -nw -q -l cps-debugger.el DEMO.org" C-m
sleep 3

# Attach to the session so user can interact
echo "Attaching to tmux session 'cps-demo-record'"
echo ""
echo "=== DEMO WALKTHROUGH GUIDE ==="
echo "1. Wait for Emacs to load"
echo "2. Press C-c C-v s to start Scheme session"
echo "3. Navigate through sections with C-n/C-p"
echo "4. Execute code blocks with C-c C-c"
echo "5. Switch to REPL with C-c C-z when needed"
echo "6. Exit Emacs with C-x C-c"
echo "7. Exit asciinema with 'exit' command"
echo "8. Detach from tmux with C-b d if needed"
echo ""
echo "Press Enter to attach to the session..."
read

tmux attach-session -t cps-demo-record