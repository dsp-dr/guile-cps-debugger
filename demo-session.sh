#!/bin/sh

# Script to run the CPS debugger demo in tmux
# This can be recorded with asciinema

SESSION_NAME="cps-debugger-demo"

# Kill existing session if it exists
tmux kill-session -t "$SESSION_NAME" 2>/dev/null

# Create new session
tmux new-session -d -s "$SESSION_NAME" -n "demo"

# Window 1: Basic examples
tmux send-keys -t "$SESSION_NAME:demo" "cd /home/dsp-dr/ghq/github.com/dsp-dr/guile-cps-debugger" C-m
tmux send-keys -t "$SESSION_NAME:demo" "clear" C-m
tmux send-keys -t "$SESSION_NAME:demo" "echo '=== Guile CPS Debugger Demo ==='" C-m
tmux send-keys -t "$SESSION_NAME:demo" "echo" C-m
tmux send-keys -t "$SESSION_NAME:demo" "echo 'First, let'\''s compile the debugger...'" C-m
tmux send-keys -t "$SESSION_NAME:demo" "gmake clean && gmake" C-m

# Create second window for REPL demo
tmux new-window -t "$SESSION_NAME" -n "repl"
tmux send-keys -t "$SESSION_NAME:repl" "cd /home/dsp-dr/ghq/github.com/dsp-dr/guile-cps-debugger" C-m
tmux send-keys -t "$SESSION_NAME:repl" "clear" C-m

# Switch back to first window
tmux select-window -t "$SESSION_NAME:demo"

# Attach to session
if [ "$1" = "attach" ]; then
    tmux attach-session -t "$SESSION_NAME"
else
    echo "Tmux session '$SESSION_NAME' created."
    echo "To attach: tmux attach-session -t $SESSION_NAME"
    echo "To record with asciinema: asciinema rec cps-debugger-demo.cast"
fi