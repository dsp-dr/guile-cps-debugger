#!/bin/sh

# Create tmux session for CPS debugger demo
# Can be recorded with: asciinema rec -t "Guile CPS Debugger Demo" cps-debugger-demo.cast

SESSION="cps-demo"

# Kill existing session
tmux kill-session -t "$SESSION" 2>/dev/null

# Create new session with first window
tmux new-session -d -s "$SESSION" -n "intro" -c "$(pwd)"

# Window 1: Introduction and simple demo
tmux send-keys -t "$SESSION:intro" "clear" C-m
tmux send-keys -t "$SESSION:intro" "echo '=== Guile CPS Debugger Demo ==='" C-m
tmux send-keys -t "$SESSION:intro" "echo" C-m
tmux send-keys -t "$SESSION:intro" "echo 'This project provides debugging tools for Guile'\''s CPS intermediate representation.'" C-m
tmux send-keys -t "$SESSION:intro" "echo 'CPS (Continuation-Passing Style) is used internally by Guile 3.0+ compiler.'" C-m
tmux send-keys -t "$SESSION:intro" "echo" C-m
tmux send-keys -t "$SESSION:intro" "echo 'Press Enter to see Tree-IL examples (precursor to CPS)...'" C-m
tmux send-keys -t "$SESSION:intro" "read" C-m
tmux send-keys -t "$SESSION:intro" "./simple-demo.scm" C-m

# Window 2: Guile REPL
tmux new-window -t "$SESSION" -n "repl" -c "$(pwd)"
tmux send-keys -t "$SESSION:repl" "clear" C-m
tmux send-keys -t "$SESSION:repl" "echo 'Starting Guile REPL...'" C-m
tmux send-keys -t "$SESSION:repl" "echo 'You can load the CPS debugger modules here.'" C-m
tmux send-keys -t "$SESSION:repl" "echo" C-m
tmux send-keys -t "$SESSION:repl" "guile" C-m

# Window 3: Emacs demo
tmux new-window -t "$SESSION" -n "emacs" -c "$(pwd)"
tmux send-keys -t "$SESSION:emacs" "clear" C-m
tmux send-keys -t "$SESSION:emacs" "echo 'Starting Emacs with CPS debugger configuration...'" C-m
tmux send-keys -t "$SESSION:emacs" "echo 'This will set up Geiser for Guile development.'" C-m
tmux send-keys -t "$SESSION:emacs" "echo" C-m
tmux send-keys -t "$SESSION:emacs" "echo 'Run: emacs -q -l cps-debugger.el'" C-m

# Window 4: Source code
tmux new-window -t "$SESSION" -n "source" -c "$(pwd)"
tmux send-keys -t "$SESSION:source" "clear" C-m
tmux send-keys -t "$SESSION:source" "echo 'CPS Debugger Source Structure:'" C-m
tmux send-keys -t "$SESSION:source" "echo" C-m
tmux send-keys -t "$SESSION:source" "tree -I '*.go' --dirsfirst" C-m

# Select first window
tmux select-window -t "$SESSION:intro"

# Attach or show info
if [ "$1" = "attach" ]; then
    tmux attach-session -t "$SESSION"
else
    echo "Tmux session '$SESSION' created with windows:"
    echo "  - intro:  Introduction and simple demo"
    echo "  - repl:   Guile REPL"
    echo "  - emacs:  Emacs setup instructions"
    echo "  - source: Source code structure"
    echo ""
    echo "To attach: tmux attach-session -t $SESSION"
    echo "To record: asciinema rec -t 'Guile CPS Debugger Demo' cps-debugger-demo.cast"
fi