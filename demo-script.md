# CPS Debugger Demo Script

## Recording Steps

### 1. Start Recording
```bash
./record-demo.sh
```

### 2. Window: intro
- Wait 2 seconds for viewers to read
- Press Enter to run the Tree-IL demo
- Let it complete (shows 4 examples)
- Wait 2 seconds

### 3. Switch to Window: repl (Ctrl-b 2)
- Type in Guile REPL:
```scheme
(use-modules (system base compile))
(compile '(lambda (x) (+ x 1)) #:to 'tree-il)
```
- Show the Tree-IL output
- Type:
```scheme
,q
```

### 4. Switch to Window: source (Ctrl-b 4)
- Already shows tree structure
- Wait 3 seconds for viewers to see structure

### 5. Switch to Window: emacs (Ctrl-b 3)
- Show the Emacs setup command
- Wait 2 seconds

### 6. Exit tmux
- Press Ctrl-b d to detach and stop recording

## Alternative Demo (with working modules)

If you have Guile 3.0+ installed:

### In REPL window:
```scheme
(add-to-load-path ".")
(use-modules (cps-debugger))
,cps-pretty (lambda (x) (+ x 1))
```

## Tips
- Keep demos short (under 2 minutes)
- Pause between actions for readability
- Use clear, deliberate typing
- Avoid corrections/backspacing if possible