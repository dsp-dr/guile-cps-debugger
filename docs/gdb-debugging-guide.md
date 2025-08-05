# GDB Debugging Guide for Guile CPS Code

This guide covers using GDB (GNU Debugger) to debug Guile Scheme programs at the C level, particularly useful for understanding CPS transformations and VM internals.

## Prerequisites

- GDB installed with Guile support
- Guile built with debugging symbols (`--enable-debug` flag)
- Understanding of basic GDB commands

## Setting Up GDB for Guile

### 1. Enable Guile GDB Extensions

Guile provides GDB extensions that make debugging Scheme code easier. These are typically installed with Guile.

```bash
# Check if Guile GDB support is available
gdb --batch --quiet -ex 'guile (display "Guile support available\n")' -ex quit
```

### 2. Configure Auto-loading Safe Path

GDB has security features that prevent automatic loading of Python/Guile scripts. You need to configure the safe path:

```bash
# Add to ~/.gdbinit or system gdbinit
add-auto-load-safe-path /usr/local/share/guile/
add-auto-load-safe-path ~/.guix-profile/share/guile/

# Or disable security check (not recommended for production)
set auto-load safe-path /
```

For more details, see: https://sourceware.org/gdb/current/onlinedocs/gdb.html/Auto_002dloading-safe-path.html

### 3. Load Guile Pretty-Printers

```gdb
# In GDB session
source /usr/local/share/guile/3.0/guile-gdb.scm

# Or add to ~/.gdbinit
python
import sys
sys.path.insert(0, '/usr/local/share/guile/3.0')
end
```

## Basic GDB Commands for Guile

### Starting a Debug Session

```bash
# Debug a Guile script
gdb guile
(gdb) run -l script.scm

# Attach to running Guile process
gdb -p $(pgrep guile)
```

### Useful Breakpoints

```gdb
# Break on Scheme function entry
(gdb) break scm_call_0
(gdb) break scm_apply_0

# Break on error handling
(gdb) break scm_error
(gdb) break scm_throw

# Break on CPS-related functions
(gdb) break scm_i_call_with_continuation
(gdb) break scm_make_continuation
```

## Examining Scheme Objects in GDB

### Using Guile's GDB Support

```gdb
# Print Scheme value
(gdb) gscm-print $1

# Display Scheme object type
(gdb) gscm-type $1

# Pretty-print Scheme list
(gdb) gscm-list $1

# Show procedure information
(gdb) gscm-procedure $1
```

### Manual Inspection

```gdb
# Examine SCM object
(gdb) p/x my_scheme_var
(gdb) x/2gx my_scheme_var

# Check if it's a pair
(gdb) p SCM_IMP(my_scheme_var)

# Get car and cdr
(gdb) p scm_car(my_scheme_var)
(gdb) p scm_cdr(my_scheme_var)
```

## Debugging CPS Code

### 1. Tracing CPS Transformations

```scheme
;; In your Scheme code, add debugging hooks
(use-modules (system vm trace))

(define (debug-cps-transform proc)
  (let ((compiled (compile proc #:to 'cps)))
    (format #t "CPS form: ~s\n" compiled)
    compiled))
```

```gdb
# In GDB, break on CPS compilation
(gdb) break compile_cps
(gdb) commands
> gscm-print $arg1
> continue
> end
```

### 2. Examining Continuations

```gdb
# Break when continuations are created
(gdb) break scm_i_make_continuation
(gdb) commands
> printf "Creating continuation\n"
> backtrace 5
> gscm-print $rdi
> continue
> end
```

### 3. Stack Frame Analysis

```gdb
# Examine Guile VM stack
(gdb) define show-vm-stack
>   set $vm = scm_the_vm()
>   set $sp = $vm->sp
>   set $fp = $vm->fp
>   printf "Stack pointer: %p\n", $sp
>   printf "Frame pointer: %p\n", $fp
>   x/10gx $sp
> end

(gdb) show-vm-stack
```

## Advanced Debugging Techniques

### 1. Conditional Breakpoints for Specific Procedures

```gdb
# Break only when calling specific Scheme procedure
(gdb) break scm_call_1 if strcmp(scm_procedure_name($rdi), "factorial-cps") == 0
```

### 2. Tracing Memory Allocation

```gdb
# Monitor heap allocation
(gdb) break scm_gc_malloc
(gdb) commands
> silent
> printf "Allocating %lu bytes\n", $rsi
> backtrace 1
> continue
> end
```

### 3. VM Instruction Tracing

```gdb
# Trace VM instructions
(gdb) break vm_engine
(gdb) commands
> x/i $pc
> stepi
> end
```

## Debugging Example: CPS Factorial

```scheme
;; factorial-cps.scm
(define (factorial-cps n k)
  (if (= n 0)
      (k 1)
      (factorial-cps (- n 1)
                     (lambda (result)
                       (k (* n result))))))

(define (identity x) x)
(factorial-cps 5 identity)
```

```bash
# Start debugging session
gdb guile
```

```gdb
# Set up debugging
(gdb) set pagination off
(gdb) set print pretty on

# Load the script with breakpoint
(gdb) break factorial-cps
(gdb) run -l factorial-cps.scm

# When breakpoint hits
(gdb) gscm-print $1
(gdb) info locals
(gdb) backtrace

# Step through CPS transformations
(gdb) step
(gdb) gscm-print k
(gdb) continue
```

## Common Issues and Solutions

### Issue: Auto-loading Refused

```
warning: File "/usr/local/lib/libguile-3.0.so.1.4.0-gdb.scm" auto-loading has been declined
```

**Solution:**
```bash
# Add to ~/.gdbinit
set auto-load safe-path /usr/local/lib:/usr/lib
```

### Issue: Pretty-printers Not Working

**Solution:**
```gdb
(gdb) info pretty-printer
(gdb) enable pretty-printer global Guile
```

### Issue: Can't Find Guile Symbols

**Solution:**
```bash
# Ensure debug symbols are installed
sudo apt-get install guile-3.0-dbg  # Debian/Ubuntu
sudo dnf debuginfo-install guile3   # Fedora
```

## GDB Python Scripts for Guile

Create custom GDB commands for Guile debugging:

```python
# ~/.gdbinit or separate file
python
class GuileBacktrace(gdb.Command):
    """Print Guile scheme backtrace"""
    
    def __init__(self):
        super(GuileBacktrace, self).__init__("guile-bt", gdb.COMMAND_USER)
    
    def invoke(self, arg, from_tty):
        gdb.execute("call scm_backtrace()")

GuileBacktrace()
end
```

## References

- [GNU Guile GDB Support Documentation](https://www.gnu.org/software/guile/manual/html_node/GDB-Support.html)
- [GDB Auto-loading Documentation](https://sourceware.org/gdb/current/onlinedocs/gdb.html/Auto_002dloading-safe-path.html)
- [GDB Python API](https://sourceware.org/gdb/current/onlinedocs/gdb.html/Python-API.html)
- [Guile VM Internals](https://www.gnu.org/software/guile/manual/html_node/VM-Concepts.html)

## Quick Reference Card

```gdb
# Essential GDB Commands for Guile
gscm-print <var>           # Print Scheme value
gscm-type <var>            # Show type
gscm-list <var>            # Pretty-print list
info threads               # Show all threads
thread apply all bt        # Backtrace all threads
call scm_backtrace()       # Scheme backtrace
call scm_display(<var>, scm_current_output_port())  # Display value
set print pretty on        # Better formatting
set pagination off         # Don't pause output
```

## Tips for CPS Debugging

1. **Use VM hooks in Scheme first** - Often easier than GDB for high-level debugging
2. **Combine with Guile's built-in debugging** - `,trace` and `,profile` commands
3. **Set breakpoints strategically** - Focus on continuation creation and application
4. **Use conditional breakpoints** - Filter for specific procedures or conditions
5. **Monitor memory** - CPS can create many closures; watch heap usage
6. **Trace VM instructions** - Understand how CPS code executes at VM level