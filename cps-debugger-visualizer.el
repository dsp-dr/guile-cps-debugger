;;; cps-debugger-visualizer.el --- Emacs visualization for Guile CPS Debugger  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 dsp-dr
;; Author: dsp-dr
;; Keywords: languages, tools, scheme, guile, visualization

;;; Commentary:

;; Provides visual debugging capabilities for Guile Scheme in Emacs
;; Integrates with org-mode, Mermaid, and Geiser

;;; Code:

(require 'geiser)
(require 'org)
(require 'ob)

;;;; Customization

(defgroup cps-debugger-viz nil
  "Visualization settings for CPS debugger"
  :group 'cps-debugger)

(defcustom cps-debugger-viz-default-format 'mermaid
  "Default visualization format"
  :type '(choice (const :tag "Mermaid" mermaid)
                 (const :tag "DOT/Graphviz" dot)
                 (const :tag "PlantUML" plantuml)
                 (const :tag "SVG" svg))
  :group 'cps-debugger-viz)

(defcustom cps-debugger-viz-auto-refresh t
  "Automatically refresh visualizations on execution"
  :type 'boolean
  :group 'cps-debugger-viz)

;;;; Org-babel Integration

(defun org-babel-execute:scheme-visual (body params)
  "Execute Scheme code with visualization support.
Supports :visualize parameter to generate diagrams."
  (let* ((visualize (assoc :visualize params))
         (viz-type (or (cdr (assoc :viz-type params)) 'stack))
         (format (or (cdr (assoc :format params)) 'mermaid))
         (session (cdr (assoc :session params))))
    
    ;; Execute the code normally
    (let ((result (org-babel-execute:scheme body params)))
      
      ;; If visualization requested, generate diagram
      (when visualize
        (let ((diagram (cps-debugger-generate-visualization 
                       result viz-type format session)))
          (setq result (concat result "\n\n" diagram))))
      
      result)))

(defun cps-debugger-generate-visualization (result viz-type format session)
  "Generate visualization based on execution result."
  (let ((viz-code
         (pcase viz-type
           ('stack (cps-viz-stack-code))
           ('trace (cps-viz-trace-code))
           ('cps-flow (cps-viz-cps-flow-code))
           ('eval-model (cps-viz-eval-model-code))
           (_ (cps-viz-stack-code)))))
    
    ;; Execute visualization code in Guile session
    (let ((diagram-output
           (geiser-eval-region-and-go 
            (point-min) (point-max) 
            viz-code)))
      
      ;; Format as org-mode block
      (format "#+begin_src %s\n%s\n#+end_src"
              (symbol-name format)
              diagram-output))))

;;;; Visualization Code Generators

(defun cps-viz-stack-code ()
  "Generate code to visualize current stack."
  "(use-modules (cps-debugger visualizer))
   (stack->mermaid)")

(defun cps-viz-trace-code ()
  "Generate code to visualize execution trace."
  "(use-modules (cps-debugger visualizer)
                (system vm trace))
   (call-with-output-string
     (lambda (port)
       (parameterize ((current-output-port port))
         (call-with-trace
           (lambda () %s)
           #:calls? #t))))")

(defun cps-viz-cps-flow-code ()
  "Generate code to visualize CPS flow."
  "(use-modules (cps-debugger visualizer))
   (visualize-cps-flow %s %s)")

(defun cps-viz-eval-model-code ()
  "Generate code to visualize evaluation model."
  "(use-modules (cps-debugger visualizer))
   (eval-model->diagram '%s)")

;;;; Interactive Commands

(defun cps-debugger-visualize-stack ()
  "Visualize current Guile stack as diagram."
  (interactive)
  (let ((viz-buffer (get-buffer-create "*CPS Stack Visualization*")))
    (with-current-buffer viz-buffer
      (erase-buffer)
      (insert "#+TITLE: Stack Visualization\n\n")
      (insert (cps-debugger-get-stack-diagram))
      (org-mode)
      (goto-char (point-min)))
    (display-buffer viz-buffer)))

(defun cps-debugger-get-stack-diagram ()
  "Get stack diagram from current Guile session."
  (let ((result (geiser-eval-sync
                 "(use-modules (cps-debugger visualizer))
                  (stack->mermaid)")))
    (format "#+begin_src mermaid\n%s\n#+end_src\n"
            (geiser-eval-result-str result))))

(defun cps-debugger-visualize-region (start end)
  "Visualize execution of region as CPS flow."
  (interactive "r")
  (let* ((code (buffer-substring-no-properties start end))
         (viz-buffer (get-buffer-create "*CPS Flow Visualization*")))
    (with-current-buffer viz-buffer
      (erase-buffer)
      (insert "#+TITLE: CPS Flow Visualization\n\n")
      (insert "* Original Code\n")
      (insert (format "#+begin_src scheme\n%s\n#+end_src\n\n" code))
      (insert "* CPS Transformation\n")
      (insert (cps-debugger-get-cps-diagram code))
      (insert "\n* Evaluation Model\n")
      (insert (cps-debugger-get-eval-model-diagram code))
      (org-mode)
      (goto-char (point-min)))
    (display-buffer viz-buffer)))

(defun cps-debugger-get-cps-diagram (code)
  "Get CPS flow diagram for code."
  (let ((result (geiser-eval-sync
                 (format "(use-modules (cps-debugger visualizer))
                          (eval-model->diagram '%s)"
                         code))))
    (format "#+begin_src mermaid\n%s\n#+end_src\n"
            (geiser-eval-result-str result))))

(defun cps-debugger-get-eval-model-diagram (code)
  "Get evaluation model diagram for code."
  (let ((result (geiser-eval-sync
                 (format "(use-modules (cps-debugger visualizer))
                          (eval-model->diagram '%s)"
                         code))))
    (format "#+begin_src mermaid\n%s\n#+end_src\n"
            (geiser-eval-result-str result))))

;;;; Live Stack Monitor

(defvar cps-debugger-monitor-timer nil
  "Timer for stack monitoring.")

(defvar cps-debugger-monitor-buffer nil
  "Buffer for stack monitoring.")

(defun cps-debugger-start-monitor ()
  "Start live stack monitoring."
  (interactive)
  (when cps-debugger-monitor-timer
    (cancel-timer cps-debugger-monitor-timer))
  
  (setq cps-debugger-monitor-buffer
        (get-buffer-create "*CPS Stack Monitor*"))
  
  (setq cps-debugger-monitor-timer
        (run-with-timer 0 0.5 'cps-debugger-update-monitor))
  
  (display-buffer cps-debugger-monitor-buffer))

(defun cps-debugger-stop-monitor ()
  "Stop live stack monitoring."
  (interactive)
  (when cps-debugger-monitor-timer
    (cancel-timer cps-debugger-monitor-timer)
    (setq cps-debugger-monitor-timer nil))
  (message "Stack monitoring stopped"))

(defun cps-debugger-update-monitor ()
  "Update stack monitor display."
  (when (buffer-live-p cps-debugger-monitor-buffer)
    (with-current-buffer cps-debugger-monitor-buffer
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        (insert "=== Live Stack Monitor ===\n")
        (insert (format "Updated: %s\n\n" (current-time-string)))
        (insert (cps-debugger-get-current-stack))
        (goto-char pos)))))

(defun cps-debugger-get-current-stack ()
  "Get current stack information."
  (condition-case err
      (let ((result (geiser-eval-sync
                     "(use-modules (system vm debug))
                      (let ((stack (make-stack #t)))
                        (format #f \"Stack depth: ~a~%Frames:~%~{  ~a~%~}\"
                                (stack-length stack)
                                (map (lambda (i)
                                       (let ((frame (stack-ref stack i)))
                                         (format #f \"~a: ~a\"
                                                 i
                                                 (frame-procedure frame))))
                                     (iota (min 10 (stack-length stack))))))")))
        (geiser-eval-result-str result))
    (error (format "Error: %s" err))))

;;;; Trace Visualization

(defun cps-debugger-trace-function (function-name)
  "Trace function and visualize execution."
  (interactive "sFunction to trace: ")
  (let ((trace-output (cps-debugger-capture-trace function-name))
        (viz-buffer (get-buffer-create "*CPS Trace Visualization*")))
    (with-current-buffer viz-buffer
      (erase-buffer)
      (insert "#+TITLE: Trace Visualization\n\n")
      (insert (format "* Function: %s\n\n" function-name))
      (insert "** Trace Output\n")
      (insert (format "#+begin_example\n%s\n#+end_example\n\n" trace-output))
      (insert "** Sequence Diagram\n")
      (insert (cps-debugger-trace-to-diagram trace-output))
      (org-mode)
      (goto-char (point-min)))
    (display-buffer viz-buffer)))

(defun cps-debugger-capture-trace (function-name)
  "Capture trace for function."
  (let ((result (geiser-eval-sync
                 (format "(use-modules (system vm trace))
                          (call-with-output-string
                            (lambda (port)
                              (parameterize ((current-output-port port))
                                (call-with-trace
                                  (lambda () (%s))
                                  #:calls? #t))))"
                         function-name))))
    (geiser-eval-result-str result)))

(defun cps-debugger-trace-to-diagram (trace-output)
  "Convert trace output to diagram."
  (let ((result (geiser-eval-sync
                 (format "(use-modules (cps-debugger visualizer))
                          (trace->mermaid %S)"
                         trace-output))))
    (format "#+begin_src mermaid\n%s\n#+end_src\n"
            (geiser-eval-result-str result))))

;;;; Export Functions

(defun cps-debugger-export-visualization (format)
  "Export current visualization to file."
  (interactive
   (list (completing-read "Export format: "
                         '("svg" "png" "pdf" "html"))))
  (let ((output-file (read-file-name "Output file: ")))
    (pcase format
      ("svg" (cps-debugger-export-svg output-file))
      ("png" (cps-debugger-export-png output-file))
      ("pdf" (cps-debugger-export-pdf output-file))
      ("html" (cps-debugger-export-html output-file)))
    (message "Exported to %s" output-file)))

(defun cps-debugger-export-svg (file)
  "Export to SVG format."
  ;; Implementation would use mermaid-cli or similar
  (message "SVG export not yet implemented"))

(defun cps-debugger-export-png (file)
  "Export to PNG format."
  ;; Implementation would use mermaid-cli or similar
  (message "PNG export not yet implemented"))

(defun cps-debugger-export-pdf (file)
  "Export to PDF format."
  ;; Implementation would use org-export
  (org-latex-export-to-pdf))

(defun cps-debugger-export-html (file)
  "Export to HTML format."
  (org-html-export-to-html))

;;;; Key Bindings

(define-key scheme-mode-map (kbd "C-c v s") 'cps-debugger-visualize-stack)
(define-key scheme-mode-map (kbd "C-c v r") 'cps-debugger-visualize-region)
(define-key scheme-mode-map (kbd "C-c v t") 'cps-debugger-trace-function)
(define-key scheme-mode-map (kbd "C-c v m") 'cps-debugger-start-monitor)
(define-key scheme-mode-map (kbd "C-c v M") 'cps-debugger-stop-monitor)
(define-key scheme-mode-map (kbd "C-c v e") 'cps-debugger-export-visualization)

;;;; Menu

(easy-menu-define cps-debugger-viz-menu scheme-mode-map
  "Menu for CPS Debugger Visualization"
  '("CPS-Viz"
    ["Visualize Stack" cps-debugger-visualize-stack t]
    ["Visualize Region" cps-debugger-visualize-region t]
    ["Trace Function" cps-debugger-trace-function t]
    "---"
    ["Start Monitor" cps-debugger-start-monitor t]
    ["Stop Monitor" cps-debugger-stop-monitor t]
    "---"
    ["Export..." cps-debugger-export-visualization t]))

(provide 'cps-debugger-visualizer)
;;; cps-debugger-visualizer.el ends here