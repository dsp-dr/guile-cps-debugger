;;; cps-debugger.el --- Emacs configuration for Guile CPS Debugger  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 dsp-dr
;; Author: dsp-dr
;; Keywords: languages, tools, scheme, guile
;; Package-Requires: ((emacs "30.1") (geiser "0.28") (geiser-guile "0.28"))

;;; Commentary:

;; Emacs configuration for working with the Guile CPS Debugger
;; 
;; Usage: emacs -q -l cps-debugger.el
;;
;; This will set up Emacs with Geiser for Guile and configure
;; everything needed to demonstrate the CPS debugger.

;;; Code:

;; Bootstrap straight.el for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Basic Emacs settings
(use-package emacs
  :straight nil
  :config
  ;; UI improvements
  (setq inhibit-startup-screen t)
  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  
  ;; Better defaults
  (setq-default indent-tabs-mode nil)
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq ring-bell-function 'ignore)
  
  ;; Show matching parens
  (show-paren-mode 1)
  (electric-pair-mode 1))

;; Theme
(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t))

;; Which-key for command discovery
(use-package which-key
  :config
  (which-key-mode 1))

;; Company for completion
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2))

;; Geiser for Scheme/Guile
(use-package geiser
  :config
  (setq geiser-default-implementation 'guile)
  (setq geiser-guile-binary "guile3")
  (setq geiser-repl-history-filename "~/.emacs.d/geiser-history")
  (setq geiser-repl-query-on-kill-p nil))

(use-package geiser-guile
  :after geiser
  :config
  ;; Add the CPS debugger to load path
  (setq geiser-guile-load-path
        (cons (expand-file-name default-directory) geiser-guile-load-path)))

;; Paredit for structured editing
(use-package paredit
  :hook ((scheme-mode . paredit-mode)
         (geiser-repl-mode . paredit-mode))
  :bind (:map paredit-mode-map
         ("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly)))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook ((scheme-mode . rainbow-delimiters-mode)
         (geiser-repl-mode . rainbow-delimiters-mode)))

;; Helpful keybindings
(use-package general
  :config
  (general-define-key
   :prefix "C-c"
   "g r" 'geiser-mode-switch-to-repl
   "g e" 'geiser-eval-definition
   "g b" 'geiser-eval-buffer
   "g l" 'geiser-load-file
   "g d" 'geiser-doc-symbol-at-point))

;; CPS Debugger specific configuration
(defun cps-debugger-setup ()
  "Set up the CPS debugger environment."
  (interactive)
  (let ((demo-file (expand-file-name "examples/demo-repl.scm" default-directory)))
    (when (file-exists-p demo-file)
      (find-file demo-file))
    (geiser-mode 1)
    (geiser-mode-switch-to-repl 'guile)
    (with-current-buffer (geiser-repl--buffer)
      (insert "(use-modules (cps-debugger) (cps-debugger repl))")
      (geiser-repl-return))))

(defun cps-debugger-demo ()
  "Run the CPS debugger demo."
  (interactive)
  (split-window-right)
  (other-window 1)
  (cps-debugger-setup)
  (message "CPS Debugger ready! Try: ,cps-pretty (lambda (x) (+ x 1))"))

;; Custom modeline
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "  "
                mode-line-position
                "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

;; Startup message
(defun cps-debugger-welcome ()
  "Display welcome message."
  (let ((buf (get-buffer-create "*CPS Debugger Welcome*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== Guile CPS Debugger - Emacs Environment ===\n\n")
      (insert "Quick Start:\n")
      (insert "  C-c C-d    Run CPS debugger demo\n")
      (insert "  C-c g r    Switch to Guile REPL\n")
      (insert "  C-c g e    Evaluate definition\n")
      (insert "  C-c g b    Evaluate buffer\n\n")
      (insert "REPL Commands:\n")
      (insert "  ,cps-debug FORM     Debug CPS compilation\n")
      (insert "  ,cps-step FORM      Step through CPS transformation\n")
      (insert "  ,cps-inspect FORM   Inspect CPS structure\n")
      (insert "  ,cps-pretty FORM    Pretty-print CPS\n\n")
      (insert "Example:\n")
      (insert "  ,cps-pretty (lambda (x) (+ x 1))\n\n")
      (insert "Press C-c C-d to start the demo!")
      (goto-char (point-min))
      (special-mode))
    (switch-to-buffer buf)))

;; Key bindings
(global-set-key (kbd "C-c C-d") 'cps-debugger-demo)

;; Demo utilities for live presentations
(use-package keycast
  :straight (:host github :repo "tarsius/keycast")
  :config
  (setq keycast-mode-line-format "%k%c%r")
  (setq keycast-mode-line-remove-tail-elements nil)
  (setq keycast-mode-line-insert-after 'mode-line-buffer-identification))

(defun cps-demo-start-keycast ()
  "Start keycast for demo - shows keys in mode line."
  (interactive)
  (keycast-mode-line-mode 1)
  (message "Keycast enabled - keys shown in mode line"))

(defun cps-demo-stop-keycast ()
  "Stop keycast display."
  (interactive)
  (keycast-mode-line-mode -1)
  (message "Keycast disabled"))

;; Alternative: show keys in separate buffer
(defun cps-demo-show-keys-buffer ()
  "Show key presses in a separate buffer for demos."
  (interactive)
  (keycast-log-mode 1)
  (let ((buf (get-buffer-create "*keycast*")))
    (with-current-buffer buf
      (setq-local window-size-fixed t))
    (display-buffer buf '(display-buffer-at-bottom
                          (window-height . 5)))))

;; Command palette for demos
(defun cps-demo-command-palette ()
  "Show a command palette with common demo commands."
  (interactive)
  (let ((choice (completing-read
                 "Demo Command: "
                 '("Start Scheme Session (C-c C-v s)"
                   "Execute Code Block (C-c C-c)"
                   "Execute Buffer (C-c C-v b)"
                   "Switch to REPL (C-c C-z)"
                   "Next Code Block (C-c C-v n)"
                   "Previous Code Block (C-c C-v p)"
                   "Show Keys in Mode Line"
                   "Show Keys in Buffer"
                   "Stop Key Display"))))
    (pcase choice
      ("Start Scheme Session (C-c C-v s)" (call-interactively 'org-babel-execute-buffer))
      ("Execute Code Block (C-c C-c)" (org-babel-execute-src-block))
      ("Execute Buffer (C-c C-v b)" (org-babel-execute-buffer))
      ("Switch to REPL (C-c C-z)" (geiser-mode-switch-to-repl))
      ("Next Code Block (C-c C-v n)" (org-babel-next-src-block))
      ("Previous Code Block (C-c C-v p)" (org-babel-previous-src-block))
      ("Show Keys in Mode Line" (cps-demo-start-keycast))
      ("Show Keys in Buffer" (cps-demo-show-keys-buffer))
      ("Stop Key Display" (cps-demo-stop-keycast)))))

;; Highlight current line for demos
(global-hl-line-mode 1)

;; Increase font size for demos
(defun cps-demo-increase-font ()
  "Increase font size for demo visibility."
  (interactive)
  (text-scale-increase 2))

(defun cps-demo-reset-font ()
  "Reset font size."
  (interactive)
  (text-scale-set 0))

;; Demo navigation helpers
(defun cps-demo-next-section ()
  "Jump to next section in Org mode."
  (interactive)
  (org-next-visible-heading 1)
  (recenter-top-bottom))

(defun cps-demo-previous-section ()
  "Jump to previous section in Org mode."
  (interactive)
  (org-previous-visible-heading 1)
  (recenter-top-bottom))

;; Demo key bindings
(global-set-key (kbd "C-c C-d") 'cps-debugger-demo)
(global-set-key (kbd "C-c d p") 'cps-demo-command-palette)
(global-set-key (kbd "C-c d k") 'cps-demo-start-keycast)
(global-set-key (kbd "C-c d K") 'cps-demo-show-keys-buffer)
(global-set-key (kbd "C-c d +") 'cps-demo-increase-font)
(global-set-key (kbd "C-c d 0") 'cps-demo-reset-font)
(global-set-key (kbd "C-c d n") 'cps-demo-next-section)
(global-set-key (kbd "C-c d p") 'cps-demo-previous-section)

;; Show demo commands on startup
(defun cps-demo-help ()
  "Show demo commands in minibuffer."
  (interactive)
  (message "Demo: C-c d k (keycast) | C-c d + (zoom) | C-c C-v s (start) | C-c C-c (exec)"))

;; Auto-start
(add-hook 'emacs-startup-hook 'cps-debugger-welcome)
(add-hook 'org-mode-hook 
          (lambda ()
            (when (string-match "DEMO\\.org" (or (buffer-file-name) ""))
              (cps-demo-help)
              (cps-demo-start-keycast))))

(provide 'cps-debugger)
;;; cps-debugger.el ends here