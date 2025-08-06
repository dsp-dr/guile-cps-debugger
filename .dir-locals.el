;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((indent-tabs-mode . nil)
         (fill-column . 80)))
 (scheme-mode . ((geiser-scheme-implementation . guile)
                 (geiser-guile-binary . "guile3")
                 (indent-tabs-mode . nil)
                 (eval . (progn
                           (when (require 'paredit nil t)
                             (paredit-mode 1))
                           (when (require 'geiser-guile nil t)
                             (setq geiser-guile-binary "guile3"))))
                 ;; Standard Scheme indentation rules
                 (eval . (put 'match 'scheme-indent-function 1))
                 (eval . (put 'when 'scheme-indent-function 1))
                 (eval . (put 'unless 'scheme-indent-function 1))
                 (eval . (put 'with-output-to-port 'scheme-indent-function 1))
                 (eval . (put 'define-record-type 'scheme-indent-function 1))
                 (eval . (put 'define-module 'scheme-indent-function 1))
                 (eval . (put 'define-public 'scheme-indent-function 1))))
 (org-mode . ((org-confirm-babel-evaluate . nil)
              (org-src-preserve-indentation . t)
              (org-babel-default-header-args:scheme . ((:results . "output")
                                                        (:exports . "both")))
              (eval . (progn
                        (org-babel-do-load-languages
                         'org-babel-load-languages
                         '((scheme . t)
                           (emacs-lisp . t)
                           (shell . t)))))))
 (emacs-lisp-mode . ((fill-column . 80)
                     (indent-tabs-mode . nil)
                     (eval . (when (require 'paredit nil t)
                               (paredit-mode 1))))))