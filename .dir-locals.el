;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((scheme-mode
  (indent-tabs-mode . nil)
  (eval . (put 'match 'scheme-indent-function 1))
  (eval . (put 'when 'scheme-indent-function 1))
  (eval . (put 'unless 'scheme-indent-function 1))
  (eval . (put 'with-output-to-port 'scheme-indent-function 1))
  (eval . (put 'define-record-type 'scheme-indent-function 1))
  (eval . (put 'define-module 'scheme-indent-function 1))
  (eval . (put 'define-public 'scheme-indent-function 1))))