;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (tab-width . 2)
  (sentence-end-double-space . t)
  (bug-reference-url-format . "http://bugs.ledger-cli.org/show_bug.cgi?id=%s"))
 (c-mode
  (c-file-style . "ledger")
  (c-style-alist
   ("ledger"
    (indent-tabs-mode)
    (c-basic-offset . 2)
    (c-comment-only-line-offset 0 . 0)
    (c-hanging-braces-alist
     (substatement-open before after)
     (arglist-cont-nonempty))
    (c-offsets-alist
     (statement-block-intro . +)
     (knr-argdecl-intro . 5)
     (substatement-open . 0)
     (substatement-label . 0)
     (label . 0)
     (case-label . 0)
     (statement-case-open . 0)
     (statement-cont . +)
     (arglist-intro . +)
     (arglist-close . +)
     (inline-open . 0)
     (brace-list-open . 0)
     (topmost-intro-cont first c-lineup-topmost-intro-cont c-lineup-gnu-DEFUN-intro-cont))
    (c-special-indent-hook . c-gnu-impose-minimum)
    (c-block-comment-prefix . ""))))
 (emacs-lisp-mode
  (indent-tabs-mode . nil)))

