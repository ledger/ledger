(require 'rx)

(eval-when-compile
  (require 'cl))

(defmacro ledger-define-regexp (name regex docs &rest args)
  "Simplify the creation of a Ledger regex and helper functions."
  (let ((defs
          (list
           `(defconst
              ,(intern (concat "ledger-" (symbol-name name) "-regexp"))
              ,(eval regex))))
        (addend 0) last-group)
    (if (null args)
        (progn
          (nconc
           defs
           (list
            `(defconst
               ,(intern
                 (concat "ledger-regex-" (symbol-name name) "-group"))
               1)))
          (nconc
           defs
           (list
            `(defconst
               ,(intern (concat "ledger-regex-" (symbol-name name)
                                "-group--count"))
               1)))
          (nconc
           defs
           (list
            `(defmacro
               ,(intern (concat "ledger-regex-" (symbol-name name)))
               (&optional string)
               ,(format "Return the match string for the %s" name)
               (match-string
                ,(intern (concat "ledger-regex-" (symbol-name name)
                                 "-group"))
                string)))))
      
      (dolist (arg args)
        (let (var grouping target)
          (if (symbolp arg)
              (setq var arg target arg)
            (assert (listp arg))
            (if (= 2 (length arg))
                (setq var (car arg)
                      target (cadr arg))
              (setq var (car arg)
                    grouping (cadr arg)
                    target (caddr arg))))

          (if (and last-group
                   (not (eq last-group (or grouping target))))
              (incf addend
                    (symbol-value
                     (intern-soft (concat "ledger-regex-"
                                          (symbol-name last-group)
                                          "-group--count")))))
          (nconc
           defs
           (list
            `(defconst
               ,(intern (concat "ledger-regex-" (symbol-name name)
                                "-group-" (symbol-name var)))
               ,(+ addend
                   (symbol-value
                    (intern-soft
                     (if grouping
                         (concat "ledger-regex-" (symbol-name grouping)
                                 "-group-" (symbol-name target))
                       (concat "ledger-regex-" (symbol-name target)
                               "-group"))))))))
          (nconc
           defs
           (list
            `(defmacro
               ,(intern (concat "ledger-regex-" (symbol-name name)
                                "-" (symbol-name var)))
               (&optional string)
               ,(format "Return the sub-group match for the %s %s."
                        name var)
               (match-string
                ,(intern (concat "ledger-regex-" (symbol-name name)
                                 "-group-" (symbol-name var)))
                string))))

          (setq last-group (or grouping target))))

      (nconc defs
             (list
              `(defconst ,(intern (concat "ledger-regex-" (symbol-name name)
                                          "-group--count"))
                 ,(length args)))))

    (cons 'progn defs)))

(put 'ledger-define-regexp 'lisp-indent-function 1)

(ledger-define-regexp date
  (let ((sep '(or ?- (any ?. ?/))))     ; can't do (any ?- ?. ?/) due to bug
    (rx (group
         (and (? (= 4 num)
                 (eval sep))
              (and num (? num))
              (eval sep)
              (and num (? num))))))
  "Match a single date, in its 'written' form.")

(ledger-define-regexp full-date
  (macroexpand
   `(rx (and (regexp ,ledger-date-regexp)
             (? (and ?= (regexp ,ledger-date-regexp))))))
  "Match a compound date, of the form ACTUAL=EFFECTIVE"
  (actual date)
  (effective date))

(ledger-define-regexp state
  (rx (group (any ?! ?*)))
  "Match a transaction or posting's \"state\" character.")

(ledger-define-regexp code
  (rx (and ?\( (group (+? (not (any ?\))))) ?\)))
  "Match the transaction code.")

(ledger-define-regexp long-space
  (rx (and (*? blank)
           (or (and ?  (or ?  ?\t)) ?\t)))
  "Match a \"long space\".")

(ledger-define-regexp note
  (rx (group (+ nonl)))
  "")

(ledger-define-regexp end-note
  (macroexpand
   `(rx (and (regexp ,ledger-long-space-regexp) ?\;
             (regexp ,ledger-note-regexp))))
  "")

(ledger-define-regexp full-note
  (macroexpand
   `(rx (and line-start (+ blank)
             ?\; (regexp ,ledger-note-regexp))))
  "")

(ledger-define-regexp xact-line
  (macroexpand
   `(rx (and line-start
             (regexp ,ledger-full-date-regexp)
             (? (and (+ blank) (regexp ,ledger-state-regexp)))
             (? (and (+ blank) (regexp ,ledger-code-regexp)))
             (+ blank) (+? nonl)
             (? (regexp ,ledger-end-note-regexp))
             line-end)))
  "Match a transaction's first line (and optional notes)."
  (actual-date full-date actual)
  (effective-date full-date effective)
  state
  code
  (note end-note))

(ledger-define-regexp account
  (rx (group (and (not (any blank ?\[ ?\( ?: ?\;)) (*? nonl))))
  "")

(ledger-define-regexp account-kind
  (rx (group (? (any ?\[ ?\())))
  "")

(ledger-define-regexp full-account
  (macroexpand
   `(rx (and (regexp ,ledger-account-kind-regexp)
             (regexp ,ledger-account-regexp)
             (? (any ?\] ?\))))))
  ""
  (kind account-kind)
  (name account))

(ledger-define-regexp commodity
  (rx (group
       (or (and ?\" (+ (not (any ?\"))) ?\")
           (not (any blank ?\n
                     digit
                     ?- ?\[ ?\]
                     ?. ?, ?\; ?+ ?* ?/ ?^ ?? ?: ?& ?| ?! ?=
                     ?\< ?\> ?\{ ?\} ?\( ?\) ?@)))))
  "")

(ledger-define-regexp amount
  (rx (group
       (and (? ?-)
            (and (+ digit)
                 (*? (and (any ?. ?,) (+ digit))))
            (? (and (any ?. ?,) (+ digit))))))
  "")

(ledger-define-regexp commoditized-amount
  (macroexpand
   `(rx (group
         (or (and (regexp ,ledger-commodity-regexp)
                  (*? blank)
                  (regexp ,ledger-amount-regexp))
             (and (regexp ,ledger-amount-regexp)
                  (*? blank)
                  (regexp ,ledger-commodity-regexp))))))
  "")

(ledger-define-regexp commodity-annotations
  (macroexpand
   `(rx (* (+ blank)
           (or (and ?\{ (regexp ,ledger-commoditized-amount-regexp) ?\})
               (and ?\[ (regexp ,ledger-date-regexp) ?\])
               (and ?\( (not (any ?\))) ?\))))))
  "")

(ledger-define-regexp cost
  (macroexpand
   `(rx (and (or "@" "@@") (+ blank)
             (regexp ,ledger-commoditized-amount-regexp))))
  "")

(ledger-define-regexp balance-assertion
  (macroexpand
   `(rx (and ?= (+ blank)
             (regexp ,ledger-commoditized-amount-regexp))))
  "")

(ledger-define-regexp full-amount
  (macroexpand `(rx (group (+? (not (any ?\;))))))
  "")

(ledger-define-regexp post-line
  (macroexpand
   `(rx (and line-start (+ blank)
             (? (and (regexp ,ledger-state-regexp) (* blank)))
             (regexp ,ledger-full-account-regexp)
             (? (and (regexp ,ledger-long-space-regexp)
                     (regexp ,ledger-full-amount-regexp)))
             (? (regexp ,ledger-end-note-regexp))
             line-end)))
  ""
  state
  (account-kind full-account kind)
  (account full-account name)
  (amount full-amount)
  (note end-note))

(provide 'ldg-regex)
