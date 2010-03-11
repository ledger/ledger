(require 'rx)

(defconst ledger-regex-date
  (let ((sep '(or ?- (any ?. ?/))))	; can't do (any ?- ?. ?/) due to bug
    (rx (group
	 (and (? (= 4 num)
		 (eval sep))
	      (and num (? num))
	      (eval sep)
	      (and num (? num))))))
  "Match a single date, in its 'written' form.")

(defconst ledger-regex-date-group 1)
(defconst ledger-regex-date-group--count 1)

(defconst ledger-regex-full-date
  (macroexpand
   `(rx (and (regexp ,ledger-regex-date)
	     (? (and ?= (regexp ,ledger-regex-date))))))
  "Match a compound date, of the form ACTUAL=EFFECTIVE")

(defconst ledger-regex-full-date-group-actual
  ledger-regex-date-group)
(defconst ledger-regex-full-date-group-effective
  (+ ledger-regex-date-group--count
     ledger-regex-date-group))
(defconst ledger-regex-full-date-group--count
  (* 2 ledger-regex-date-group--count))

(defconst ledger-regex-state
  (rx (group (any ?! ?*))))

(defconst ledger-regex-state-group 1)
(defconst ledger-regex-state-group--count 1)

(defconst ledger-regex-code
  (rx (and ?\( (group (+? (not (any ?\))))) ?\))))

(defconst ledger-regex-code-group 1)
(defconst ledger-regex-code-group--count 1)

(defconst ledger-regex-long-space
  (rx (and (*? space)
	   (or (and ?  (or ?  ?\t)) ?\t))))

(defconst ledger-regex-note
  (rx (group (+ nonl))))

(defconst ledger-regex-note-group 1)
(defconst ledger-regex-note-group--count 1)

(defconst ledger-regex-end-note
  (macroexpand `(rx (and (regexp ,ledger-regex-long-space) ?\;
			 (regexp ,ledger-regex-note)))))

(defconst ledger-regex-end-note-group
  ledger-regex-note-group)
(defconst ledger-regex-end-note-group--count
  ledger-regex-note-group--count)

(defconst ledger-regex-full-note
  (macroexpand `(rx (and line-start (+ space)
			 ?\; (regexp ,ledger-regex-note)))))

(defconst ledger-regex-full-note-group
  ledger-regex-note-group)
(defconst ledger-regex-full-note-group--count
  ledger-regex-note-group--count)

(defconst ledger-regex-xact-line
  (macroexpand
   `(rx (and line-start
	     (regexp ,ledger-regex-full-date)
	     (? (and (+ space) (regexp ,ledger-regex-state)))
	     (? (and (+ space) (regexp ,ledger-regex-code)))
	     (+ space) (+? nonl)
	     (? (regexp ,ledger-regex-end-note))
	     line-end))))

(defconst ledger-regex-xact-line-group-actual-date
  ledger-regex-full-date-group-actual)
(defconst ledger-regex-xact-line-group-effective-date
  ledger-regex-full-date-group-effective)
(defconst ledger-regex-xact-line-group-state
  (+ ledger-regex-full-date-group--count
     ledger-regex-state-group))
(defconst ledger-regex-xact-line-group-code
  (+ ledger-regex-full-date-group--count
     ledger-regex-state-group--count
     ledger-regex-code-group))
(defconst ledger-regex-xact-line-group-note
  (+ ledger-regex-full-date-group--count
     ledger-regex-state-group--count
     ledger-regex-code-group--count
     ledger-regex-note-group))
(defconst ledger-regex-full-note-group--count
  (+ ledger-regex-full-date-group--count
     ledger-regex-state-group--count
     ledger-regex-code-group--count
     ledger-regex-note-group--count))

(defun ledger-regex-xact-line-actual-date
  (&optional string)
  (match-string ledger-regex-xact-line-group-actual-date string))

(defconst ledger-regex-account
  (rx (group (and (not (any ?:)) (*? nonl)))))

(defconst ledger-regex-full-account
  (macroexpand
   `(rx (and (group (? (any ?\[ ?\))))
	     (regexp ,ledger-regex-account)
	     (? (any ?\] ?\)))))))

(defconst ledger-regex-commodity
  (rx (or (and ?\" (+ (not (any ?\"))) ?\")
	  (not (any space ?\n
		    digit
		    ?- ?\[ ?\]
		    ?. ?, ?\; ?+ ?* ?/ ?^ ?? ?: ?& ?| ?! ?=
		    ?\< ?\> ?\{ ?\} ?\( ?\) ?@)))))

(defconst ledger-regex-amount
  (rx (and (? ?-)
	   (and (+ digit)
		(*? (and (any ?. ?,) (+ digit))))
	   (? (and (any ?. ?,) (+ digit))))))

(defconst ledger-regex-commoditized-amount
  (macroexpand
   `(rx (or (and (regexp ,ledger-regex-commodity)
		 (*? space)
		 (regexp ,ledger-regex-amount))
	    (and (regexp ,ledger-regex-amount)
		 (*? space)
		 (regexp ,ledger-regex-commodity))))))

(defconst ledger-regex-commodity-annotations
  (macroexpand
   `(rx (* (+ space)
	   (or (and ?\{ (regexp ,ledger-regex-commoditized-amount) ?\})
	       (and ?\[ (regexp ,ledger-regex-date) ?\])
	       (and ?\( (not (any ?\))) ?\)))))))

(defconst ledger-regex-cost
  (macroexpand
   `(rx (and (or "@" "@@") (+ space)
	     (regexp ,ledger-regex-commoditized-amount)))))

(defconst ledger-regex-balance-assertion
  (macroexpand
   `(rx (and ?= (+ space)
	     (regexp ,ledger-regex-commoditized-amount)))))

(defconst ledger-regex-full-amount
  (macroexpand `(rx (group (+? (not (any ?\;)))))))

(defconst ledger-regex-post-line
  (macroexpand
   `(rx (and line-start
	     (? (and (+ space) (regexp ,ledger-regex-state)))
	     (+ space) (regexp ,ledger-regex-full-account)
	     (+ space) (regexp ,ledger-regex-full-amount)
	     (? (regexp ,ledger-regex-end-note))
	     line-end))))

(provide 'ldg-regex)
