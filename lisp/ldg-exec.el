(defgroup ledger-binary nil
  "Interface to the Ledger command-line accounting program."
  :group 'ledger)

(defcustom ledger-binary-path "ledger"
  "Path to the ledger executable."
  :type 'file
  :group 'ledger)

(defvar ledger-delete-after nil)

(defun ledger-run-ledger (buffer &rest args)
  "run ledger with supplied arguments"
  ;; Let's try again, just in case they moved it while we were sleeping.
  (cond
   ((null ledger-binary-path)
    (error "The variable `ledger-binary-path' has not been set"))
   (t
    (let ((buf (current-buffer)))
      (with-current-buffer buffer
	(let ((coding-system-for-write 'utf-8)
	      (coding-system-for-read 'utf-8))
	  (apply #'call-process-region
		 (append (list (point-min) (point-max)
			       ledger-binary-path ledger-delete-after
			       buf nil "-f" "-")
			 args))))))))

(defun ledger-run-ledger-and-delete (buffer &rest args)
  (let ((ledger-delete-after t))
    (apply #'ledger-run-ledger buffer args)))

(provide 'ldg-exec)
