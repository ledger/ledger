(defgroup ledger-exec nil
  "Interface to the Ledger command-line accounting program."
  :group 'ledger)

(defcustom ledger-binary-path "ledger"
  "Path to the ledger executable."
  :type 'file
  :group 'ledger)

(defun ledger-exec-ledger (input-buffer &optional output-buffer &rest args)
  "Run Ledger."
  (if (null ledger-binary-path)
      (error "The variable `ledger-binary-path' has not been set"))
  (let ((buf (or input-buffer (current-buffer)))
        (outbuf (or output-buffer
                    (generate-new-buffer " *ledger-tmp*"))))
    (with-current-buffer buf
      (let ((coding-system-for-write 'utf-8)
            (coding-system-for-read 'utf-8))
        (apply #'call-process-region
               (append (list (point-min) (point-max)
                             ledger-binary-path nil outbuf nil "-f" "-")
                       args)))
      outbuf)))

(defun ledger-exec-read (&optional input-buffer &rest args)
  (with-current-buffer
      (apply #'ledger-exec-ledger input-buffer nil "emacs" args)
    (goto-char (point-min))
    (prog1
        (read (current-buffer))
      (kill-buffer (current-buffer)))))

(provide 'ldg-exec)
