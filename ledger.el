(defun ledger-add-entry (entry)
  (interactive
   (list (read-string "Entry: "
		      (format-time-string "%m.%d " (current-time)))))
  (let ((args (mapcar 'shell-quote-argument (split-string entry))))
    (shell-command
     (concat "ledger entry "
	     (mapconcat 'identity args " ")) t)
    (delete-char 5)
    (exchange-point-and-mark)))

(define-derived-mode ledger-mode text-mode "Ledger"
  "A mode for editing ledger data files."
  (setq comment-start ";" comment-end nil)
  (let ((map (current-local-map)))
    (define-key map [(control ?c) ?n] 'ledger-add-entry)))
