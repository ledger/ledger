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

(defun ledger-clear-current ()
  (interactive)
  (save-excursion
    (when (re-search-backward "^[0-9]" nil t)
      (skip-chars-forward "0-9./")
      (insert " *"))))

(define-derived-mode ledger-mode text-mode "Ledger"
  "A mode for editing ledger data files."
  (setq comment-start ";" comment-end nil)
  (let ((map (current-local-map)))
    (define-key map [(control ?c) (control ?n)] 'ledger-add-entry)
    (define-key map [(control ?c) (control ?c)]
  'ledger-clear-current)))

(defun ledger-parse-entries (account)
  (let* ((now (current-time))
	 (current-year (nth 5 (decode-time now)))
	 (then now)
	 entries)
    ;; `then' is 45 days ago
    (setq then (time-subtract then (seconds-to-time (* 45 24 60 60))))
    (while (not (eobp))
      (when (looking-at
	     (concat "\\(Y\\s-+\\([0-9]+\\)\\|\\([0-9]{4}+\\)?[./]?"
		     "\\([0-9]+\\)[./]\\([0-9]+\\)\\s-+"
		     "\\(\\*\\s-+\\)?\\(.+\\)\\)"))
	(let ((found (match-string 2))
	      when)
	  (if found
	      (setq current-year (string-to-number found))
	    (let ((start (match-beginning 0))
		  (year (match-string 3))
		  (month (string-to-number (match-string 4)))
		  (day (string-to-number (match-string 5)))
		  (mark (match-string 6))
		  (desc (match-string 7)))
	      (if (and year (> (length year) 0))
		  (setq year (string-to-number year)))
	      (setq when (encode-time 0 0 0 day month
				      (or year current-year)))
	      (when (or (not mark) (time-less-p then when))
		(forward-line)
		(while (looking-at
			(concat "\\s-+\\([A-Za-z_].+?\\)  \\s-*"
				"\\([^0-9]+\\)\\s-*\\([0-9.]+\\)"))
		  (let ((acct (match-string 1))
			(amt (match-string 3)))
		    (if (string= account acct)
			(setq entries
			      (cons (list (copy-marker start)
					  mark when desc amt)
				    entries))))
		  (forward-line)))))))
      (forward-line))
    (nreverse entries)))

(define-derived-mode ledger-reconcile-mode text-mode "Reconcile"
  "A mode for reconciling ledger entries."
  (let ((map (make-sparse-keymap)))
    (define-key map [space] 'ledger-reconcile-toggle)
    (use-local-map map)))

(defvar ledger-buf nil)
(make-variable-buffer-local 'ledger-buf)

(defun ledger-reconcile-toggle ()
  (interactive)
  (let ((where (get-text-property (point) 'where)))
    (with-current-buffer ledger-buf
      (goto-char where)
      (ledger-clear-current))))

(defun ledger-reconcile (account)
  (interactive "sAccount to reconcile: ")
  (let ((items (save-excursion
		 (goto-char (point-min))
		 (ledger-parse-entries account)))
	(buf (current-buffer)))
    (pop-to-buffer (generate-new-buffer "*Reconcile*"))
    (ledger-reconcile-mode)
    (setq ledger-buf buf)
    (dolist (item items)
      (let ((beg (point)))
	(insert (format "%s %-30s %8.2f\n"
			(format-time-string "%Y.%m.%d" (nth 2 item))
			(nth 3 item)
			(string-to-number (nth 4 item))))
	(if (nth 1 item)
	    (set-text-properties beg (1- (point))
				 (list 'face 'bold
				       'where (nth 0 item))))))))
