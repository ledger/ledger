(require 'ldg-regex)

(defgroup ledger-account nil
  ""
  :group 'ledger)

(defcustom ledger-account-auto-adjust-amounts t
  "If non-nil, ."
  :type 'boolean
  :group 'ledger-account)

(declare-function iswitchb-read-buffer "iswitchb"
                  (prompt &optional default require-match start matches-set))
(defvar iswitchb-temp-buflist)

(defvar ledger-account-current-list nil)

(defun ledger-account-find-all ()
  (let ((origin (point))
	(ledger-account-list nil)
	account-path elements)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      "^[ \t]+\\([*!]\\s-+\\)?[[(]?\\(.+?\\)\\(\t\\|\n\\| [ \t]\\)" nil t)
	(unless (and (>= origin (match-beginning 0))
		     (< origin (match-end 0)))
	  (setq account-path (match-string-no-properties 2))
	  (unless (string-match "\\`\\s-*;" account-path)
	    (add-to-list 'ledger-account-list account-path))))
      (setq ledger-account-current-list
	    (nreverse ledger-account-list)))))

(defun ledger-account-completing-read (prompt choices)
  "Use iswitchb as a completing-read replacement to choose from choices.
PROMPT is a string to prompt with.  CHOICES is a list of strings
to choose from."
  (let* ((iswitchb-use-virtual-buffers nil)
	 (iswitchb-make-buflist-hook
	  (lambda ()
	    (setq iswitchb-temp-buflist choices))))
    (iswitchb-read-buffer prompt)))

(defun ledger-account-select ()
  (interactive)
  (let* ((account
	  (ledger-account-completing-read "Account: "
					  (or ledger-account-current-list
					      (ledger-account-find-all))))
	 (account-len (length account))
	 (pos (point)))
    (goto-char (line-beginning-position))
    (when (re-search-forward ledger-regex-post-line (line-end-position) t)
      (let ((existing-len (length (match-string 3))))
	(goto-char (match-beginning 3))
	(delete-region (match-beginning 3) (match-end 3))
	(insert account)
	(cond
	 ((> existing-len account-len)
	  (insert (make-string (- existing-len account-len) ? )))
	 ((< existing-len account-len)
	  (dotimes (n (- account-len existing-len))
	    (if (looking-at "[ \t]\\( [ \t]\\|\t\\)")
		(delete-char 1)))))))
    (goto-char pos)))

(defun ledger-account-align-amount ()
  (interactive)
  (save-excursion
    (set-mark (line-beginning-position))
    (goto-char (1+ (line-end-position)))
    (ledger-align-amounts)))

(defun ledger-account-maybe-align (beg end len)
  (save-excursion
    (goto-char beg)
    (when (< end (line-end-position))
      (goto-char (line-beginning-position))
      (if (looking-at ledger-regex-post-line)
	  (ledger-account-align-amount)))))

(defun ledger-account-edit-amount ()
  (interactive)
  (goto-char (line-beginning-position))
  (when (re-search-forward ledger-regex-post-line (line-end-position) t)
    (goto-char (match-end 3))
    (when (re-search-forward "[-.,0-9]+" (line-end-position) t)
      (let ((val (match-string 0)))
	(goto-char (match-beginning 0))
	(delete-region (match-beginning 0) (match-end 0))
	(calc)
	(while (string-match "," val)
	  (setq val (replace-match "" nil nil val)))
	(calc-eval val 'push)))))

(defun ledger-account-prev-xact ()
  (interactive)
  (backward-paragraph)
  (when (re-search-backward ledger-regex-xact-line nil t)
    (goto-char (match-beginning 0))
    (re-search-forward ledger-regex-post-line)
    (goto-char (match-end 3))))

(defun ledger-account-next-xact ()
  (interactive)
  (when (re-search-forward ledger-regex-xact-line nil t)
    (goto-char (match-beginning 0))
    (re-search-forward ledger-regex-post-line)
    (goto-char (match-end 3))))

(defun ledger-account-setup ()
  (let ((map (current-local-map)))
    (define-key map [(meta ?p)] 'ledger-account-prev-xact)
    (define-key map [(meta ?n)] 'ledger-account-next-xact)
    (define-key map [(control ?c) (control ?c)] 'ledger-account-select)
    (define-key map [(control ?c) (control ?e)] 'ledger-account-select))
  (if ledger-account-auto-adjust-amounts
      (add-hook 'after-change-functions 'ledger-account-maybe-align t t)))

(add-hook 'ledger-mode-hook 'ledger-account-setup)

(provide 'ldg-account)
