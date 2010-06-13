(defcustom ledger-clear-whole-entries nil
  "If non-nil, clear whole entries, not individual transactions."
  :type 'boolean
  :group 'ledger)

(defun ledger-toggle-state (state &optional style)
  (if (not (null state))
      (if (and style (eq style 'cleared))
          'cleared)
    (if (and style (eq style 'pending))
        'pending
      'cleared)))

(defun ledger-entry-state ()
  (save-excursion
    (when (or (looking-at "^[0-9]")
              (re-search-backward "^[0-9]" nil t))
      (skip-chars-forward "0-9./=")
      (skip-syntax-forward " ")
      (cond ((looking-at "!\\s-*") 'pending)
            ((looking-at "\\*\\s-*") 'cleared)
            (t nil)))))

(defun ledger-transaction-state ()
  (save-excursion
    (goto-char (line-beginning-position))
    (skip-syntax-forward " ")
    (cond ((looking-at "!\\s-*") 'pending)
          ((looking-at "\\*\\s-*") 'cleared)
          (t (ledger-entry-state)))))

(defun ledger-toggle-current-transaction (&optional style)
  "Toggle the cleared status of the transaction under point.
Optional argument STYLE may be `pending' or `cleared', depending
on which type of status the caller wishes to indicate (default is
`cleared').
This function is rather complicated because it must preserve both
the overall formatting of the ledger entry, as well as ensuring
that the most minimal display format is used.  This could be
achieved more certainly by passing the entry to ledger for
formatting, but doing so causes inline math expressions to be
dropped."
  (interactive)
  (let ((bounds (ledger-current-entry-bounds))
        clear cleared)
    ;; Uncompact the entry, to make it easier to toggle the
    ;; transaction
    (save-excursion
      (goto-char (car bounds))
      (skip-chars-forward "0-9./= \t")
      (setq cleared (and (member (char-after) '(?\* ?\!))
                         (char-after)))
      (when cleared
        (let ((here (point)))
          (skip-chars-forward "*! ")
          (let ((width (- (point) here)))
            (when (> width 0)
              (delete-region here (point))
              (if (search-forward "  " (line-end-position) t)
                  (insert (make-string width ? ))))))
        (forward-line)
        (while (looking-at "[ \t]")
          (skip-chars-forward " \t")
          (insert cleared " ")
          (if (search-forward "  " (line-end-position) t)
              (delete-char 2))
          (forward-line))))
    ;; Toggle the individual transaction
    (save-excursion
      (goto-char (line-beginning-position))
      (when (looking-at "[ \t]")
        (skip-chars-forward " \t")
        (let ((here (point))
              (cleared (member (char-after) '(?\* ?\!))))
          (skip-chars-forward "*! ")
          (let ((width (- (point) here)))
            (when (> width 0)
              (delete-region here (point))
              (save-excursion
                (if (search-forward "  " (line-end-position) t)
                    (insert (make-string width ? ))))))
          (let (inserted)
            (if cleared
                (if (and style (eq style 'cleared))
                    (progn
                      (insert "* ")
                      (setq inserted t)))
              (if (and style (eq style 'pending))
                  (progn
                    (insert "! ")
                    (setq inserted t))
                (progn
                  (insert "* ")
                  (setq inserted t))))
            (if (and inserted
                     (re-search-forward "\\(\t\\| [ \t]\\)"
                                        (line-end-position) t))
                (cond
                 ((looking-at "\t")
                  (delete-char 1))
                 ((looking-at " [ \t]")
                  (delete-char 2))
                 ((looking-at " ")
                  (delete-char 1))))
            (setq clear inserted)))))
    ;; Clean up the entry so that it displays minimally
    (save-excursion
      (goto-char (car bounds))
      (forward-line)
      (let ((first t)
            (state ? )
            (hetero nil))
        (while (and (not hetero) (looking-at "[ \t]"))
          (skip-chars-forward " \t")
          (let ((cleared (if (member (char-after) '(?\* ?\!))
                             (char-after)
                           ? )))
            (if first
                (setq state cleared
                      first nil)
              (if (/= state cleared)
                  (setq hetero t))))
          (forward-line))
        (when (and (not hetero) (/= state ? ))
          (goto-char (car bounds))
          (forward-line)
          (while (looking-at "[ \t]")
            (skip-chars-forward " \t")
            (let ((here (point)))
              (skip-chars-forward "*! ")
              (let ((width (- (point) here)))
                (when (> width 0)
                  (delete-region here (point))
                  (if (re-search-forward "\\(\t\\| [ \t]\\)"
                                         (line-end-position) t)
                      (insert (make-string width ? ))))))
            (forward-line))
          (goto-char (car bounds))
          (skip-chars-forward "0-9./= \t")
          (insert state " ")
          (if (re-search-forward "\\(\t\\| [ \t]\\)"
                                 (line-end-position) t)
              (cond
               ((looking-at "\t")
                (delete-char 1))
               ((looking-at " [ \t]")
                (delete-char 2))
               ((looking-at " ")
                (delete-char 1)))))))
    clear))

(defun ledger-toggle-current (&optional style)
  (interactive)
  (if (or ledger-clear-whole-entries
          (eq 'entry (ledger-thing-at-point)))
      (progn
        (save-excursion
          (forward-line)
          (goto-char (line-beginning-position))
          (while (and (not (eolp))
                      (save-excursion
                        (not (eq 'entry (ledger-thing-at-point)))))
            (if (looking-at "\\s-+[*!]")
                (ledger-toggle-current-transaction nil))
            (forward-line)
            (goto-char (line-beginning-position))))
        (ledger-toggle-current-entry style))
    (ledger-toggle-current-transaction style)))

(defun ledger-toggle-current-entry (&optional style)
  (interactive)
  (let (clear)
    (save-excursion
      (when (or (looking-at "^[0-9]")
                (re-search-backward "^[0-9]" nil t))
        (skip-chars-forward "0-9./=")
        (delete-horizontal-space)
        (if (member (char-after) '(?\* ?\!))
            (progn
              (delete-char 1)
              (if (and style (eq style 'cleared))
                  (insert " *")))
          (if (and style (eq style 'pending))
              (insert " ! ")
            (insert " * "))
          (setq clear t))))
    clear))

(provide 'ldg-state)
