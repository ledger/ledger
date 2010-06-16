;; Reconcile mode

(defvar ledger-buf nil)
(defvar ledger-acct nil)

(defun ledger-display-balance ()
  (let ((buffer ledger-buf)
        (account ledger-acct))
    (with-temp-buffer
      (let ((exit-code (ledger-run-ledger buffer "-C" "balance" account)))
        (if (/= 0 exit-code)
            (message "Error determining cleared balance")
          (goto-char (1- (point-max)))
          (goto-char (line-beginning-position))
          (delete-horizontal-space)
          (message "Cleared balance = %s"
                   (buffer-substring-no-properties (point)
                                                   (line-end-position))))))))

(defun ledger-reconcile-toggle ()
  (interactive)
  (let ((where (get-text-property (point) 'where))
        (account ledger-acct)
        (inhibit-read-only t)
        cleared)
    (when (or (equal (car where) "<stdin>") (equal (car where) "/dev/stdin"))
      (with-current-buffer ledger-buf
          (goto-char (cdr where))
        (setq cleared (ledger-toggle-current 'pending)))
      (if cleared
          (add-text-properties (line-beginning-position)
                               (line-end-position)
                               (list 'face 'bold))
        (remove-text-properties (line-beginning-position)
                                (line-end-position)
                                (list 'face))))
    (forward-line)))

(defun ledger-reconcile-refresh ()
  (interactive)
  (let ((inhibit-read-only t)
        (line (count-lines (point-min) (point))))
    (erase-buffer)
    (ledger-do-reconcile)
    (set-buffer-modified-p t)
    (goto-char (point-min))
    (forward-line line)))

(defun ledger-reconcile-refresh-after-save ()
  (let ((buf (get-buffer "*Reconcile*")))
    (if buf
        (with-current-buffer buf
          (ledger-reconcile-refresh)
          (set-buffer-modified-p nil)))))

(defun ledger-reconcile-add ()
  (interactive)
  (with-current-buffer ledger-buf
    (call-interactively #'ledger-add-entry))
  (ledger-reconcile-refresh))

(defun ledger-reconcile-delete ()
  (interactive)
  (let ((where (get-text-property (point) 'where)))
    (when (or (equal (car where) "<stdin>") (equal (car where) "/dev/stdin"))
      (with-current-buffer ledger-buf
        (goto-char (cdr where))
        (ledger-delete-current-entry))
      (let ((inhibit-read-only t))
        (goto-char (line-beginning-position))
        (delete-region (point) (1+ (line-end-position)))
        (set-buffer-modified-p t)))))

(defun ledger-reconcile-visit ()
  (interactive)
  (let ((where (get-text-property (point) 'where)))
    (when (or (equal (car where) "<stdin>") (equal (car where) "/dev/stdin"))
      (switch-to-buffer-other-window ledger-buf)
      (goto-char (cdr where)))))

(defun ledger-reconcile-save ()
  (interactive)
  (with-current-buffer ledger-buf
    (save-buffer))
  (set-buffer-modified-p nil)
  (ledger-display-balance))

(defun ledger-reconcile-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun ledger-reconcile-finish ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((where (get-text-property (point) 'where))
            (face  (get-text-property (point) 'face)))
        (if (and (eq face 'bold)
                 (or (equal (car where) "<stdin>") (equal (car where) "/dev/stdin")))
            (with-current-buffer ledger-buf
              (goto-char (cdr where))
              (ledger-toggle-current 'cleared))))
      (forward-line 1)))
  (ledger-reconcile-save))

(defun ledger-do-reconcile ()
  )

(defun ledger-reconcile (account)
  (interactive "sAccount to reconcile: ")
  (let ((buf (current-buffer))
        (rbuf (get-buffer "*Reconcile*")))
    (if rbuf
        (kill-buffer rbuf))
    (add-hook 'after-save-hook 'ledger-reconcile-refresh-after-save)
    (with-current-buffer
        (pop-to-buffer (get-buffer-create "*Reconcile*"))
      (ledger-reconcile-mode)
      (set (make-local-variable 'ledger-buf) buf)
      (set (make-local-variable 'ledger-acct) account)
      (ledger-do-reconcile))))

(defvar ledger-reconcile-mode-abbrev-table)

(define-derived-mode ledger-reconcile-mode text-mode "Reconcile"
  "A mode for reconciling ledger entries."
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?m)] 'ledger-reconcile-visit)
    (define-key map [return] 'ledger-reconcile-visit)
    (define-key map [(control ?c) (control ?c)] 'ledger-reconcile-finish)
    (define-key map [(control ?x) (control ?s)] 'ledger-reconcile-save)
    (define-key map [(control ?l)] 'ledger-reconcile-refresh)
    (define-key map [? ] 'ledger-reconcile-toggle)
    (define-key map [?a] 'ledger-reconcile-add)
    (define-key map [?d] 'ledger-reconcile-delete)
    (define-key map [?n] 'next-line)
    (define-key map [?p] 'previous-line)
    (define-key map [?s] 'ledger-reconcile-save)
    (define-key map [?q] 'ledger-reconcile-quit)
    (use-local-map map)))
