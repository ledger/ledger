(defcustom ledger-default-acct-transaction-indent "    "
  "Default indentation for account transactions in an entry."
  :type 'string
  :group 'ledger)

(defvar bold 'bold)
(defvar ledger-font-lock-keywords
  '(("\\(       \\|  \\|^\\)\\(;.*\\)" 2 font-lock-comment-face)
    ("^[0-9]+[-/.=][-/.=0-9]+\\s-+\\(([^)]+)\\s-+\\)?\\([^*].+?\\)\\(\\(        ;\\|  ;\\|$\\)\\)" 2 bold)
    ;;("^[0-9]+[-/.=][-/.=0-9]+\\s-+\\(([^)]+)\\s-+\\)?\\([*].+?\\)\\(\\(       ;\\|  ;\\|$\\)\\)"
    ;; 2 font-lock-type-face)
    ("^\\s-+\\([*]\\s-*\\)?\\(\\([[(]\\)?[^*:
        ]+?:\\([^]);
        ]\\|\\s-\\)+?\\([])]\\)?\\)\\(    \\|  \\|$\\)"
     2 font-lock-keyword-face)
    ("^\\([~=].+\\)" 1 font-lock-function-name-face)
    ("^\\([A-Za-z]+ .+\\)" 1 font-lock-function-name-face))
  "Expressions to highlight in Ledger mode.")

(defvar ledger-mode-abbrev-table)

;;;###autoload
(define-derived-mode ledger-mode text-mode "Ledger"
  "A mode for editing ledger data files."
  (ledger-post-setup)

  (set (make-local-variable 'comment-start) " ; ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'indent-tabs-mode) nil)

  (if (boundp 'font-lock-defaults)
      (set (make-local-variable 'font-lock-defaults)
           '(ledger-font-lock-keywords nil t)))

  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'ledger-parse-arguments)
  (set (make-local-variable 'pcomplete-command-completion-function)
       'ledger-complete-at-point)
  (set (make-local-variable 'pcomplete-termination-string) "")

  (let ((map (current-local-map)))
    (define-key map [(control ?c) (control ?a)] 'ledger-add-entry)
    (define-key map [(control ?c) (control ?d)] 'ledger-delete-current-entry)
    (define-key map [(control ?c) (control ?y)] 'ledger-set-year)
    (define-key map [(control ?c) (control ?m)] 'ledger-set-month)
    (define-key map [(control ?c) (control ?c)] 'ledger-toggle-current)
    (define-key map [(control ?c) (control ?e)] 'ledger-toggle-current-entry)
    (define-key map [(control ?c) (control ?r)] 'ledger-reconcile)
    (define-key map [(control ?c) (control ?s)] 'ledger-sort)
    (define-key map [(control ?c) (control ?t)] 'ledger-test-run)
    (define-key map [tab] 'pcomplete)
    (define-key map [(control ?i)] 'pcomplete)
    (define-key map [(control ?c) tab] 'ledger-fully-complete-entry)
    (define-key map [(control ?c) (control ?i)] 'ledger-fully-complete-entry))

  (ledger-report-patch-reports (current-buffer)))

(defun ledger-time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
           (< (nth 1 t1) (nth 1 t2)))))

(defun ledger-time-subtract (t1 t2)
  "Subtract two time values.
Return the difference in the format of a time value."
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
          (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))

(defun ledger-find-slot (moment)
  (catch 'found
    (ledger-iterate-entries
     (function
      (lambda (start date mark desc)
        (if (ledger-time-less-p moment date)
            (throw 'found t)))))))

(defun ledger-add-entry (entry-text &optional insert-at-point)
  (interactive "sEntry: ")
  (let* ((args (with-temp-buffer
                 (insert entry-text)
                 (eshell-parse-arguments (point-min) (point-max))))
         (ledger-buf (current-buffer))
         exit-code)
    (unless insert-at-point
      (let ((date (car args)))
        (if (string-match "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)" date)
            (setq date
                  (encode-time 0 0 0 (string-to-number (match-string 3 date))
                               (string-to-number (match-string 2 date))
                               (string-to-number (match-string 1 date)))))
        (ledger-find-slot date)))
    (save-excursion
      (insert
       (with-temp-buffer
         (setq exit-code
               (apply #'ledger-run-ledger ledger-buf "entry"
                      (mapcar 'eval args)))
         (goto-char (point-min))
         (if (looking-at "Error: ")
             (error (buffer-string))
           (buffer-string)))
       "\n"))))

(defun ledger-current-entry-bounds ()
  (save-excursion
    (when (or (looking-at "^[0-9]")
              (re-search-backward "^[0-9]" nil t))
      (let ((beg (point)))
        (while (not (eolp))
          (forward-line))
        (cons (copy-marker beg) (point-marker))))))

(defun ledger-delete-current-entry ()
  (interactive)
  (let ((bounds (ledger-current-entry-bounds)))
    (delete-region (car bounds) (cdr bounds))))

(provide 'ldg-mode)
