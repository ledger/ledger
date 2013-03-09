;;; ldg-mode.el --- Helper code for use with the "ledger" command-line tool

;; Copyright (C) 2003-2013 John Wiegley (johnw AT gnu DOT org)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.



;;; Commentary:
;; Most of the general ledger-mode code is here.

;;; Code:

(defsubst ledger-current-year ()
  "The default current year for adding transactions."
  (format-time-string "%Y"))
(defsubst ledger-current-month ()
  "The default current month for adding transactions."
  (format-time-string "%m"))

(defvar ledger-year (ledger-current-year)
  "Start a ledger session with the current year, but make it customizable to ease retro-entry.")

(defvar ledger-month (ledger-current-month)
  "Start a ledger session with the current month, but make it customizable to ease retro-entry.")

(defun ledger-remove-overlays ()
  "Remove all overlays from the ledger buffer."
(interactive)
  "remove overlays formthe buffer, used if the buffer is reverted"
   (remove-overlays))

(defvar ledger-mode-abbrev-table)

;;;###autoload
(define-derived-mode ledger-mode text-mode "Ledger"
    "A mode for editing ledger data files."
    (ledger-check-version)
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

    (add-hook 'post-command-hook 'ledger-highlight-xact-under-point nil t)
    (add-hook 'before-revert-hook 'ledger-remove-overlays nil t)
    (make-variable-buffer-local 'highlight-overlay)

    (ledger-init-load-init-file)

    (let ((map (current-local-map)))
      (define-key map [(control ?c) (control ?a)] 'ledger-add-transaction)
      (define-key map [(control ?c) (control ?b)] 'ledger-post-edit-amount)
      (define-key map [(control ?c) (control ?c)] 'ledger-toggle-current)
      (define-key map [(control ?c) (control ?d)] 'ledger-delete-current-transaction)
      (define-key map [(control ?c) (control ?e)] 'ledger-toggle-current-transaction)
      (define-key map [(control ?c) (control ?f)] 'ledger-occur)
      (define-key map [(control ?c) (control ?k)] 'ledger-copy-transaction)
      (define-key map [(control ?c) (control ?m)] 'ledger-set-month)
      (define-key map [(control ?c) (control ?r)] 'ledger-reconcile)
      (define-key map [(control ?c) (control ?s)] 'ledger-sort-region)
      (define-key map [(control ?c) (control ?t)] 'ledger-test-run)
      (define-key map [(control ?c) (control ?y)] 'ledger-set-year)
      (define-key map [tab] 'pcomplete)
      (define-key map [(control ?i)] 'pcomplete)
      (define-key map [(control ?c) tab] 'ledger-fully-complete-entry)
      (define-key map [(control ?c) (control ?i)] 'ledger-fully-complete-entry)
      (define-key map [(control ?c) (control ?o) (control ?r)] 'ledger-report)
      (define-key map [(control ?c) (control ?o) (control ?g)] 'ledger-report-goto)
      (define-key map [(control ?c) (control ?o) (control ?a)] 'ledger-report-redo)
      (define-key map [(control ?c) (control ?o) (control ?s)] 'ledger-report-save)
      (define-key map [(control ?c) (control ?o) (control ?e)] 'ledger-report-edit)
      (define-key map [(control ?c) (control ?o) (control ?k)] 'ledger-report-kill)

      (define-key map [(meta ?p)] 'ledger-post-prev-xact)
      (define-key map [(meta ?n)] 'ledger-post-next-xact)

      (define-key map [menu-bar] (make-sparse-keymap "ldg-menu"))
      (define-key map [menu-bar ldg-menu] (cons "Ledger" map))

      (define-key map [report-kill] '(menu-item "Kill Report" ledger-report-kill :enable ledger-works))
      (define-key map [report-edit] '(menu-item "Edit Report" ledger-report-edit :enable ledger-works))
      (define-key map [report-save] '(menu-item "Save Report" ledger-report-save :enable ledger-works))
      (define-key map [report-rrun] '(menu-item "Re-run Report" ledger-report-redo :enable ledger-works))
      (define-key map [report-goto] '(menu-item "Goto Report" ledger-report-goto :enable ledger-works))
      (define-key map [report-run] '(menu-item "Run Report" ledger-report :enable ledger-works))
      (define-key map [sep5] '(menu-item "--"))
      (define-key map [set-month] '(menu-item "Set Month" ledger-set-month :enable ledger-works))
      (define-key map [set-year] '(menu-item "Set Year" ledger-set-year :enable ledger-works))
      (define-key map [cust] '(menu-item "Customize Ledger Mode"  (lambda ()
								    (interactive)
								    (customize-group 'ledger))))
      (define-key map [sep1] '("--"))
      (define-key map [sort-end] '(menu-item "Mark Sort End" ledger-sort-insert-end-mark))
      (define-key map [sort-start] '(menu-item "Mark Sort Beginning" ledger-sort-insert-start-mark))
      (define-key map [sort-buff] '(menu-item "Sort Buffer" ledger-sort-buffer))
      (define-key map [sort-reg] '(menu-item "Sort Region" ledger-sort-region :enable mark-active))
      (define-key map [sep2] '(menu-item "--"))
      (define-key map [copy-xact] '(menu-item "Copy Trans at Point" ledger-copy-transaction))
      (define-key map [toggle-post] '(menu-item "Toggle Current Posting" ledger-toggle-current))
      (define-key map [toggle-xact] '(menu-item "Toggle Current Transaction" ledger-toggle-current-entry))
      (define-key map [sep4] '(menu-item "--"))
      (define-key map [edit-amount] '(menu-item "Reconcile Account" ledger-reconcile))
      (define-key map [sep6] '(menu-item "--"))
      (define-key map [edit-amount] '(menu-item "Calc on Amount" ledger-post-edit-amount))
      (define-key map [sep] '(menu-item "--"))
      (define-key map [delete-xact] '(menu-item "Delete Entry" ledger-delete-current-transaction))
      (define-key map [cmp-xact] '(menu-item "Complete Transaction" ledger-fully-complete-entry))
      (define-key map [add-xact] '(menu-item "Add Transaction (ledger xact)" ledger-add-transaction :enable ledger-works))
      (define-key map [sep3] '(menu-item "--"))
      (define-key map [reconcile] '(menu-item "Reconcile Account" ledger-reconcile :enable ledger-works))
      (define-key map [reconcile] '(menu-item "Hide Xacts" ledger-occur))))

(defun ledger-time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
           (< (nth 1 t1) (nth 1 t2)))))

(defun ledger-time-subtract (t1 t2)
  "Subtract two time values, T1 - T2.
Return the difference in the format of a time value."
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
          (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))

(defun ledger-find-slot (moment)
  "Find the right place in the buffer for a transaction at MOMENT.
MOMENT is an encoded date"
  (catch 'found
    (ledger-iterate-transactions
     (function
      (lambda (start date mark desc)
       (if (ledger-time-less-p moment date)
	   (throw 'found t)))))))

(defun ledger-iterate-transactions (callback)
  "Iterate through each transaction call CALLBACK for each."
  (goto-char (point-min))
  (let* ((now (current-time))
         (current-year (nth 5 (decode-time now))))
    (while (not (eobp))
      (when (looking-at
             (concat "\\(Y\\s-+\\([0-9]+\\)\\|"
                     "\\([0-9]\\{4\\}+\\)?[./-]?"
                     "\\([0-9]+\\)[./-]\\([0-9]+\\)\\s-+"
                     "\\(\\*\\s-+\\)?\\(.+\\)\\)"))
        (let ((found (match-string 2)))
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
		(funcall callback start
			 (encode-time 0 0 0 day month
				      (or year current-year))
			 mark desc)))))
      (forward-line))))

(defun ledger-set-year (newyear)
  "Set ledger's idea of the current year to the prefix argument NEWYEAR."
  (interactive "p")
  (if (= newyear 1)
      (setq ledger-year (read-string "Year: " (ledger-current-year)))
      (setq ledger-year (number-to-string newyear))))

(defun ledger-set-month (newmonth)
  "Set ledger's idea of the current month to the prefix argument NEWMONTH."
  (interactive "p")
  (if (= newmonth 1)
      (setq ledger-month (read-string "Month: " (ledger-current-month)))
      (setq ledger-month (format "%02d" newmonth))))

(defun ledger-add-transaction (transaction-text &optional insert-at-point)
  "Use ledger xact TRANSACTION-TEXT to add a transaction to the buffer.
If INSERT-AT-POINT is non-nil insert the transaction
there, otherwise call `ledger-find-slot' to insert it at the
correct chronological place in the buffer."
  (interactive (list
		(read-string "Transaction: " (concat ledger-year "/" ledger-month "/"))))
  (let* ((args (with-temp-buffer
                 (insert transaction-text)
                 (eshell-parse-arguments (point-min) (point-max))))
         (ledger-buf (current-buffer))
         exit-code)
    (unless insert-at-point
      (let ((date (car args)))
        (if (string-match "\\([0-9]+\\)[-/]\\([0-9]+\\)[-/]\\([0-9]+\\)" date)
            (setq date
                  (encode-time 0 0 0 (string-to-number (match-string 3 date))
                               (string-to-number (match-string 2 date))
                               (string-to-number (match-string 1 date)))))
        (ledger-find-slot date)))
    (if (> (length args) 1)
	(save-excursion
	  (insert
	   (with-temp-buffer
	     (setq exit-code
		   (apply #'ledger-exec-ledger ledger-buf ledger-buf "xact"
			  (mapcar 'eval args)))
	     (goto-char (point-min))
	     (if (looking-at "Error: ")
		 (error (concat "Error in ledger-add-transaction: " (buffer-string)))
		 (buffer-string)))
	   "\n"))
	(progn
	  (insert (car args) " \n\n")
	  (end-of-line -1)))))

(defun ledger-current-transaction-bounds ()
  "Return markers for the beginning and end of transaction surrounding point."
  (save-excursion
    (when (or (looking-at "^[0-9]")
              (re-search-backward "^[0-9]" nil t))
      (let ((beg (point)))
        (while (not (eolp))
          (forward-line))
        (cons (copy-marker beg) (point-marker))))))

(defun ledger-delete-current-transaction ()
  "Delete the transaction surrounging point."
  (interactive)
  (let ((bounds (ledger-current-transaction-bounds)))
    (delete-region (car bounds) (cdr bounds))))

(provide 'ldg-mode)

;;; ldg-mode.el ends here
