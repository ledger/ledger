;;; ledger-mode.el --- Helper code for use with the "ledger" command-line tool

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

(require 'ledger-regex)
(require 'esh-util)
(require 'esh-arg)
(require 'ledger-commodities)
(require 'ledger-complete)
(require 'ledger-context)
(require 'ledger-exec)
(require 'ledger-fonts)
(require 'ledger-init)
(require 'ledger-occur)
(require 'ledger-post)
(require 'ledger-reconcile)
(require 'ledger-report)
(require 'ledger-sort)
(require 'ledger-state)
(require 'ledger-test)
(require 'ledger-texi)
(require 'ledger-xact)
(require 'ledger-schedule)

;;; Code:

(defgroup ledger nil
  "Interface to the Ledger command-line accounting program."
  :group 'data)

(defconst ledger-version "3.0"
  "The version of ledger.el currently loaded.")

(defconst ledger-mode-version "3.0.0")

(defun ledger-mode-dump-variable (var)
  (if var
   (insert (format "         %s: %S\n" (symbol-name var) (eval var)))))

(defun ledger-mode-dump-group (group)
  "Dump GROUP customizations to current buffer"
  (let ((members (custom-group-members group nil)))
    (dolist (member members)
      (cond ((eq (cadr member) 'custom-group)
	     (insert (format "Group %s:\n" (symbol-name (car member))))
	     (ledger-mode-dump-group (car member)))
	    ((eq (cadr member) 'custom-variable)
	     (ledger-mode-dump-variable (car member)))))))

(defun ledger-mode-dump-configuration ()
  "Dump all customizations"
  (find-file "ledger-mode-dump")
  (ledger-mode-dump-group 'ledger))


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

(defun ledger-read-account-with-prompt (prompt)
  (let* ((context (ledger-context-at-point))
				 (default (if (and (eq (ledger-context-line-type context) 'acct-transaction)
													 (eq (ledger-context-current-field context) 'account))
											(regexp-quote (ledger-context-field-value context 'account))
											nil)))
    (ledger-read-string-with-default prompt default)))

(defun ledger-read-string-with-default (prompt default)
  "Return user supplied string after PROMPT, or DEFAULT."
  (read-string (concat prompt
											 (if default
													 (concat " (" default "): ")
													 ": "))
							 nil 'ledger-minibuffer-history default))

(defun ledger-display-balance-at-point ()
  "Display the cleared-or-pending balance.
And calculate the target-delta of the account being reconciled."
  (interactive)
  (let* ((account (ledger-read-account-with-prompt "Account balance to show"))
				 (buffer (current-buffer))
				 (balance (with-temp-buffer
										(ledger-exec-ledger buffer (current-buffer) "cleared" account)
										(if (> (buffer-size) 0)
												(buffer-substring-no-properties (point-min) (1- (point-max)))
												(concat account " is empty.")))))
    (when balance
      (message balance))))

(defun ledger-display-ledger-stats ()
  "Display the cleared-or-pending balance.
And calculate the target-delta of the account being reconciled."
  (interactive)
  (let* ((buffer (current-buffer))
				 (balance (with-temp-buffer
										(ledger-exec-ledger buffer (current-buffer) "stats")
										(buffer-substring-no-properties (point-min) (1- (point-max))))))
    (when balance
      (message balance))))

(defun ledger-magic-tab (&optional interactively)
  "Decide what to with with <TAB>.
Can indent, complete or align depending on context."
  (interactive "p")
	(if (= (point) (line-beginning-position))
			(indent-to ledger-post-account-alignment-column)
			(if (and (> (point) 1)
							 (looking-back "\\([^ \t]\\)" 1))
					(ledger-pcomplete interactively)
					(ledger-post-align-postings))))

(defvar ledger-mode-abbrev-table)

(defun ledger-insert-effective-date ()
  (interactive)
  (let ((context (car (ledger-context-at-point)))
				(date-string (format-time-string (cdr (assoc "date-format" ledger-environment-alist)))))
		(cond ((eq 'xact context)
					 (beginning-of-line)
					 (insert date-string "="))
					((eq 'acct-transaction context)
					 (end-of-line)
					 (insert "  ; [=" date-string "]")))))

(defun ledger-mode-remove-extra-lines ()
	(goto-char (point-min))
	(while (re-search-forward "\n\n\\(\n\\)+" nil t)
		(replace-match "\n\n")))

(defun ledger-mode-clean-buffer ()
	"indent, remove multiple linfe feeds and sort the buffer"
	(interactive)
	(ledger-sort-buffer)
	(ledger-post-align-postings (point-min) (point-max))
	(ledger-mode-remove-extra-lines))


;;;###autoload
(define-derived-mode ledger-mode text-mode "Ledger"
	"A mode for editing ledger data files."
	(ledger-check-version)
	(ledger-post-setup)

	(set (make-local-variable 'comment-start) "; ")
	(set (make-local-variable 'comment-end) "")
	(set (make-local-variable 'indent-tabs-mode) nil)

	(if (boundp 'font-lock-defaults)
			(set (make-local-variable 'font-lock-defaults)
					 '(ledger-font-lock-keywords nil t)))
	(setq font-lock-extend-region-functions
				(list #'font-lock-extend-region-wholelines))
	(setq font-lock-multiline nil)

	(set (make-local-variable 'pcomplete-parse-arguments-function)
			 'ledger-parse-arguments)
	(set (make-local-variable 'pcomplete-command-completion-function)
			 'ledger-complete-at-point)
	(set (make-local-variable 'pcomplete-termination-string) "")

	(add-hook 'post-command-hook 'ledger-highlight-xact-under-point nil t)
	(add-hook 'before-revert-hook 'ledger-occur-remove-all-overlays nil t)
	(make-variable-buffer-local 'highlight-overlay)

	(ledger-init-load-init-file)

	(set (make-local-variable 'indent-region-function) 'ledger-post-align-postings)

	(let ((map (current-local-map)))
		(define-key map [(control ?c) (control ?a)] 'ledger-add-transaction)
		(define-key map [(control ?c) (control ?b)] 'ledger-post-edit-amount)
		(define-key map [(control ?c) (control ?c)] 'ledger-toggle-current)
		(define-key map [(control ?c) (control ?d)] 'ledger-delete-current-transaction)
		(define-key map [(control ?c) (control ?e)] 'ledger-toggle-current-transaction)
		(define-key map [(control ?c) (control ?f)] 'ledger-occur)
		(define-key map [(control ?c) (control ?k)] 'ledger-copy-transaction-at-point)
		(define-key map [(control ?c) (control ?m)] 'ledger-set-month)
		(define-key map [(control ?c) (control ?r)] 'ledger-reconcile)
		(define-key map [(control ?c) (control ?s)] 'ledger-sort-region)
		(define-key map [(control ?c) (control ?t)] 'ledger-insert-effective-date)
		(define-key map [(control ?c) (control ?u)] 'ledger-schedule-upcoming)
		(define-key map [(control ?c) (control ?y)] 'ledger-set-year)
		(define-key map [(control ?c) (control ?p)] 'ledger-display-balance-at-point)
		(define-key map [(control ?c) (control ?l)] 'ledger-display-ledger-stats)
		(define-key map [(control ?c) (control ?q)] 'ledger-post-align-xact)

		(define-key map [tab] 'ledger-magic-tab)
		(define-key map [(control tab)] 'ledger-post-align-xact)
		(define-key map [(control ?i)] 'ledger-magic-tab)
		(define-key map [(control ?c) tab] 'ledger-fully-complete-xact)
		(define-key map [(control ?c) (control ?i)] 'ledger-fully-complete-xact)

		(define-key map [(control ?c) (control ?o) (control ?a)] 'ledger-report-redo)
		(define-key map [(control ?c) (control ?o) (control ?e)] 'ledger-report-edit)
		(define-key map [(control ?c) (control ?o) (control ?g)] 'ledger-report-goto)
		(define-key map [(control ?c) (control ?o) (control ?k)] 'ledger-report-kill)
		(define-key map [(control ?c) (control ?o) (control ?r)] 'ledger-report)
		(define-key map [(control ?c) (control ?o) (control ?s)] 'ledger-report-save)

		(define-key map [(meta ?p)] 'ledger-post-prev-xact)
		(define-key map [(meta ?n)] 'ledger-post-next-xact)

		(define-key map [menu-bar] (make-sparse-keymap "ledger-menu"))
		(define-key map [menu-bar ledger-menu] (cons "Ledger" map))

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
		(define-key map [effective-date] '(menu-item "Set effective date" ledger-insert-effective-date))
		(define-key map [sort-end] '(menu-item "Mark Sort End" ledger-sort-insert-end-mark))
		(define-key map [sort-start] '(menu-item "Mark Sort Beginning" ledger-sort-insert-start-mark))
		(define-key map [sort-buff] '(menu-item "Sort Buffer" ledger-sort-buffer))
		(define-key map [sort-reg] '(menu-item "Sort Region" ledger-sort-region :enable mark-active))
		(define-key map [align-xact] '(menu-item "Align Xact" ledger-post-align-xact))
		(define-key map [align-reg] '(menu-item "Align Region" ledger-post-align-postings :enable mark-active))
		(define-key map [clean-buf] '(menu-item "Clean-up Buffer" ledger-mode-clean-buffer))
		(define-key map [sep2] '(menu-item "--"))
		(define-key map [copy-xact] '(menu-item "Copy Trans at Point" ledger-copy-transaction-at-point))
		(define-key map [toggle-post] '(menu-item "Toggle Current Posting" ledger-toggle-current))
		(define-key map [toggle-xact] '(menu-item "Toggle Current Transaction" ledger-toggle-current-transaction))
		(define-key map [sep4] '(menu-item "--"))
		(define-key map [recon-account] '(menu-item "Reconcile Account" ledger-reconcile :enable ledger-works))
		(define-key map [check-balance] '(menu-item "Check Balance" ledger-display-balance-at-point :enable ledger-works))
		(define-key map [sep6] '(menu-item "--"))
		(define-key map [edit-amount] '(menu-item "Calc on Amount" ledger-post-edit-amount))
		(define-key map [sep] '(menu-item "--"))
		(define-key map [delete-xact] '(menu-item "Delete Transaction" ledger-delete-current-transaction))
		(define-key map [cmp-xact] '(menu-item "Complete Transaction" ledger-fully-complete-xact))
		(define-key map [add-xact] '(menu-item "Add Transaction (ledger xact)" ledger-add-transaction :enable ledger-works))
		(define-key map [sep3] '(menu-item "--"))
		(define-key map [stats] '(menu-item "Ledger Statistics" ledger-display-ledger-stats :enable ledger-works))
		(define-key map [fold-buffer] '(menu-item "Narrow to REGEX" ledger-occur))))




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



(provide 'ledger)

;;; ledger-mode.el ends here
