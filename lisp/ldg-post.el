;;; ldg-post.el --- Helper code for use with the "ledger" command-line tool

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
;; Utility functions for dealing with postings.

(require 'ldg-regex)

;;; Code:

(defcustom ledger-default-acct-transaction-indent "    "
  "Default indentation for account transactions in an entry."
  :type 'string
  :group 'ledger-post)
(defgroup ledger-post nil
  "Options for controlling how Ledger-mode deals with postings and completion"
  :group 'ledger)

(defcustom ledger-post-auto-adjust-postings t
  "If non-nil, adjust account and amount to columns set below"
  :type 'boolean
  :group 'ledger-post)

(defcustom ledger-post-account-alignment-column 4
  "The column Ledger-mode attempts to align accounts to."
  :type 'integer 
  :group 'ledger-post)

(defcustom ledger-post-amount-alignment-column 52
  "The column Ledger-mode attempts to align amounts to."
  :type 'integer
  :group 'ledger-post)

(defcustom ledger-post-use-completion-engine :built-in
  "Which completion engine to use, :iswitchb or :ido chose those engines,
:built-in uses built-in Ledger-mode completion"
   :type '(radio (const :tag "built in completion" :built-in) 
	   (const :tag "ido completion" :ido) 
	   (const :tag "iswitchb completion" :iswitchb) )
   :group 'ledger-post)

(defun ledger-post-all-accounts ()
  "Return a list of all accounts in the buffer."
  (let ((origin (point))
        (ledger-post-list nil)
        account elements)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ledger-post-line-regexp nil t)
        (unless (and (>= origin (match-beginning 0))
                     (< origin (match-end 0)))
          (add-to-list 'ledger-post-list (ledger-regex-post-line-account))))
      (nreverse ledger-post-list))))

(declare-function iswitchb-read-buffer "iswitchb"
                  (prompt &optional default require-match start matches-set))
(defvar iswitchb-temp-buflist)

(defun ledger-post-completing-read (prompt choices)
  "Use iswitchb as a `completing-read' replacement to choose from choices.
PROMPT is a string to prompt with.  CHOICES is a list of
   strings to choose from."
  (cond
    ((eq ledger-post-use-completion-engine :iswitchb)
     (let* ((iswitchb-use-virtual-buffers nil)
	    (iswitchb-make-buflist-hook
	     (lambda ()
	       (setq iswitchb-temp-buflist choices))))
       (iswitchb-read-buffer prompt)))
    ((eq ledger-post-use-completion-engine :ido)
     (ido-completing-read prompt choices))
    (t
     (completing-read prompt choices))))

(defvar ledger-post-current-list nil)

(defun ledger-post-pick-account ()
  "Insert an account entered by the user."
  (interactive)
  (let* ((account
          (ledger-post-completing-read
           "Account: " (or ledger-post-current-list
                           (setq ledger-post-current-list
                                 (ledger-post-all-accounts)))))
         (account-len (length account))
         (pos (point)))
    (goto-char (line-beginning-position))
    (when (re-search-forward ledger-post-line-regexp (line-end-position) t)
      (let ((existing-len (length (ledger-regex-post-line-account))))
        (goto-char (match-beginning ledger-regex-post-line-group-account))
        (delete-region (match-beginning ledger-regex-post-line-group-account)
                       (match-end ledger-regex-post-line-group-account))
        (insert account)
        (cond
	  ((> existing-len account-len)
	   (insert (make-string (- existing-len account-len) ? )))
	  ((< existing-len account-len)
	   (dotimes (n (- account-len existing-len))
	     (if (looking-at "[ \t]\\( [ \t]\\|\t\\)")
		 (delete-char 1)))))))
    (goto-char pos)))

(defun ledger-next-amount (&optional end)
  "Move point to the next amount, as long as it is not past END."
  (when (re-search-forward "\\(  \\|\t\\| \t\\)[ \t]*-?\\([A-Z$€£]+ *\\)?\\(-?[0-9,]+?\\)\\(.[0-9]+\\)?\\( *[A-Z$€£]+\\)?\\([ \t]*@@?[^\n;]+?\\)?\\([ \t]+;.+?\\|[ \t]*\\)?$" (marker-position end) t)
    (goto-char (match-beginning 0))
    (skip-syntax-forward " ")
    (- (or (match-end 4)
           (match-end 3)) (point))))

(defun ledger-post-align-postings (&optional column)
  "Align amounts and accounts in the current region.
This is done so that the last digit falls in COLUMN, which
defaults to 52.  ledger-post-account-column positions
the account"
  (interactive "p")
  (if (or (null column) (= column 1))
      (setq column ledger-post-amount-alignment-column))
  (save-excursion
    ;; Position the account
    (if (not (or (looking-at "[ \t]*[1-9]")
		 (and (looking-at "[ \t]+\n")
		      (looking-back "[ \n]" (- (point) 2)))))
	(save-excursion
	  (beginning-of-line)
	  (set-mark (point)) 
	  (delete-horizontal-space) 
	  (insert (make-string ledger-post-account-alignment-column ? )))
	(set-mark (point)))
    (set-mark (point))
    (goto-char (1+ (line-end-position)))
    (let* ((mark-first (< (mark) (point)))
           (begin (if mark-first (mark) (point)))
           (end (if mark-first (point-marker) (mark-marker)))
           offset)
      ;; Position the amount
      (goto-char begin)
      (while (setq offset (ledger-next-amount end))
        (let ((col (current-column))
              (target-col (- column offset))
              adjust)
    (setq adjust (- target-col col))
          (if (< col target-col)
              (insert (make-string (- target-col col) ? ))
	      (move-to-column target-col)
	      (if (looking-back "  ")
		  (delete-char (- col target-col))
		  (skip-chars-forward "^ \t")
		  (delete-horizontal-space)
		  (insert "  ")))
          (forward-line))))))

(defun ledger-post-align-posting ()
  "Align the amounts in this posting."
  (interactive)
  (save-excursion
    (set-mark (line-beginning-position))
    (goto-char (1+ (line-end-position)))
    (ledger-post-align-postings)))

(defun ledger-post-maybe-align (beg end len)
  "Align amounts only if point is in a posting.
BEG, END, and LEN control how far it can align."
  (if ledger-post-auto-adjust-postings
      (save-excursion
     (goto-char beg)
     (when (<= end (line-end-position))
       (goto-char (line-beginning-position))
       (if (looking-at ledger-post-line-regexp)
	   (ledger-post-align-postings))))))

(defun ledger-post-edit-amount ()
  "Call 'calc-mode' and push the amount in the posting to the top of stack."
  (interactive)
  (goto-char (line-beginning-position))
  (when (re-search-forward ledger-post-line-regexp (line-end-position) t)
    (goto-char (match-end ledger-regex-post-line-group-account)) ;; go to the and of the account
    (let ((end-of-amount (re-search-forward "[-.,0-9]+" (line-end-position) t)))
      ;; determine if there is an amount to edit
      (if end-of-amount
	  (let ((val (ledger-commodity-string-number-decimalize (match-string 0) :from-user)))
	    (goto-char (match-beginning 0))
	    (delete-region (match-beginning 0) (match-end 0))
	    (calc)
	    (calc-eval val 'push)) ;; edit the amount
	  (progn ;;make sure there are two spaces after the account name and go to calc
	    (if (search-backward "  " (- (point) 3) t)
		(goto-char (line-end-position))
		(insert "  "))
	    (calc))))))

(defun ledger-post-prev-xact ()
  "Move point to the previous transaction."
  (interactive)
  (backward-paragraph)
  (when (re-search-backward ledger-xact-line-regexp nil t)
    (goto-char (match-beginning 0))
    (re-search-forward ledger-post-line-regexp)
    (goto-char (match-end ledger-regex-post-line-group-account))))

(defun ledger-post-next-xact ()
  "Move point to the next transaction."
  (interactive)
  (when (re-search-forward ledger-xact-line-regexp nil t)
    (goto-char (match-beginning 0))
    (re-search-forward ledger-post-line-regexp)
    (goto-char (match-end ledger-regex-post-line-group-account))))

(defun ledger-post-setup ()
  "Configure `ledger-mode' to auto-align postings."
  (add-hook 'after-change-functions 'ledger-post-maybe-align t t)
  (add-hook 'after-save-hook #'(lambda () (setq ledger-post-current-list nil))))


(defun ledger-post-read-account-with-prompt (prompt) 
  (let* ((context (ledger-context-at-point))
	 (default
	  (if (and (eq (ledger-context-line-type context) 'acct-transaction)
		   (eq (ledger-context-current-field context) 'account))
	      (regexp-quote (ledger-context-field-value context 'account))
	      nil)))
    (ledger-read-string-with-default prompt default)))


(provide 'ldg-post)



;;; ldg-post.el ends here
