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
  "Move point to the next amount, as long as it is not past END.
Return the width of the amount field as an integer."
  (beginning-of-line)
  (when (re-search-forward "\\(  \\|\t\\| \t\\)[ \t]*-?\\([A-Z$€£_]+ *\\)?\\(-?[0-9,]+?\\)\\(.[0-9]+\\)?\\( *[[:word:]€£_\"]+\\)?\\([ \t]*[@={]@?[^\n;]+?\\)?\\([ \t]+;.+?\\|[ \t]*\\)?$" (marker-position end) t)
    (goto-char (match-beginning 0))
    (skip-syntax-forward " ")
    (- (or (match-end 4)
           (match-end 3)) (point))))

(defun ledger-next-account (&optional end)
  "Move point to the beginning of the next account, or status marker (!*), as long as it is not past END.
Return the column of the beginning of the account"
  (beginning-of-line)
  (if (> (marker-position end) (point))
      (when (re-search-forward "\\(^[ 	]+\\)\\([\\[(*!;a-zA-Z0-9]+?\\)" (marker-position end) t)
	(goto-char (match-beginning 2))
	(current-column))))


(defun end-of-line-or-region (end-region)
  "Return a number or marker to the END-REGION or end of line
position, whichever is closer."
  (let ((end (if (< end-region (line-end-position))
		 end-region
		 (line-end-position))))
    (if (markerp end-region)
	(copy-marker end)
	end)))

(defun ledger-post-adjust (adjust-by)
  (if (> adjust-by 0)
      (insert (make-string adjust-by ? ))
      (if (looking-back " " (- (point) 3))
	  (delete-char adjust-by)
	  (skip-chars-forward "^ \t")
	  (delete-horizontal-space)
	  (insert "  "))))

(defun ledger-post-align-postings ()
  "Align all accounts and amounts within region, if there is no
region alight the posting on the current line."
  (interactive)
  (save-excursion
    (let* ((mark-first (< (mark) (point)))
	   (begin-region (if mark-first (mark) (point)))
	   (end-region (if mark-first (point-marker) (mark-marker)))
	   acc-col amt-offset acc-adjust)
      ;; Condition point and mark to the beginning and end of lines
      (goto-char end-region)
      (setq end-region (copy-marker (line-end-position)))
      (goto-char begin-region)
      (setq begin-region (copy-marker (line-beginning-position)))
      (goto-char begin-region)
      (while (or (setq acc-col (ledger-next-account (end-of-line-or-region end-region)))
		 (< (point) (marker-position end-region)))
	(when acc-col 
	    (setq acc-adjust (- ledger-post-account-alignment-column acc-col))
	    (if (/= acc-adjust 0)
		(ledger-post-adjust acc-adjust))
	    
	    (when (setq amt-offset (ledger-next-amount (end-of-line-or-region end-region)))
	      (let* ((amt-adjust (- ledger-post-amount-alignment-column 
				    amt-offset 
				    (current-column))))
		(if (/= amt-adjust 0)
		    (ledger-post-adjust amt-adjust)))))
	(forward-line)))))

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
