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

(require 'ldg-regex)

(defgroup ledger-post nil
  ""
  :group 'ledger)

(defcustom ledger-post-auto-adjust-amounts nil
  "If non-nil, ."
  :type 'boolean
  :group 'ledger-post)

(defcustom ledger-post-amount-alignment-column 52
  "If non-nil, ."
  :type 'integer
  :group 'ledger-post)

(defcustom ledger-post-use-iswitchb nil
  "If non-nil, ."
  :type 'boolean
  :group 'ledger-post)

(defcustom ledger-post-use-ido nil
  "If non-nil, ."
  :type 'boolean
  :group 'ledger-post)

(defun ledger-post-all-accounts ()
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
  "Use iswitchb as a completing-read replacement to choose from choices.
PROMPT is a string to prompt with.  CHOICES is a list of strings
to choose from."
  (cond
    (ledger-post-use-iswitchb
     (let* ((iswitchb-use-virtual-buffers nil)
	    (iswitchb-make-buflist-hook
	     (lambda ()
	       (setq iswitchb-temp-buflist choices))))
       (iswitchb-read-buffer prompt)))
    (ledger-post-use-ido
     (ido-completing-read prompt choices))
    (t
     (completing-read prompt choices))))

(defvar ledger-post-current-list nil)

(defun ledger-post-pick-account ()
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
  (when (re-search-forward "\\(  \\|\t\\| \t\\)[ \t]*-?\\([A-Z$]+ *\\)?\\(-?[0-9,]+?\\)\\(.[0-9]+\\)?\\( *[A-Z$]+\\)?\\([ \t]*@@?[^\n;]+?\\)?\\([ \t]+;.+?\\)?$" (marker-position end) t)
    (goto-char (match-beginning 0))
    (skip-syntax-forward " ")
    (- (or (match-end 4)
           (match-end 3)) (point))))

(defun ledger-align-amounts (&optional column)
  "Align amounts in the current region.
This is done so that the last digit falls in COLUMN, which defaults to 52."
  (interactive "p")
  (if (or (null column) (= column 1))
      (setq column ledger-post-amount-alignment-column))
  (save-excursion
    (let* ((mark-first (< (mark) (point)))
           (begin (if mark-first (mark) (point)))
           (end (if mark-first (point-marker) (mark-marker)))
           offset)
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

(defun ledger-post-align-amount ()
  (interactive)
  (save-excursion
    (set-mark (line-beginning-position))
    (goto-char (1+ (line-end-position)))
    (ledger-align-amounts)))

(defun ledger-post-maybe-align (beg end len)
  (save-excursion
    (goto-char beg)
    (when (< end (line-end-position))
      (goto-char (line-beginning-position))
      (if (looking-at ledger-post-line-regexp)
          (ledger-post-align-amount)))))

(defun ledger-post-edit-amount ()
  (interactive)
  (goto-char (line-beginning-position))
  (when (re-search-forward ledger-post-line-regexp (line-end-position) t) 
    (goto-char (match-end ledger-regex-post-line-group-account)) ;go to the and of the account
    (let ((end-of-amount (re-search-forward "[-.,0-9]+" (line-end-position) t))) ;determine if the is an amount to edit
      (if end-of-amount
	  (let ((val (match-string 0)))
	    (goto-char (match-beginning 0))
	    (delete-region (match-beginning 0) (match-end 0))
	    (calc)
	    (while (string-match "," val)
	      (setq val (replace-match "" nil nil val))) ;gets rid of commas
	    (calc-eval val 'push)) ;edit the amount
	  (progn  ;make sure there are two spaces after the account name and go to calc
	    (if (search-backward "  " (- (point) 3) t)
		(goto-char (line-end-position))
		(insert "  "))
	    (calc)) 
	  ))))

(defun ledger-post-prev-xact ()
  (interactive)
  (backward-paragraph)
  (when (re-search-backward ledger-xact-line-regexp nil t)
    (goto-char (match-beginning 0))
    (re-search-forward ledger-post-line-regexp)
    (goto-char (match-end ledger-regex-post-line-group-account))))

(defun ledger-post-next-xact ()
  (interactive)
  (when (re-search-forward ledger-xact-line-regexp nil t)
    (goto-char (match-beginning 0))
    (re-search-forward ledger-post-line-regexp)
    (goto-char (match-end ledger-regex-post-line-group-account))))

(defun ledger-post-setup ()
  (if ledger-post-auto-adjust-amounts
      (add-hook 'after-change-functions 'ledger-post-maybe-align t t))
  (add-hook 'after-save-hook #'(lambda () (setq ledger-post-current-list nil))))

(provide 'ldg-post)
