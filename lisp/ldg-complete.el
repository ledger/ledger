;;; ldg-complete.el --- Helper code for use with the "ledger" command-line tool

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

;;(require 'esh-util)
;;(require 'esh-arg)

;;; Commentary:
;; Functions providing payee and account auto complete.

(require 'pcomplete)

;; In-place completion support

;;; Code:

(defun ledger-parse-arguments ()
  "Parse whitespace separated arguments in the current region."
  (let* ((info (save-excursion
                 (cons (ledger-thing-at-point) (point))))
         (begin (cdr info))
         (end (point))
         begins args)
    (save-excursion
      (goto-char begin)
      (when (< (point) end)
        (skip-chars-forward " \t\n")
        (setq begins (cons (point) begins))
        (setq args (cons (buffer-substring-no-properties
                          (car begins) end)
                         args)))
      (cons (reverse args) (reverse begins)))))

(defun ledger-payees-in-buffer ()
  "Scan buffer and return list of all payees."
  (let ((origin (point))
        payees-list)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+"
                      "\\(.+?\\)\\(\t\\|\n\\| [ \t]\\)") nil t)  ;; matches first line
        (unless (and (>= origin (match-beginning 0))
                     (< origin (match-end 0)))
          (setq payees-list (cons (match-string-no-properties 3)
                                   payees-list)))))  ;; add the payee
						     ;; to the list
    (pcomplete-uniqify-list (nreverse payees-list))))

(defun ledger-find-accounts-in-buffer ()
  "Search through buffer and build tree of accounts.
Return tree structure"
  (let ((origin (point))
	(account-tree (list t))
	(account-elements nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]+\\([*!]\\s-+\\)?[[(]?\\(.+?\\)\\(\t\\|\n\\| [ \t]\\)" nil t)
        (unless (and (>= origin (match-beginning 0))
                     (< origin (match-end 0)))
          (setq account-elements
		(split-string
		 (match-string-no-properties 2) ":"))
          (let ((root account-tree))
            (while account-elements
              (let ((entry (assoc (car account-elements) root)))
                (if entry
                    (setq root (cdr entry))
		    (setq entry (cons (car account-elements) (list t)))
		    (nconc root (list entry))
		    (setq root (cdr entry))))
              (setq account-elements (cdr account-elements)))))))
    account-tree))

(defun ledger-accounts ()
  "Return a tree of all accounts in the buffer."
  (let* ((current (caar (ledger-parse-arguments)))
         (elements (and current (split-string current ":")))
         (root (ledger-find-accounts-in-buffer))
         (prefix nil))
    (while (cdr elements)
      (let ((entry (assoc (car elements) root)))
        (if entry
            (setq prefix (concat prefix (and prefix ":")
                                 (car elements))
                  root (cdr entry))
	    (setq root nil elements nil)))
      (setq elements (cdr elements)))
    (and root
         (sort
          (mapcar (function
                   (lambda (x)
		    (let ((term (if prefix
				    (concat prefix ":" (car x))
				    (car x))))
		      (if (> (length (cdr x)) 1)
			  (concat term ":")
			  term))))
                  (cdr root))
          'string-lessp))))

(defun ledger-complete-at-point ()
  "Do appropriate completion for the thing at point."
  (interactive)
  (while (pcomplete-here
          (if (eq (save-excursion
                    (ledger-thing-at-point)) 'transaction)
              (if (null current-prefix-arg)
                  (ledger-payees-in-buffer)  ;; this completes against payee names
		  (progn
		    (let ((text (buffer-substring (line-beginning-position)
						  (line-end-position))))
		      (delete-region (line-beginning-position)
				     (line-end-position))
		      (condition-case err
			  (ledger-add-transaction text t)
			((error "ledger-complete-at-point")
			 (insert text))))
		    (forward-line)
		    (goto-char (line-end-position))
		    (search-backward ";" (line-beginning-position) t)
		    (skip-chars-backward " \t0123456789.,")
		    (throw 'pcompleted t)))
	      (ledger-accounts)))))

(defun ledger-fully-complete-entry ()
  "Completes a transaction if there is another matching payee in the buffer.
Does not use ledger xact"
  (interactive)
  (let* ((name (caar (ledger-parse-arguments)))
	(rest-of-name name)
        xacts)
    (save-excursion
      (when (eq 'transaction (ledger-thing-at-point))
	(delete-region (point) (+ (length name) (point)))
	;; Search backward for a matching payee
        (when (re-search-backward
               (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+\\(.*"
                       (regexp-quote name) ".*\\)" ) nil t)  ;; "\\(\t\\|\n\\| [ \t]\\)"
	  (setq rest-of-name (match-string 3))
          ;; Start copying the postings
	  (forward-line)
          (while (looking-at "^\\s-+")
            (setq xacts (cons (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))
                              xacts))
            (forward-line))
          (setq xacts (nreverse xacts)))))
    ;; Insert rest-of-name and the postings
    (when xacts
      (save-excursion
	(insert rest-of-name ?\n)
        (while xacts
          (insert (car xacts) ?\n)
          (setq xacts (cdr xacts))))
      (forward-line)
      (goto-char (line-end-position))
      (if (re-search-backward "\\(\t\\| [ \t]\\)" nil t)
          (goto-char (match-end 0))))))

(provide 'ldg-complete)

;;; ldg-complete.el ends here
