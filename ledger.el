;;; ledger.el --- Helper code for use with the "ledger" command-line tool

;; Copyright (C) 2004 John Wiegley (johnw AT gnu DOT org)

;; Emacs Lisp Archive Entry
;; Filename: ledger.el
;; Version: 1.1
;; Date: Thu 02-Apr-2004
;; Keywords: data
;; Author: John Wiegley (johnw AT gnu DOT org)
;; Maintainer: John Wiegley (johnw AT gnu DOT org)
;; Description: Helper code for using my "ledger" command-line tool
;; URL: http://www.newartisans.com/johnw/emacs.html
;; Compatibility: Emacs21

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

;; To use this module: Load this file, open a ledger data file, and
;; type M-x ledger-mode.  Once this is done, you can type:
;;
;;   C-c C-a  add a new entry, based on previous entries
;;   C-c C-r  reconcile the entries related to an account
;;
;; In the reconcile buffer, use SPACE to toggle the cleared status of
;; a transaction.

(defvar ledger-version "1.1"
  "The version of ledger.el currently loaded")

(defgroup ledger nil
  "Interface to the Ledger command-line accounting program."
  :group 'data)

(defcustom ledger-binary-path (executable-find "ledger")
  "Path to the ledger executable."
  :type 'file
  :group 'ledger)

(defcustom ledger-data-file (getenv "LEDGER")
  "Path to the ledger data file."
  :type 'file
  :group 'ledger)

(defvar bold 'bold)

(defvar ledger-font-lock-keywords
  '(("^[0-9./]+\\s-+\\(?:([^)]+)\\s-+\\)?\\([^*].+\\)" 1 bold)
    ("^\\s-+.+?\\(  \\|\t\\|\\s-+$\\)" . font-lock-keyword-face))
  "Default expressions to highlight in Ledger mode.")

(defun ledger-iterate-entries (callback)
  (goto-char (point-min))
  (let* ((now (current-time))
	 (current-year (nth 5 (decode-time now))))
    (while (not (eobp))
      (when (looking-at
	     (concat "\\(Y\\s-+\\([0-9]+\\)\\|"
		     "\\([0-9]\\{4\\}+\\)?[./]?"
		     "\\([0-9]+\\)[./]\\([0-9]+\\)\\s-+"
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

(defun ledger-find-slot (moment)
  (catch 'found
    (ledger-iterate-entries
     (function
      (lambda (start date mark desc)
	(if (time-less-p moment date)
	    (throw 'found t)))))))

(defun ledger-add-entry (entry-text)
  (interactive
   (list (read-string "Entry: " (format-time-string "%Y/%m/"))))
  (let* ((date (car (split-string entry-text)))
	 (insert-year t) exit-code)
    (if (string-match "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)" date)
	(setq date (encode-time 0 0 0 (string-to-int (match-string 3 date))
				(string-to-int (match-string 2 date))
				(string-to-int (match-string 1 date)))))
    (ledger-find-slot date)
    (save-excursion
      (if (re-search-backward "^Y " nil t)
	  (setq insert-year nil)))
    (save-excursion
      (insert
       (with-temp-buffer
	 (setq exit-code (ledger-run-ledger "entry" entry-text))
	 (if (= 0 exit-code)
	     (progn
	       (goto-char (point-min))
	       (delete-char 1)
	       (if insert-year
		   (buffer-string)
		 (buffer-substring 5 (point-max))))
	   (concat (if insert-year entry-text
		     (substring entry-text 5)) "\n"))) "\n"))))

(defun ledger-expand-entry ()
  (interactive)
  (ledger-add-entry (prog1
			(buffer-substring (line-beginning-position)
					  (line-end-position))
		      (delete-region (line-beginning-position)
				     (1+ (line-end-position))))))

(defun ledger-toggle-current ()
  (interactive)
  (let (clear)
    (save-excursion
      (when (or (looking-at "^[0-9]")
		(re-search-backward "^[0-9]" nil t))
	(skip-chars-forward "0-9./")
	(delete-horizontal-space)
	(if (equal ?\* (char-after))
	    (delete-char 1)
	  (insert " * ")
	  (setq clear t))))
    clear))

(defun ledger-print-result (command)
  (interactive "sLedger command: ")
  (shell-command (format "%s -f %s %s" ledger-binary-path
			 buffer-file-name command)))

(define-derived-mode ledger-mode text-mode "Ledger"
  "A mode for editing ledger data files."
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'indent-tabs-mode) nil)
  (if (boundp 'font-lock-defaults)
      (set (make-local-variable 'font-lock-defaults)
	   '(ledger-font-lock-keywords nil t)))
  (let ((map (current-local-map)))
    (define-key map [(control ?c) (control ?a)] 'ledger-add-entry)
    (define-key map [(control ?c) (control ?c)] 'ledger-toggle-current)
    (define-key map [(control ?c) (control ?p)] 'ledger-print-result)
    (define-key map [(control ?c) (control ?r)] 'ledger-reconcile)))

(defun ledger-parse-entries (account &optional all-p after-date)
  (let (total entries)
    (ledger-iterate-entries
     (function
      (lambda (start date mark desc)
	(when (and (or all-p (not mark))
		   (time-less-p after-date date))
	  (forward-line)
	  (setq total 0.0)
	  (while (looking-at
		  (concat "\\s-+\\([A-Za-z_].+?\\)\\(\\s-*$\\|  \\s-*"
			  "\\([^0-9]+\\)\\s-*\\([0-9,.]+\\)\\)?"
			  "\\(\\s-+;.+\\)?$"))
	    (let ((acct (match-string 1))
		  (amt (match-string 4)))
	      (when amt
		(while (string-match "," amt)
		  (setq amt (replace-match "" nil nil amt)))
		(setq amt (string-to-number amt)
		      total (+ total amt)))
	      (if (string= account acct)
		  (setq entries
			(cons (list (copy-marker start)
				    mark date desc (or amt total))
			      entries))))
	    (forward-line))))))
    entries))

(defvar ledger-reconcile-text "Reconcile")

(define-derived-mode ledger-reconcile-mode text-mode 'ledger-reconcile-text
  "A mode for reconciling ledger entries."
  (let ((map (make-sparse-keymap)))
    (define-key map [? ] 'ledger-reconcile-toggle)
    (define-key map [?q]
      (function
       (lambda ()
	 (interactive)
	 (kill-buffer (current-buffer)))))
    (use-local-map map)))

(add-to-list 'minor-mode-alist
	     '(ledger-reconcile-mode ledger-reconcile-text))

(defvar ledger-buf nil)
(defvar ledger-acct nil)

(defun ledger-update-balance-display ()
  (let ((account ledger-acct))
    (with-temp-buffer
      (let ((exit-code (ledger-run-ledger "-C" "balance" account)))
	(if (/= 0 exit-code)
	    (setq ledger-reconcile-text "Reconcile [ERR]")
	  (goto-char (point-min))
	  (delete-horizontal-space)
	  (skip-syntax-forward "^ ")
	  (setq ledger-reconcile-text
		(concat "Reconcile ["
			(buffer-substring-no-properties (point-min) (point))
			"]"))))))
  (force-mode-line-update))

(defun ledger-reconcile-toggle ()
  (interactive)
  (let ((where (get-text-property (point) 'where))
	(account ledger-acct)
	cleared)
    (with-current-buffer ledger-buf
      (goto-char where)
      (setq cleared (ledger-toggle-current))
      (save-buffer))
    (if cleared
	(add-text-properties (line-beginning-position)
			     (line-end-position)
			     (list 'face 'bold))
      (remove-text-properties (line-beginning-position)
			      (line-end-position)
			      (list 'face)))
    (forward-line)
    (ledger-update-balance-display)))

(defun ledger-reconcile (account &optional days)
  (interactive "sAccount to reconcile: \nnBack how far (default 30 days): ")
  (let* ((then (time-subtract (current-time)
			      (seconds-to-time (* (or days 30) 24 60 60))))
	 (items (save-excursion
		  (goto-char (point-min))
		  (ledger-parse-entries account t then)))
	 (buf (current-buffer)))
    (with-current-buffer
	(pop-to-buffer (generate-new-buffer "*Reconcile*"))
      (ledger-reconcile-mode)
      (set (make-local-variable 'ledger-buf) buf)
      (set (make-local-variable 'ledger-acct) account)
      (ledger-update-balance-display)
      (dolist (item items)
	(let ((beg (point)))
	  (insert (format "%s %-30s %8.2f\n"
			  (format-time-string "%Y/%m/%d" (nth 2 item))
			  (nth 3 item) (nth 4 item)))
	  (if (nth 1 item)
	      (set-text-properties beg (1- (point))
				   (list 'face 'bold
					 'where (nth 0 item)))
	    (set-text-properties beg (1- (point))
				 (list 'where (nth 0 item)))))
	(goto-char (point-min))))))

(defun ledger-align-dollars (&optional column)
  (interactive "p")
  (if (= column 1)
      (setq column 48))
  (while (search-forward "$" nil t)
    (backward-char)
    (let ((col (current-column))
	  (beg (point))
	  target-col len)
      (skip-chars-forward "-$0-9,.")
      (setq len (- (point) beg))
      (setq target-col (- column len))
      (if (< col target-col)
	  (progn
	    (goto-char beg)
	    (insert (make-string (- target-col col) ? )))
	(move-to-column target-col)
	(if (looking-back "  ")
	    (delete-char (- col target-col))
	  (skip-chars-forward "^ \t")
	  (delete-horizontal-space)
	  (insert "  ")))
      (forward-line))))

(defun ledger-run-ledger (&rest args)
  "run ledger with supplied arguments"
  (let ((command (mapconcat 'identity
			    (append (list ledger-binary-path
					  "-f" ledger-data-file) args) " ")))
    (insert (shell-command-to-string command)))
  0)

(provide 'ledger)

;;; ledger.el ends here
