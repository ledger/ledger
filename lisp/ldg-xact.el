;;; ldg-xact.el --- Helper code for use with the "ledger" command-line tool

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

;; A sample entry sorting function, which works if entry dates are of
;; the form YYYY/mm/dd.

(defcustom ledger-highlight-xact-under-point t
  "If t highlight xact under point"
  :type 'boolean
  :group 'ledger)

(defvar highlight-overlay (list))

(defun ledger-find-xact-extents (pos)
  "return point for beginning of xact and and of xact containing
   position.  Requires empty line separating xacts"
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (let ((end-pos pos)
	  (beg-pos pos))
      (backward-paragraph)
      (forward-line)
      (beginning-of-line)
      (setq beg-pos (point))
      (forward-paragraph)
      (forward-line -1)
      (end-of-line)
      (setq end-pos (1+ (point)))
      (list beg-pos end-pos))))


(defun ledger-highlight-xact-under-point ()
  (if ledger-highlight-xact-under-point
      (let ((exts (ledger-find-xact-extents (point)))
	    (ovl highlight-overlay))
	(if (not highlight-overlay)
	    (setq ovl
		  (setq highlight-overlay
			(make-overlay (car exts)
				      (cadr exts)
				      (current-buffer) t nil)))
	    (move-overlay ovl (car exts) (cadr exts)))
	(overlay-put ovl 'face 'ledger-font-highlight-face)
	(overlay-put ovl 'priority 100))))

(defun ledger-xact-payee ()
  "Returns the payee of the entry containing point or nil."
  (let ((i 0))
    (while (eq (ledger-context-line-type (ledger-context-other-line i)) 'acct-transaction)
      (setq i (- i 1)))
    (let ((context-info (ledger-context-other-line i)))
      (if (eq (ledger-context-line-type context-info) 'entry)
          (ledger-context-field-value context-info 'payee)
	  nil))))

(defsubst ledger-goto-line (line-number)
  (goto-char (point-min)) (forward-line (1- line-number)))

(defun ledger-thing-at-point ()
  (let ((here (point)))
    (goto-char (line-beginning-position))
    (cond ((looking-at "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.+?)\\)?\\s-+")
           (goto-char (match-end 0))
           'transaction)
          ((looking-at "^\\s-+\\([*!]\\s-+\\)?[[(]?\\(.\\)")
           (goto-char (match-beginning 2))
           'posting)
          ((looking-at "^\\(sun\\|mon\\|tue\\|wed\\|thu\\|fri\\|sat\\)\\s-+")
           (goto-char (match-end 0))
           'entry)
          (t
           (ignore (goto-char here))))))

(defun ledger-copy-transaction-at-point (date)
  (interactive  (list
		 (read-string "Copy to date: " 
			      (concat ledger-year "/" ledger-month "/")))) 
  (let* ((here (point))
	 (extents (ledger-find-xact-extents (point)))
	 (transaction (buffer-substring (car extents) (cadr extents)))
	 encoded-date)
    (if (string-match "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)" date)
	(setq encoded-date
	      (encode-time 0 0 0 (string-to-number (match-string 3 date))
			   (string-to-number (match-string 2 date))
			   (string-to-number (match-string 1 date)))))
    (ledger-find-slot encoded-date)
    (insert transaction "\n")
    (backward-paragraph)
    (re-search-forward "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)")
    (replace-match date)
    (re-search-forward "[1-9][0-9]+\.[0-9]+")))


    
(provide 'ldg-xact)