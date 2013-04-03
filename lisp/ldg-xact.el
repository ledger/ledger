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


;;; Commentary:
;; Utilities for running ledger synchronously.

;;; Code:

(defcustom ledger-highlight-xact-under-point t
  "If t highlight xact under point."
  :type 'boolean
  :group 'ledger)

(defvar highlight-overlay (list))

(defun ledger-find-xact-extents (pos)
  "Return point for beginning of xact and and of xact containing position.
Requires empty line separating xacts.  Argument POS is a location
within the transaction."
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (let ((end-pos pos)
	  (beg-pos pos))
      (backward-paragraph)
      (if (/= (point) (point-min))
	  (forward-line))
      (setq beg-pos (line-beginning-position))
      (forward-paragraph)
      (forward-line -1)
      (setq end-pos (1+ (line-end-position)))
      (list beg-pos end-pos))))


(defun ledger-highlight-xact-under-point ()
  "Move the highlight overlay to the current transaction."
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
	(overlay-put ovl 'face 'ledger-font-xact-highlight-face)
	(overlay-put ovl 'priority 100))))

(defun ledger-xact-payee ()
  "Return the payee of the entry containing point or nil."
  (let ((i 0))
    (while (eq (ledger-context-line-type (ledger-context-other-line i)) 'acct-transaction)
      (setq i (- i 1)))
    (let ((context-info (ledger-context-other-line i)))
      (if (eq (ledger-context-line-type context-info) 'entry)
          (ledger-context-field-value context-info 'payee)
	  nil))))

(defun ledger-xact-find-slot (moment)
  "Find the right place in the buffer for a transaction at MOMENT.
MOMENT is an encoded date"
  (catch 'found
    (ledger-xact-iterate-transactions
     (function
      (lambda (start date mark desc)
       (if (ledger-time-less-p moment date)
	   (throw 'found t)))))))

(defun ledger-xact-iterate-transactions (callback)
  "Iterate through each transaction call CALLBACK for each."
  (goto-char (point-min))
  (let* ((now (current-time))
         (current-year (nth 5 (decode-time now))))
    (while (not (eobp))
      (when (looking-at ledger-iterate-regex)
        (let ((found-y-p (match-string 2)))  
          (if found-y-p  
              (setq current-year (string-to-number found-y-p)) ;; a Y directive was found
	      (let ((start (match-beginning 0))
		    (year (match-string 4))
		    (month (string-to-number (match-string 5)))
		    (day (string-to-number (match-string 6)))
		    (mark (match-string 7))
		    (code (match-string 8))
		    (desc (match-string 9)))
		(if (and year (> (length year) 0))
		    (setq year (string-to-number year)))
		(funcall callback start
			 (encode-time 0 0 0 day month
				      (or year current-year))
			 mark desc)))))
      (forward-line))))

(defsubst ledger-goto-line (line-number)
  "Rapidly move point to line LINE-NUMBER."
  (goto-char (point-min)) 
  (forward-line (1- line-number)))

(defun ledger-thing-at-point ()
  "Describe thing at points.  Return 'transaction, 'posting, or nil."
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
  "Ask for a new DATE and copy the transaction under point to that date.  Leave point on the first amount."
  (interactive  (list
		 (read-string "Copy to date: " 
			      (concat ledger-year "/" ledger-month "/") 'ledger-minibuffer-history)))
  (let* ((here (point))
	 (extents (ledger-find-xact-extents (point)))
	 (transaction (buffer-substring-no-properties (car extents) (cadr extents)))
	 encoded-date)
    (if (string-match ledger-iso-date-regexp date)
	(setq encoded-date
	      (encode-time 0 0 0 (string-to-number (match-string 4 date))
			   (string-to-number (match-string 3 date))
			   (string-to-number (match-string 2 date)))))
    (ledger-xact-find-slot encoded-date)
    (insert transaction "\n")
    (backward-paragraph 2)
    (re-search-forward ledger-iso-date-regexp)
    (replace-match date)
    (ledger-next-amount)))

(provide 'ldg-xact)

;;; ldg-xact.el ends here
