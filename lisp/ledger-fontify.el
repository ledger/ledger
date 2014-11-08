;;; ledger-fontify.el --- Provide custom fontification for ledger-mode


;; Copyright (C) 2014 Craig P. Earls (enderw88 at gmail dot com)

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.

;;; Commentary:
;;  Font-lock-mode doesn't handle multiline syntax very well.  This
;;  code provides font lock that is sensitive to overall transaction
;;  states


(provide 'ledger-fontify)

(defcustom ledger-fontify-xact-state-overrides nil
  "If t the overall xact state (cleard, pending, nil) will
  control the font of the entire transaction, not just the payee
  line."
  :type 'boolean
  :group 'ledger)

(defun ledger-fontify-buffer-part (&optional beg end len)
	(save-excursion
		(unless beg (setq beg (point-min)))
		(unless end (setq end (point-max)))
		(beginning-of-line)
		(while (< (point) end)
			(cond ((or (looking-at ledger-xact-start-regex)
								 (looking-at ledger-posting-regex))
						 (ledger-fontify-xact-at (point)))
						((looking-at ledger-directive-start-regex)
						 (ledger-fontify-directive-at (point))))
			(ledger-navigate-next-xact-or-directive))))

(defun ledger-fontify-xact-at (position)
  (interactive "d")
	(save-excursion
		(goto-char position)
		(let ((extents (ledger-navigate-find-element-extents position))
					(state (ledger-transaction-state)))
			(if (and ledger-fontify-xact-state-overrides state)
					(cond ((eq state 'cleared)
								 (ledger-fontify-set-face extents 'ledger-font-xact-cleared-face))
								((eq state 'pending)
								 (ledger-fontify-set-face extents 'ledger-font-xact-pending-face)))
				(ledger-fontify-xact-by-line extents)))))

(defun ledger-fontify-xact-by-line (extents)
	"do line-by-line detailed fontification of xact"
	(save-excursion
		(ledger-fontify-xact-start (car extents))
		(while (< (point) (cadr extents))
			(if (looking-at "[ \t]+;")
					(ledger-fontify-set-face (list (point) (progn
																									 (end-of-line)
																									 (point))) 'ledger-font-comment-face)
				(ledger-fontify-posting (point)))
			(forward-line))))

(defun ledger-fontify-xact-start (pos)
	"POS should be at the beginning of a line starting an xact.
Fontify the first line of an xact"
	(goto-char pos)
	(beginning-of-line)
	(let ((state nil))
		(re-search-forward ledger-xact-start-regex)
		(ledger-fontify-set-face (list (match-beginning 1) (match-end 1)) 'ledger-font-posting-date-face)
		(save-match-data (setq state (ledger-state-from-string (s-trim (match-string 5)))))
		(ledger-fontify-set-face (list (match-beginning 7) (match-end 7))
														 (cond ((eq state 'pending)
																		'ledger-font-payee-pending-face)
																	 ((eq state 'cleared)
																		'ledger-font-payee-cleared-face)
																	 (t
																		'ledger-font-payee-uncleared-face)))
		(ledger-fontify-set-face (list (match-beginning 8)
																	 (match-end 8)) 'ledger-font-comment-face)))

(defun ledger-fontify-posting (pos)
	(let ((state nil))
		(re-search-forward ledger-posting-regex)
		(if (match-string 1)
				(save-match-data (setq state (ledger-state-from-string (s-trim (match-string 1))))))
		(ledger-fontify-set-face (list (match-beginning 0) (match-end 2))
														 (cond ((eq state 'cleared)
																		'ledger-font-posting-account-cleared-face)
																	 ((eq state 'pending)
																		'ledger-font-posting-account-pending-face)
																	 (t
																		'ledger-font-posting-account-face)))
		(ledger-fontify-set-face (list (match-beginning 4) (match-end 4))
														 (cond ((eq state 'cleared)
																		'ledger-font-posting-amount-cleared-face)
																	 ((eq state 'pending)
																		'ledger-font-posting-amount-pending-face)
																	 (t
																		'ledger-font-posting-amount-face)))
		(ledger-fontify-set-face (list (match-beginning 5) (match-end 5))
														 'ledger-font-comment-face)))

(defun ledger-fontify-directive-at (position)
	(let ((extents (ledger-navigate-find-element-extents position))
				(face 'ledger-font-default-face))
		(cond ((looking-at "=")
					 (setq face 'ledger-font-auto-xact-face))
					((looking-at "~")
					 (setq face 'ledger-font-periodic-xact-face))
					((looking-at "[;#%|\\*]")
					 (setq face 'ledger-font-comment-face))
					((looking-at "\\(year\\)\\|Y")
					 (setq face 'ledger-font-year-directive-face))
					((looking-at "account")
					 (setq face 'ledger-font-account-directive-face))
					((looking-at "apply")
					 (setq face 'ledger-font-apply-directive-face))
					((looking-at "alias")
					 (setq face 'ledger-font-alias-directive-face))
					((looking-at "assert")
					 (setq face 'ledger-font-assert-directive-face))
					((looking-at "\\(bucket\\)\\|A")
					 (setq face 'ledger-font-bucket-directive-face))
					((looking-at "capture")
					 (setq face 'ledger-font-capture-directive-face))
					((looking-at "check")
					 (setq face 'ledger-font-check-directive-face))
					((looking-at "commodity")
					 (setq face 'ledger-font-commodity-directive-face))
					((looking-at "define")
					 (setq face 'ledger-font-define-directive-face))
					((looking-at "end")
					 (setq face 'ledger-font-end-directive-face))
					((looking-at "expr")
					 (setq face 'ledger-font-expr-directive-face))
					((looking-at "fixed")
					 (setq face 'ledger-font-fixed-directive-face))
					((looking-at "include")
					 (setq face 'ledger-font-include-directive-face))
					((looking-at "payee")
					 (setq face 'ledger-font-payee-directive-face))
					((looking-at "P")
					 (setq face 'ledger-font-price-directive-face))
					((looking-at "tag")
					 (setq face 'ledger-font-tag-directive-face)))
		(ledger-fontify-set-face extents face)))

(defun ledger-fontify-set-face (extents face)
	(put-text-property (car extents) (cadr extents) 'face face))


(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (s-trim-left (s-trim-right s)))
;;; ledger-fontify.el ends here
