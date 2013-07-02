;;; ledger-commodities.el --- Helper code for use with the "ledger" command-line tool

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
;; Helper functions to deal with commoditized numbers.  A commoditized
;; number will be a list of value and string where the string contains
;; the commodity

;;; Code:

(require 'ledger-regex)

(defcustom ledger-reconcile-default-commodity "$"
  "The default commodity for use in target calculations in ledger reconcile."
  :type 'string
  :group 'ledger-reconcile)

(defcustom ledger-scale  10000
	"The 10 ^ maximum number of digits you would expect to appear in your reports.
This is a cheap way of getting around floating point silliness in subtraction")

(defun ledger-split-commodity-string (str)
  "Split a commoditized string, STR, into two parts.
Returns a list with (value commodity)."
  (let ((number-regex (if (assoc "decimal-comma" ledger-environment-alist)
											 ledger-amount-decimal-comma-regex
											 ledger-amount-decimal-period-regex)))
		(if (> (length str) 0)
				(with-temp-buffer
					(insert str)
					(goto-char (point-min))
					(cond
						((re-search-forward "\"\\(.*\\)\"" nil t) ; look for quoted commodities
						 (let ((com (delete-and-extract-region
												 (match-beginning 1)
												 (match-end 1))))
							 (if (re-search-forward
										number-regex nil t)
									(list
									 (ledger-string-to-number
										(delete-and-extract-region (match-beginning 0) (match-end 0)))
									 com))))
					 ((re-search-forward number-regex nil t)
						;; found a number in the current locale, return it in the
						;; car.  Anything left over is annotation, the first
						;; thing should be the commodity, separated by
						;; whitespace, return it in the cdr.  I can't think of
						;; any counterexamples
						(list
						 (ledger-string-to-number
							(delete-and-extract-region (match-beginning 0) (match-end 0)))
						 (nth 0 (split-string (buffer-substring-no-properties (point-min) (point-max))))))
					 ((re-search-forward "0" nil t)
						;; couldn't find a decimal number, look for a single 0,
						;; indicating account with zero balance
						(list 0 ledger-reconcile-default-commodity))))
			 ;; nothing found, return 0
			 (list 0 ledger-reconcile-default-commodity))))

(defun ledger-string-balance-to-commoditized-amount (str)
  "Return a commoditized amount (val, 'comm') from STR."
  ; break any balances with multi commodities into a list
  (mapcar #'(lambda (st)
	      (ledger-split-commodity-string st))
	  (split-string str "[\n\r]")))

(defun -commodity (c1 c2)
  "Subtract C2 from C1, ensuring their commodities match."
  (if (string= (cadr c1) (cadr c2))
			; the scaling below is to get around inexact subtraction results where, for example
			; 1.23 - 4.56 = -3.3299999999999996 instead of -3.33
      (list (/ (-  (* ledger-scale (car c1)) (* ledger-scale (car c2))) ledger-scale) (cadr c1))
      (error "Can't subtract different commodities %S from %S" c2 c1)))

(defun +commodity (c1 c2)
  "Add C1 and C2, ensuring their commodities match."
  (if (string= (cadr c1) (cadr c2))
      (list (+ (car c1) (car c2)) (cadr c1))
      (error "Can't add different commodities, %S to %S" c1 c2)))

(defun ledger-strip (str char)
	(let (new-str)
		(concat (dolist (ch (append str nil) new-str)
							(unless (= ch char)
								(setq new-str (append new-str (list ch))))))))

(defun ledger-string-to-number (str &optional decimal-comma)
	"improve builtin string-to-number by handling internationalization, and return nil if number can't be parsed"
	(let ((nstr (if (or decimal-comma
											(assoc "decimal-comma" ledger-environment-alist))
									(ledger-strip str ?.)
									(ledger-strip str ?,))))
		(while (string-match "," nstr)  ;if there is a comma now, it is a thousands separator
			(setq nstr (replace-match "." nil nil nstr)))
		(string-to-number nstr)))

(defun ledger-number-to-string (n &optional decimal-comma)
	(let ((str (number-to-string n)))
		(if (or decimal-comma
						(assoc "decimal-comma" ledger-environment-alist))
				(while (string-match "\\." str)
					 (setq str (replace-match "," nil nil str)))
				str)))

(defun ledger-commodity-to-string (c1)
  "Return string representing C1.
Single character commodities are placed ahead of the value,
longer ones are after the value."
	(let ((str (ledger-number-to-string (car c1)))
				(commodity (cadr c1)))
  (if (> (length commodity) 1)
      (concat str " " commodity)
      (concat commodity " " str))))

(defun ledger-read-commodity-string (prompt)
  (let ((str (read-from-minibuffer
							(concat prompt " (" ledger-reconcile-default-commodity "): ")))
				comm)
    (if (and (> (length str) 0)
						 (ledger-split-commodity-string str))
				(progn
					(setq comm (ledger-split-commodity-string str))
					(if (cadr comm)
							comm
							(list (car comm) ledger-reconcile-default-commodity))))))

(provide 'ledger-commodities)

;;; ledger-commodities.el ends here
