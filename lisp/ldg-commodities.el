;;; ldg-commodities.el --- Helper code for use with the "ledger" command-line tool

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




;;; Commentary:
;; Helper functions to deal with commoditized numbers.  A commoditized
;; number will be a cons of value and string where the string contains
;; the commodity

;;; Code:

(defcustom ledger-reconcile-default-commodity "$"
  "The default commodity for use in target calculations in ledger reconcile."
  :type 'string
  :group 'ledger)

(defun ledger-string-balance-to-commoditized-amount (str)
  "Return a commoditized amount (val, 'comm') from STR."
  (let ((fields (split-string str "[\n\r]"))) ; break any balances
					      ; with multi commodities
					      ; into a list
    (mapcar '(lambda (str)
	      (let* ((parts (split-string str))  ;break into number and commodity string
		     (first (car parts))
		     (second (cadr parts)))
		;"^-*[1-9][0-9]*[.,][0-9]*"
		(if (string-match "^-*[1-9]+" first)
		    (list (string-to-number first) second)
		    (list (string-to-number second) first))))
	    fields)))


(defun -commodity (c1 c2)
  "Subtract C2 from C1, ensuring their commodities match."
  (if (string= (cadr c1) (cadr c2))
      (list (- (car c1) (car c2)) (cadr c1))
      (error "Can't subtract different commodities %S from %S" c2 c1)))

(defun +commodity (c1 c2)
  "Add C1 and C2, ensuring their commodities match."
  (if (string= (cadr c1) (cadr c2))
      (list (+ (car c1) (car c2)) (cadr c1))
      (error "Can't add different commodities, %S to %S" c1 c2)))

(defun ledger-commodity-to-string (c1)
  "Return string representing C1.
Single character commodities are placed ahead of the value,
longer one are after the value."
(let ((val (number-to-string (car c1)))
	(commodity (cadr c1)))
    (if (> (length commodity) 1)
	(concat val " " commodity)
	(concat commodity " " val))))

(defun ledger-read-commodity-string (comm)
  "Return a commoditizd value (val 'comm') from COMM.
Assumes a space between the value and the commodity."
  (interactive (list (read-from-minibuffer
		(concat "Enter commoditized amount (" ledger-reconcile-default-commodity "): "))))
  (let ((parts (split-string comm)))
    (if parts
	(if (/= (length parts) 2) ;;assume a number was entered and use default commodity
	    (list (string-to-number (car parts))
		  ledger-reconcile-default-commodity)
	    (let ((valp1 (string-to-number (car parts)))
		  (valp2 (string-to-number (cadr parts))))
	      (cond ((and (= valp1 valp2) (= 0 valp1));; means neither contained a valid number (both = 0)
		     (list 0 ""))
		    ((and (/= 0 valp1) (= valp2 0))
		     (list valp1 (cadr parts)))
		    ((and (/= 0 valp2) (= valp1 0))
		     (list valp2 (car parts)))
		    (t
		     (error "Cannot understand commodity"))))))))

(provide 'ldg-commodities)

;;; ldg-commodities.el ends here
