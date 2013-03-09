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

;;; Commentary:
;; Helper functions to deal with commoditized numbers.  A commoditized
;; number will be a cons of value and string where the string contains
;; the commodity

;;; Code:

(defcustom ledger-reconcile-default-commodity "$"
  "The default commodity for use in target calculations in ledger reconcile."
  :type 'string
  :group 'ledger-reconcile)

(defun ledger-split-commodity-string (str)
  "Split a commoditized amount into two parts"
  (if (> (length str) 0) 
      (let (val
	    comm)
	(with-temp-buffer
	  (insert str)
	  (goto-char (point-min))
	  (cond ((re-search-forward "-?[1-9][0-9]*[.,][0-9]*" nil t)
		 ;; found a decimal number
		 (setq val 
		       (string-to-number
			(ledger-commodity-string-number-decimalize 
			 (delete-and-extract-region (match-beginning 0) (match-end 0)) :from-user)))
		 (goto-char (point-min))
		 (re-search-forward "[^[:space:]]" nil t)
		 (setq comm 
		       (delete-and-extract-region (match-beginning 0) (match-end 0)))
		 (list val comm))
		((re-search-forward "0" nil t)
		 ;; couldn't find a decimal number, look for a single 0,
		 ;; indicating account with zero balance
		 (list 0 ledger-reconcile-default-commodity))
		(t
		 (error "split-commodity-string: cannot parse commodity string: %S" str)))))
      (list 0 ledger-reconcile-default-commodity)))
    

(defun ledger-string-balance-to-commoditized-amount (str)
  "Return a commoditized amount (val, 'comm') from STR."
  (let ((fields (split-string str "[\n\r]"))) ; break any balances
					      ; with multi commodities
					      ; into a list
    (mapcar '(lambda (str)
	      (ledger-split-commodity-string str))
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

(defun ledger-commodity-string-number-decimalize (number-string direction)
  "Take NUMBER-STRING and ensure proper decimalization for use by string-to-number and number-to-string.  

DIRECTION can be :to-user or :from-user.  All math calculations
are done with decimal-period, some users may prefer decimal-comma
which must be translated both directions."
  (let ((val number-string))
    (if (assoc "decimal-comma" ledger-environment-alist)
	(cond ((eq direction :from-user)
	       ;; change string to decimal-period
	       (while (string-match "," val)
		 (setq val (replace-match "." nil nil val)))) ;; switch to period separator
	      ((eq direction :to-user)
	     ;; change to decimal-comma
	       (while (string-match "\\." val)
		 (setq val (replace-match "," nil nil val)))) ;; gets rid of periods
	      (t
	       (error "ledger-commodity-string-number-decimalize: direction not properly specified %S" direction)))
	(while (string-match "," val)
	  (setq val (replace-match "" nil nil val))))
    val))
      
	  
      
(defun ledger-commodity-to-string (c1)
  "Return string representing C1.
Single character commodities are placed ahead of the value,
longer one are after the value."
(let ((val (ledger-commodity-string-number-decimalize
	    (number-to-string (car c1)) :to-user))
      (commodity (cadr c1)))
  (if (> (length commodity) 1)
      (concat val " " commodity)
      (concat commodity " " val))))

(defun ledger-read-commodity-string (prompt)
  "Return a commoditizd value (val 'comm') from COMM.
Assumes a space between the value and the commodity."
  (let ((parts (split-string (read-from-minibuffer
			      (concat prompt " (" ledger-reconcile-default-commodity "): ")))))
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
