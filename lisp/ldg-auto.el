;;; ldg-auto.el --- Helper code for use with the "ledger" command-line tool

;; Copyright (C) 2013 Craig Earls (enderw88 at gmail dot com)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 
;; This module provides or automatically adding transactions to a
;; ledger buffer on a periodic basis. h Recurrence expressions are
;; inspired by Martin Fowler's "Recurring Events for Calendars",
;; martinfowler.com/apsupp/recurring.pdf

;; use (fset 'VARNAME (macro args)) to put the macro definition in the
;; function slot of the symbol VARNAME.  Then use VARNAME as the
;; function without have to use funcall.

(defsubst between (val low high)
  (and (>= val low) (<= val high)))

(defun ledger-auto-days-in-month (month year)
  "Return number of days in the MONTH, MONTH is form 1 to 12"
  (if (between month 1 12)
      (if (and (date-leap-year-p year) (= 2 month))
	  29
	  (nth (1- month) '(31 28 31 30 31 30 31 31 30 31 30 31)))
      (error "Month out of range, MONTH=%S" month)))

;; Macros to handle date expressions
      
(defmacro ledger-auto-day-in-month-macro (count day-of-week)
  "Return a form that evaluates DATE that returns true for the COUNT DAY-OF-WEEK.
For example, return true if date is the 3rd Thursday of the
month.  Negative COUNT starts from the end of the month. (EQ
COUNT 0) means EVERY day-of-week (eg. every Saturday)"
  (if (and (between count -6 6) (between day-of-week 0 6))
      (cond ((zerop count) ;; Return true if day-of-week matches
	     `(eq (nth 6 (decode-time date)) ,day-of-week))
	    ((> count 0) ;; Positive count
	     (let ((decoded (gensym)))
	       `(let ((,decoded (decode-time date)))
		   (if (and (eq (nth 6 ,decoded) ,day-of-week)
			    (between  (nth 3 ,decoded) 
				      ,(* (1- count) 7) 
				      ,(* count 7)))
		       t
		       nil))))
	    ((< count 0) 
	     (let ((days-in-month (gensym))
		   (decoded (gensym)))
	       `(let* ((,decoded (decode-time date))
		       (,days-in-month (ledger-auto-days-in-month 
					(nth 4 ,decoded) 
					(nth 5 ,decoded))))
		  (if (and (eq (nth 6 ,decoded) ,day-of-week)
			   (between  (nth 3 ,decoded) 
				     (+ ,days-in-month ,(* count 7)) 
				     (+ ,days-in-month ,(* (1+ count) 7))))
		      t
		      nil))))
	    (t
	     (error "COUNT out of range, COUNT=%S" count)))
      (error "Invalid argument to ledger-auto-day-in-month-macro %S %S" 
	     count 
	     day-of-week)))
		  
(defmacro ledger-auto-day-of-month-macro (day)
  "Return a form of date that returns true for the DAY of the month.
For example, return true if date is the 23rd of the month."
  `(if (eq (nth 3 (decode-time date)) ,day)
       t))

(defmacro ledger-auto-month-of-year-macro (month)
  "Return a form of date that returns true for the MONTH of the year.
For example, return true if date is the 4th month of the year."
  `(if (eq (nth 4 (decode-time date)) ,month)
       t))

(defmacro ledger-auto-every-count-day-macro (day-of-week skip start-date)
  "Return a form that is true for every DAY skipping SKIP, starting on START.
For example every second Friday, regardless of month."
  (let ((start-day (nth 6 (decode-time (eval start-date)))))
    (if (eq start-day day-of-week)  ;; good, can proceed
	`(if (zerop (mod (- (time-to-days date) ,(time-to-days (eval start-date))) ,(* skip 7)))
	     t
	     nil)
	(error "START-DATE day of week doesn't match DAY-OF-WEEK"))))

(defmacro ledger-auto-date-range-macro (month1 day1 month2 day2)
  "Return a form of DATE that is true if DATE falls between MONTH1 DAY1 and MONTH2 DAY2."
  (let ((decoded (gensym))
	(target-month (gensym))
	(target-day (gensym)))
    `(let* ((,decoded (decode-time date))
	    (,target-month (nth 4 decoded))
	    (,target-day (nth 3 decoded)))
       (and (and (> ,target-month ,month1)
		 (< ,target-month ,month2))
	    (and (> ,target-day ,day1)
		 (< ,target-day ,day2))))))

(defun ledger-auto-is-holiday (date)
  "Return true if DATE is a holiday.")

(defun ledger-auto-scan-transactions (auto-file)
  (let ((xact-list (list)))
    (save-excursion
      (find-file auto-file)
      (goto-char (point-min))
      (while (re-search-forward "^\\[\\(.*\\)\\] " nil t)
	(let ((date-descriptor "")
	      (transaction nil)
	      (xact-start (match-end 0)))
	  (setq date-descriptors 
		(ledger-auto-read-descriptor-tree
		 (buffer-substring-no-properties 
		  (match-beginning 0) 
		  (match-end 0))))
	  (forward-paragraph)
	  (setq transaction (list date-descriptors
				  (buffer-substring-no-properties
				   xact-start
				   (point))))
	  (setq xact-list (cons transaction xact-list))))
    xact-list)))
	  
(defun ledger-auto-read-descriptor-tree (descriptor-string)
  "Take a date descriptor string and return a function that
returns true if the date meets the requirements"
  (with-temp-buffer
    (let (pos)
      (insert descriptor-string)
      (goto-char (point-min))
      (replace-string "[" "(")
      (goto-char (point-min))
      (replace-string "]" ")")
      (goto-char (point-max))
      (while (re-search-backward 
	      (concat "\\([0-9]+\\|[\*]\\)/"  ;; Year slot
		      "\\([\*EO]\\|[0-9]+\\)/" ;; Month slot
		      "\\([\*]\\|\\([0-9][0-9]\\)\\|"
		      "\\([0-5]"
		      "\\(\\(Su\\)\\|"
		      "\\(Mo\\)\\|" 
		      "\\(Tu\\)\\|"
		      "\\(We\\)\\|"
		      "\\(Th\\)\\|"
		      "\\(Fr\\)\\|"
		      "\\(Sa\\)\\)\\)\\)") nil t) ;; Day slot
	(goto-char 
	 (match-end 0))
	(insert ?\")
	(goto-char (match-beginning 0))
	(insert "\"" )))
       (ledger-auto-traverse-descriptor-tree 
	(read (buffer-substring (point-min) (point-max))) 0)))

(defun ledger-auto-traverse-descriptor-tree (tree depth)
  (dolist (node tree)
	  (cond ((eq (type-of node) 'string)
		 (ledger-auto-parse-date-descriptor node))
		((eq (type-of node) 'cons)
		 (ledger-auto-traverse-descriptor-tree node (1+ depth))))))
  

(defun ledger-auto-parse-date-descriptor (descriptor)
  "Parse the date descriptor, return the evaluator"
  descriptor)

(provide 'ldg-auto)

;;; ldg-auto.el ends here
