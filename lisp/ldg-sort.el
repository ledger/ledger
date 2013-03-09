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
;; 

;;; Code:

(defun ledger-next-record-function ()
  "Move point to next transaction."
  (if (re-search-forward
       (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+"
	       "\\(.+?\\)\\(\t\\|\n\\| [ \t]\\)") nil t)
      (goto-char (match-beginning 0))
      (goto-char (point-max))))

(defun ledger-end-record-function ()
  "Move point to end of transaction."
  (forward-paragraph))

(defun ledger-sort-find-start ()
  (if (re-search-forward ";.*Ledger-mode:.*Start sort" nil t)
      (match-end 0)))

(defun ledger-sort-find-end ()
  (if (re-search-forward ";.*Ledger-mode:.*End sort" nil t)
      (match-end 0)))

(defun ledger-sort-insert-start-mark ()
  (interactive)
  (let (has-old-marker) 
    (save-excursion
      (goto-char (point-min))
      (setq has-old-marker (ledger-sort-find-start))
      (if has-old-marker 
	  (delete-region (match-beginning 0) (match-end 0))))
    (beginning-of-line)
    (insert "\n; Ledger-mode: Start sort\n\n")))

(defun ledger-sort-insert-end-mark ()
  (interactive)
  (let (has-old-marker) 
    (save-excursion
      (goto-char (point-min))
      (setq has-old-marker (ledger-sort-find-end))
      (if has-old-marker 
	  (delete-region (match-beginning 0) (match-end 0))))
    (beginning-of-line)
    (insert "\n; Ledger-mode: End sort\n\n")))

(defun ledger-sort-region (beg end)
  "Sort the region from BEG to END in chronological order."
  (interactive "r") ;; load beg and end from point and mark
		    ;; automagically
  (let ((new-beg beg)
	(new-end end))
    (save-excursion
      (save-restriction
   (goto-char beg)
	(ledger-next-record-function) ;; make sure point is at the
				      ;; beginning of a xact
	(setq new-beg (point))
	(goto-char end)
	(ledger-next-record-function) ;; make sure end of region is at
				      ;; the beginning of next record
				      ;; after the region
	(setq new-end (point))
	(narrow-to-region new-beg new-end)
	(goto-char new-beg)

	(let ((inhibit-field-text-motion t))
	  (sort-subr
	   nil
	   'ledger-next-record-function
	   'ledger-end-record-function))))))

(defun ledger-sort-buffer ()
  "Sort the entire buffer."
  (interactive)
  (goto-char (point-min))
  (let ((sort-start (ledger-sort-find-start))
	(sort-end (ledger-sort-find-end)))
    (ledger-sort-region (if sort-start
			    sort-start
			    (point-min)) 
			(if sort-end
			    sort-end
			    (point-max)))))

(provide 'ldg-sort)

;;; ldg-sort.el ends here
