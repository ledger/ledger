;;; ledger-xact.el --- Helper code for use with the "ledger" command-line tool

;; Copyright (C) 2003-2014 John Wiegley (johnw AT gnu DOT org)

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
;;

;;; Code:

;; (defun ledger-next-record-function ()
;;   "Move point to next transaction."
;; 	;; make sure we actually move to the next xact, even if we are the
;; 	;; beginning of one now.
;; 	(if (looking-at ledger-payee-any-status-regex)
;; 			(forward-line))
;;   (if (re-search-forward  ledger-payee-any-status-regex nil t)
;;       (goto-char (match-beginning 0))
;;     (goto-char (point-max))))

;; (defun ledger-prev-record-function ()
;;   "Move point to beginning of previous xact."
;; 	(ledger-beginning-record-function)
;; 	(re-search-backward ledger-xact-start-regex nil t))

;; (defun ledger-beginning-record-function ()
;; 	"Move point to the beginning of the current xact"
;; 	(interactive)
;; 	(unless (looking-at ledger-xact-start-regex)
;; 		(re-search-backward ledger-xact-start-regex nil t)
;; 		(beginning-of-line))
;; 	(point))

;; (defun ledger-end-record-function ()
;;   "Move point to end of xact."
;; 	(interactive)
;;   (ledger-navigate-next-xact)
;; 	(backward-char)
;; 	(end-of-line)
;; 	(point))

(defun ledger-sort-find-start ()
  (if (re-search-forward ";.*Ledger-mode:.*Start sort" nil t)
      (match-end 0)))

(defun ledger-sort-find-end ()
  (if (re-search-forward ";.*Ledger-mode:.*End sort" nil t)
      (match-end 0)))

(defun ledger-sort-insert-start-mark ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (ledger-sort-find-start)
        (delete-region (match-beginning 0) (match-end 0))))
  (beginning-of-line)
  (insert "\n; Ledger-mode: Start sort\n\n"))

(defun ledger-sort-insert-end-mark ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (ledger-sort-find-end)
        (delete-region (match-beginning 0) (match-end 0))))
  (beginning-of-line)
  (insert "\n; Ledger-mode: End sort\n\n"))

(defun ledger-sort-startkey ()
  "Return the actual date so the sort-subr doesn't sort onthe entire first line."
  (buffer-substring-no-properties (point) (+ 10 (point))))

(defun ledger-sort-region (beg end)
  "Sort the region from BEG to END in chronological order."
  (interactive "r") ;; load beg and end from point and mark
										;; automagically
  (let ((new-beg beg)
        (new-end end)
        point-delta
        (bounds (ledger-navigate-find-xact-extents (point)))
        target-xact)

    (setq point-delta (- (point) (car bounds)))
    (setq target-xact (buffer-substring (car bounds) (cadr bounds)))
    (setq inhibit-modification-hooks t)
    (save-excursion
      (save-restriction
        (goto-char beg)
				;; make sure point is at the beginning of a xact
        (ledger-navigate-next-xact)
        (setq new-beg (point))
        (goto-char end)
        (ledger-navigate-next-xact)
				;; make sure end of region is at the beginning of next record
        ;; after the region
        (setq new-end (point))
        (narrow-to-region new-beg new-end)
        (goto-char new-beg)

        (let ((inhibit-field-text-motion t))
          (sort-subr
           nil
           'ledger-navigate-next-xact
           'ledger-navigate-end-of-xact
           'ledger-sort-startkey))))

    (goto-char (point-min))
    (re-search-forward (regexp-quote target-xact))
    (goto-char (+ (match-beginning 0) point-delta))
    (setq inhibit-modification-hooks nil)))

(defun ledger-sort-buffer ()
  "Sort the entire buffer."
  (interactive)
  (let (sort-start
        sort-end)
    (save-excursion
      (goto-char (point-min))
      (setq sort-start (ledger-sort-find-start)
            sort-end (ledger-sort-find-end)))
    (ledger-sort-region (if sort-start
                            sort-start
                          (point-min))
                        (if sort-end
                            sort-end
                          (point-max)))))

(provide 'ledger-sort)

;;; ledger-sort.el ends here
