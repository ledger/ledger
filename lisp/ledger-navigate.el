;;; ledger-navigate.el --- Provide navigation services through the ledger buffer.

;; Copyright (C) 2014-2015 Craig Earls (enderw88 AT gmail DOT com)

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


(provide 'ledger-navigate)

(defun ledger-navigate-next-xact ()
	"Move point to beginning of next xact."
 	;; make sure we actually move to the next xact, even if we are the
 	;; beginning of one now.
 	(if (looking-at ledger-payee-any-status-regex)
 			(forward-line))
   (if (re-search-forward  ledger-payee-any-status-regex nil t)
       (goto-char (match-beginning 0))
     (goto-char (point-max))))

(defun ledger-navigate-start-xact-or-directive-p ()
	"return t if at the beginning of an empty line or line
beginning with whitespace"
	(not (looking-at "[ \t]\\|\\(^$\\)")))

(defun ledger-navigate-next-xact-or-directive ()
	"move to the beginning of the next xact or directive"
	(interactive)
	(beginning-of-line)
	(if (ledger-navigate-start-xact-or-directive-p) ; if we are the start of an xact, move forward to the next xact
			(progn
				(forward-line)
				(if (not (ledger-navigate-start-xact-or-directive-p)) ; we have moved forward and are not at another xact, recurse forward
						(ledger-navigate-next-xact-or-directive)))
		(while (not	(or (eobp)  ; we didn't start off at the beginning of an xact
										(ledger-navigate-start-xact-or-directive-p)))
			(forward-line))))

(defun ledger-navigate-prev-xact ()
  "Move point to beginning of previous xact."
	(ledger-navigate-beginning-of-xact)
	(re-search-backward ledger-xact-start-regex nil t))

(defun ledger-navigate-beginning-of-xact ()
	"Move point to the beginning of the current xact"
	(interactive)
	;; need to start at the beginning of a line incase we are in the first line of an xact already.
	(beginning-of-line)
	(unless (looking-at ledger-xact-start-regex)
		(re-search-backward ledger-xact-start-regex nil t)
		(beginning-of-line))
	(point))

(defun ledger-navigate-end-of-xact ()
  "Move point to end of xact."
	(interactive)
  (ledger-navigate-next-xact-or-directive)
	(backward-char)
	(end-of-line)
	(point))

(defun ledger-navigate-to-line (line-number)
  "Rapidly move point to line LINE-NUMBER."
  (goto-char (point-min))
  (forward-line (1- line-number)))

(defun ledger-navigate-find-xact-extents (pos)
  "Return list containing point for beginning and end of xact containing POS.
Requires empty line separating xacts."
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (list (ledger-navigate-beginning-of-xact)
					(ledger-navigate-end-of-xact))))

;;; ledger-navigate.el ends here
