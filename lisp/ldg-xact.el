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

(defun ledger-sort ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (sort-subr
     nil
     (function
      (lambda ()
        (if (re-search-forward
             (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+"
                     "\\(.+?\\)\\(\t\\|\n\\| [ \t]\\)") nil t)
            (goto-char (match-beginning 0))
          (goto-char (point-max)))))
     (function
      (lambda ()
        (forward-paragraph))))))

(provide 'ldg-xact)