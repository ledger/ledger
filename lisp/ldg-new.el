;;; ledger.el --- Helper code for use with the "ledger" command-line tool

;; Copyright (C) 2003-2010 John Wiegley (johnw AT gnu DOT org)

;; Emacs Lisp Archive Entry
;; Filename: ledger.el
;; Version: 3.0
;; Date: Mon 12-Apr-2010
;; Keywords: data
;; Author: John Wiegley (johnw AT gnu DOT org)
;; Maintainer: John Wiegley (johnw AT gnu DOT org)
;; Description: Helper code for using my "ledger" command-line tool
;; URL: http://www.newartisans.com/johnw/emacs.html
;; Compatibility: Emacs22,Emacs23,Emacs24

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

(require 'ldg-post)
(require 'ldg-mode)
(require 'ldg-complete)
(require 'ldg-state)
(require 'ldg-report)

;(autoload #'ledger-mode "ldg-mode" nil t)
;(autoload #'ledger-fully-complete-entry "ldg-complete" nil t)
;(autoload #'ledger-toggle-current "ldg-state" nil t)

(autoload #'ledger-texi-update-test "ldg-texi" nil t)
(autoload #'ledger-texi-update-examples "ldg-texi" nil t)

(defgroup ledger nil
  "Interface to the Ledger command-line accounting program."
  :group 'data)

(defconst ledger-version "3.0"
  "The version of ledger.el currently loaded")

(provide 'ledger)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ledger-create-test ()
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (save-excursion
      (let (text beg)
        (goto-char (point-min))
        (forward-line 1)
        (setq beg (point))
        (search-forward ":PROPERTIES:")
        (goto-char (line-beginning-position))
        (setq text (buffer-substring-no-properties beg (point)))
        (goto-char (point-min))
        (re-search-forward ":ID:\\s-+\\([^-]+\\)")
        (find-file-other-window
         (format "~/src/ledger/test/regress/%s.test" (match-string 1)))
        (sit-for 0)
        (insert text)
        (goto-char (point-min))
        (while (not (eobp))
          (goto-char (line-beginning-position))
          (delete-char 3)
          (forward-line 1))))))

;;; ledger.el ends here
