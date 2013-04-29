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
;; Load up the ledger mode
(require 'ldg-regex)
(require 'esh-util)
(require 'esh-arg)
(require 'ldg-commodities)
(require 'ldg-complete)
(require 'ldg-context)
(require 'ldg-exec)
(require 'ldg-fonts)
(require 'ldg-init)
(require 'ldg-mode)
(require 'ldg-occur)
(require 'ldg-post)
(require 'ldg-reconcile)
(require 'ldg-report)
(require 'ldg-sort)
(require 'ldg-state)
(require 'ldg-test)
(require 'ldg-texi)
(require 'ldg-xact)
(require 'ldg-schedule)

;;; Code:

(autoload #'ledger-texi-update-test "ldg-texi" nil t)
(autoload #'ledger-texi-update-examples "ldg-texi" nil t)

(defgroup ledger nil
  "Interface to the Ledger command-line accounting program."
  :group 'data)

(defconst ledger-version "3.0"
  "The version of ledger.el currently loaded.")

(defun ledger-mode-dump-variable (var)
  (if var
   (insert (format "         %s: %S\n" (symbol-name var) (eval var)))))
  
(defun ledger-mode-dump-group (group)
  "Dump GROUP customizations to current buffer"
  (let ((members (custom-group-members group nil)))
    (dolist (member members)
      (cond ((eq (cadr member) 'custom-group)
	     (insert (format "Group %s:\n" (symbol-name (car member))))
	     (ledger-mode-dump-group (car member)))
	    ((eq (cadr member) 'custom-variable) 
	     (ledger-mode-dump-variable (car member)))))))

(defun ledger-mode-dump-configuration ()
  "Dump all customizations"
  (find-file "ledger-mode-dump")
  (ledger-mode-dump-group 'ledger))
 
(provide 'ledger)

;;; ldg-new.el ends here

