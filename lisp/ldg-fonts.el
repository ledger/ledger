;;; ldg-fonts.el --- Helper code for use with the "ledger" command-line tool

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
;; All of the faces for ledger mode are defined here.

;;; Code:

(defgroup ledger-faces nil "Ledger mode highlighting" :group 'ledger)
(defface ledger-font-uncleared-face
    `((t :foreground "#dc322f" :weight bold ))
  "Default face for Ledger"
  :group 'ledger-faces)

(defface ledger-font-cleared-face
    `((t :foreground "#657b83" :weight normal ))
  "Default face for cleared (*) transactions"
  :group 'ledger-faces)

(defface ledger-font-highlight-face
    `((t :background "white"))
  "Default face for transaction under point"
  :group 'ledger-faces)

(defface ledger-font-pending-face
    `((t :foreground "#cb4b16" :weight normal ))
  "Default face for pending (!) transactions"
  :group 'ledger-faces)

(defface ledger-font-other-face
    `((t :foreground "yellow" ))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-posting-account-face
    `((t :foreground "#268bd2" ))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-amount-face
    `((t :foreground "yellow" ))
  "Face for Ledger amounts"
  :group 'ledger-faces)

(defface ledger-occur-folded-face
    `((t :foreground "grey70" :invisible t ))
  "Default face for Ledger occur mode hidden transactions"
  :group 'ledger-faces)

(defface ledger-occur-xact-face
    `((t :background "#eee8d5" ))
  "Default face for Ledger occur mode shown transactions"
  :group 'ledger-faces)

(defface ledger-font-comment-face
    `((t :foreground "#93a1a1" :slant italic))
  "Face for Ledger comments"
  :group 'ledger-faces)

(defface ledger-font-reconciler-uncleared-face
    `((t :foreground "#dc322f" :weight bold ))
  "Default face for uncleared transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-reconciler-cleared-face
    `((t :foreground "#657b83" :weight normal ))
  "Default face for cleared (*) transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-reconciler-pending-face
    `((t :foreground "#cb4b16" :weight normal ))
  "Default face for pending (!) transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-report-clickable-face
    `((t :foreground "#cb4b16" :weight normal ))
  "Default face for pending (!) transactions in the reconcile window"
  :group 'ledger-faces)


(defvar ledger-font-lock-keywords
  '(("^[0-9]+[-/.=][-/.=0-9]+\\s-\\!\\s-+\\(([^)]+)\\s-+\\)?\\([^*].+?\\)\\(\\(        ;\\|  ;\\|$\\)\\)" 2 'ledger-font-pending-face)
    ("^[0-9]+[-/.=][-/.=0-9]+\\s-\\*\\s-+\\(([^)]+)\\s-+\\)?\\([^*].+?\\)\\(\\(        ;\\|  ;\\|$\\)\\)" 2 'ledger-font-cleared-face)
    ("^[0-9]+[-/.=][-/.=0-9]+\\s-+\\(([^)]+)\\s-+\\)?\\([^*].+?\\)\\(\\(        ;\\|  ;\\|$\\)\\)" 2 'ledger-font-uncleared-face)
    ("^\\s-+\\([*]\\s-*\\)?\\(\\([[(]\\)?[^*:
        ]+?:\\([^]);
        ]\\|\\s-\\)+?\\([])]\\)?\\)\\(    \\|  \\|$\\)"
     2 'ledger-font-posting-account-face) ; works
    ("\\(       \\|  \\|^\\)\\(;.*\\)" 2 'ledger-font-comment-face)  ; works
    ("^\\([~=].+\\)" 1 ledger-font-other-face)
    ("^\\([A-Za-z]+ .+\\)" 1 ledger-font-other-face))
  "Expressions to highlight in Ledger mode.")


(provide 'ldg-fonts)

;;; ldg-fonts.el ends here
