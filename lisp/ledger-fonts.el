;;; ledger-fonts.el --- Helper code for use with the "ledger" command-line tool

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
;; All of the faces for ledger mode are defined here.

;;; Code:

(require 'ledger-regex)

(defgroup ledger-faces nil "Ledger mode highlighting" :group 'ledger)
(defface ledger-font-payee-uncleared-face
  `((t :foreground "#dc322f" :weight bold ))
  "Default face for Ledger"
  :group 'ledger-faces)

(defface ledger-font-payee-cleared-face
  `((t :inherit ledger-font-other-face))
  "Default face for cleared (*) transactions"
  :group 'ledger-faces)

(defface ledger-font-xact-highlight-face
  `((t :inherit ledger-occur-xact-face))
  "Default face for transaction under point"
  :group 'ledger-faces)

(defface ledger-font-pending-face
  `((t :foreground "#cb4b16" :weight normal ))
  "Default face for pending (!) transactions"
  :group 'ledger-faces)

(defface ledger-font-other-face
  `((t :foreground "#657b83" :weight normal))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-posting-account-face
  `((t :foreground "#268bd2" ))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-account-cleared-face
  `((t :inherit ledger-font-other-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-account-pending-face
  `((t :inherit ledger-font-pending-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-amount-face
  `((t :foreground "#cb4b16" ))
  "Face for Ledger amounts"
  :group 'ledger-faces)

(defface ledger-font-posting-date-face
  `((t :foreground "#cb4b16" ))
  "Face for Ledger dates"
  :group 'ledger-faces)

(defface ledger-occur-narrowed-face
  `((t :foreground "grey70" :invisible t ))
  "Default face for Ledger occur mode hidden transactions"
  :group 'ledger-faces)

(defface ledger-occur-xact-face
  `((((background dark)) :background "#1a1a1a" )
    (t :background "#eee8d5" ))
  "Default face for Ledger occur mode shown transactions"
  :group 'ledger-faces)

(defface ledger-font-comment-face
  `((t :foreground "#93a1a1" :slant italic))
  "Face for Ledger comments"
  :group 'ledger-faces)

(defface ledger-font-reconciler-uncleared-face
  `((t :inherit ledger-font-payee-uncleared-face))
  "Default face for uncleared transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-reconciler-cleared-face
  `((t :inherit ledger-font-other-face))
  "Default face for cleared (*) transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-reconciler-pending-face
  `((t :inherit ledger-font-pending-face))
  "Default face for pending (!) transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-report-clickable-face
  `((t :foreground "#cb4b16" :weight normal ))
  "Default face for pending (!) transactions in the reconcile window"
  :group 'ledger-faces)


(defvar ledger-font-lock-keywords
  `( ;; (,ledger-other-entries-regex 1
    ;;          ledger-font-other-face)
    (,ledger-comment-regex 0
                           'ledger-font-comment-face)
    (,ledger-amount-regex 0
                          'ledger-font-posting-amount-face)
    (,ledger-multiline-comment-regex 0 'ledger-font-comment-face)
    (,ledger-payee-pending-regex 2
                                 'ledger-font-payee-pending-face) ; Works
    (,ledger-payee-cleared-regex 2
                                 'ledger-font-payee-cleared-face) ; Works
    (,ledger-payee-uncleared-regex 2
                                   'ledger-font-payee-uncleared-face) ; Works
    (,ledger-account-cleared-regex 2
                                   'ledger-font-posting-account-cleared-face) ; Works
    (,ledger-account-pending-regex 2
                                   'ledger-font-posting-account-pending-face) ; Works
    (,ledger-account-any-status-regex 2
                                      'ledger-font-posting-account-face) ; Works
    (,ledger-other-entries-regex 1
                                 'ledger-font-other-face))
  "Expressions to highlight in Ledger mode.")


(provide 'ledger-fonts)

;;; ledger-fonts.el ends here
