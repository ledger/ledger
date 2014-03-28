;;; ledger-hideshow.el --- Provides code folding support for ledger-mode.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides code folding support for ledger-mode using hideshow, which
;; ships with Emacs 20+.
;;
;; The hideshow commands are available from the menu bar or with the
;; 'C-c @' prefix.  See the documentation for the hideshow package for
;; more information.

;;; Code:

(defvar ledger--transaction-start-re (rx bol (any digit "~" "="))
  "Regex matching the start of a transaction line.")

(defun ledger--hs-forward (&optional n)
  "Forward motion command for ledger-mode's hideshow support.
Argument N is provided for compatibility and is not used."
  (forward-line 1)
  (or (when (search-forward-regexp ledger--transaction-start-re nil t)
        (forward-line -1)
        (goto-char (line-end-position))
        t)
      (goto-char (point-max))))

(eval-after-load 'hideshow
  '(add-to-list 'hs-special-modes-alist
                `(ledger-mode
                  ,ledger--transaction-start-re
                  nil
                  nil
                  ledger--hs-forward
                  nil)))

(provide 'ledger-hideshow)

;;; ledger-hideshow.el ends here
