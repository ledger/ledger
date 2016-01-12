;;; ledger-exec.el --- Helper code for use with the "ledger" command-line tool

;; Copyright (C) 2003-2016 John Wiegley (johnw AT gnu DOT org)

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
;; Code for executing ledger synchronously.

;;; Code:

(defvar ledger-buf)

(defconst ledger-version-needed "3.0.0"
  "The version of ledger executable needed for interactive features.")

(defvar ledger-works nil
  "Flag showing whether the ledger binary can support `ledger-mode' interactive features.")

(defgroup ledger-exec nil
  "Interface to the Ledger command-line accounting program."
  :group 'ledger)

(defcustom ledger-mode-should-check-version t
  "Should Ledger-mode verify that the executable is working?"
  :type 'boolean
  :group 'ledger-exec)

(defcustom ledger-binary-path "ledger"
  "Path to the ledger executable."
  :type 'file
  :group 'ledger-exec)

(defun ledger-exec-handle-error (ledger-output)
  "Deal with ledger errors contained in LEDGER-OUTPUT."
  (with-current-buffer (get-buffer-create "*Ledger Error*")
    (insert-buffer-substring ledger-output)
    (view-mode)
    (setq buffer-read-only t)))

(defun ledger-exec-success-p (ledger-output-buffer)
  "Return t if the ledger output in LEDGER-OUTPUT-BUFFER is successful."
  (with-current-buffer ledger-output-buffer
    (goto-char (point-min))
    (if (and (> (buffer-size) 1) (looking-at (regexp-quote "While")))
        nil  ;; failure, there is an error starting with "While"
      ledger-output-buffer)))

(defun ledger-exec-ledger (input-buffer &optional output-buffer &rest args)
  "Run Ledger using INPUT-BUFFER and optionally capturing output in OUTPUT-BUFFER with ARGS."
  (if (null ledger-binary-path)
      (error "The variable `ledger-binary-path' has not been set")
    (let ((buf (or input-buffer (current-buffer)))
          (outbuf (or output-buffer
                      (generate-new-buffer " *ledger-tmp*"))))
      (with-current-buffer buf
        (let ((coding-system-for-write 'utf-8)
              (coding-system-for-read 'utf-8))
          (apply #'call-process-region
                 (append (list (point-min) (point-max)
                               ledger-binary-path nil outbuf nil "-f" "-")
                         args)))
        (if (ledger-exec-success-p outbuf)
            outbuf
          (ledger-exec-handle-error outbuf))))))

(defun ledger-version-greater-p (needed)
  "Verify the ledger binary is usable for `ledger-mode' (version greater than NEEDED)."
  (let ((buffer ledger-buf)
        (version-strings '()))
    (with-temp-buffer
      (when (ledger-exec-ledger (current-buffer) (current-buffer) "--version")
        (goto-char (point-min))
        (delete-horizontal-space)
        (setq version-strings (split-string
                               (buffer-substring-no-properties (point)
                                                               (point-max))))
        (if (and (string-match (regexp-quote "Ledger") (car version-strings))
                 (or (string= needed (cadr version-strings))
                     (string< needed (cadr version-strings))))
            t ;; success
          nil))))) ;;failure

(defun ledger-check-version ()
  "Verify that ledger works and is modern enough."
  (interactive)
  (if ledger-mode-should-check-version
      (if (setq ledger-works (ledger-version-greater-p ledger-version-needed))
          (message "Good Ledger Version")
        (message "Bad Ledger Version"))))

(provide 'ledger-exec)

;;; ledger-exec.el ends here
