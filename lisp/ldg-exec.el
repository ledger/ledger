;;; ldg-exec.el --- Helper code for use with the "ledger" command-line tool

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

(defconst ledger-version-needed "3.0.0"
  "The version of ledger executable needed for interactive features")

(defvar ledger-works nil
  "Flag showing whether the ledger binary can support ledger-mode interactive features")

(defgroup ledger-exec nil
  "Interface to the Ledger command-line accounting program."
  :group 'ledger)

(defcustom ledger-binary-path "ledger"
  "Path to the ledger executable."
  :type 'file
  :group 'ledger)

(defun ledger-exec-ledger (input-buffer &optional output-buffer &rest args)
  "Run Ledger."
  (if (null ledger-binary-path)
      (error "The variable `ledger-binary-path' has not been set"))
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
      outbuf)))

(defun ledger-exec-read (&optional input-buffer &rest args)
  (with-current-buffer
      (apply #'ledger-exec-ledger input-buffer nil "emacs" args)
    (goto-char (point-min))
    (prog1
        (read (current-buffer))
      (kill-buffer (current-buffer)))))

(defun ledger-version-greater-p (needed)
  "verify the ledger binary is usable for ledger-mode"
  (let ((buffer ledger-buf)
        (version-strings '())
	(version-number))
    (with-temp-buffer
      (ledger-exec-ledger buffer (current-buffer) "--version")
      (goto-char (point-min))
      (delete-horizontal-space)
      (setq version-strings (split-string
			     (buffer-substring-no-properties (point)
							     (+ (point) 12))))
      (if (and (string-match (regexp-quote "Ledger") (car version-strings))
	       (or (string= needed (car (cdr version-strings)))
		   (string< needed (car (cdr version-strings)))))
	  t
	  nil))))

(defun ledger-check-version ()
  (interactive)
  (setq ledger-works (ledger-version-greater-p ledger-version-needed))
  (if ledger-works
      (message "Good Ledger Version")
      (message "Bad Ledger Version")))

(provide 'ldg-exec)
