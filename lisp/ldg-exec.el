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

(provide 'ldg-exec)
