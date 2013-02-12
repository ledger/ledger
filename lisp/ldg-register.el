;;; ldg-register.el --- Helper code for use with the "ledger" command-line tool

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

(require 'ldg-post)
(require 'ldg-state)

(defgroup ledger-register nil
  ""
  :group 'ledger)

(defcustom ledger-register-date-format "%m/%d/%y"
  "*The date format used for ledger register reports."
  :type 'string
  :group 'ledger-register)

(defcustom ledger-register-line-format "%s %-30.30s %-25.25s %15s\n"
  "*The date format used for ledger register reports."
  :type 'string
  :group 'ledger-register)

(defface ledger-register-pending-face
    '((((background light)) (:weight bold))
      (((background dark)) (:weight bold)))
  "Face used to highlight pending entries in a register report."
  :group 'ledger-register)

(defun ledger-register-render (data-buffer posts)
  (dolist (post posts)
    (let ((index 1))
      (dolist (xact (nthcdr 5 post))
        (let ((beg (point))
              (where
               (with-current-buffer data-buffer
                 (cons
                  (nth 0 post)
                  (if ledger-clear-whole-transactions
                      (save-excursion
                        (goto-line (nth 1 post))
                        (point-marker))
		      (save-excursion
			(goto-line (nth 0 xact))
			(point-marker)))))))
          (insert (format ledger-register-line-format
                          (format-time-string ledger-register-date-format
                                              (nth 2 post))
                          (nth 4 post) (nth 1 xact) (nth 2 xact)))
          (if (nth 3 xact)
              (set-text-properties beg (1- (point))
                                   (list 'face 'ledger-register-pending-face
                                         'where where))
	      (set-text-properties beg (1- (point))
				   (list 'where where))))
        (setq index (1+ index)))))
  (goto-char (point-min))
  )

(defun ledger-register-generate (&optional data-buffer &rest args)
  (let ((buf (or data-buffer (current-buffer))))
    (with-current-buffer (get-buffer-create "*ledger-register*")
      (let ((pos (point))
            (inhibit-read-only t))
        (erase-buffer)
        (ledger-register-render buf (apply #'ledger-exec-read buf args))
        (goto-char pos))
      (set-buffer-modified-p nil)
      (toggle-read-only t)
      (display-buffer (current-buffer) t))))

(provide 'ldg-register)
