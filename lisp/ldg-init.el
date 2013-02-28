;;; ldg-init.el --- Helper code for use with the "ledger" command-line tool

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
;; Determine the ledger environment

(defcustom ledger-init-file-name "~/.ledgerrc"
  "Location of the ledger initialization file. nil if you don't have one"
  :group 'ledger-exec)

(defvar ledger-environment-alist nil)

(defun ledger-init-parse-initialization (file)
  (with-current-buffer file
    (setq ledger-environment-alist nil)
    (goto-char (point-min))
    (while (re-search-forward "^--.+?\\($\\|[ ]\\)" nil t )
      (let ((matchb (match-beginning 0)) ;; save the match data, string-match stomp on it
	    (matche (match-end 0)))
	(end-of-line)
	(setq ledger-environment-alist 
	      (append ledger-environment-alist
		      (list (cons (let ((flag (buffer-substring (+ 2 matchb) matche)))
				    (if (string-match "[ \t\n\r]+\\'" flag)
					(replace-match "" t t flag)
					flag))
				  (let ((value (buffer-substring  matche (point) )))
				    (if (> (length value) 0)
					value
					t))))))))
    ledger-environment-alist))

(defun ledger-init-load-init-file ()
  (interactive)
  (let ((init-base-name (file-name-nondirectory ledger-init-file-name)))
    (if (get-buffer init-base-name) ;; init file already loaded, parse it and leave it
	(ledger-init-parse-initialization init-base-name)
       (if (and  ;; init file not loaded, load, parse and kill
	    ledger-init-file-name
	    (file-exists-p ledger-init-file-name)
	    (file-readable-p ledger-init-file-name))
	   (progn 
	     (find-file-noselect ledger-init-file-name)
	     (ledger-init-parse-initialization init-base-name)
	     (kill-buffer init-base-name))))))



(provide 'ldg-init)

;;; ldg-init.el ends here
