;;; ldg-reconcile.el --- Helper code for use with the "ledger" command-line tool

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

;; Reconcile mode

(defvar ledger-buf nil)
(defvar ledger-acct nil)

(defun ledger-display-balance ()
  "Calculate the cleared balance of the account being reconciled"
  (let ((buffer ledger-buf)
        (account ledger-acct))
    (with-temp-buffer
      (ledger-exec-ledger buffer (current-buffer) "-C" "balance"  account)
      (goto-char (1- (point-max)))
      (goto-char (line-beginning-position))
      (delete-horizontal-space)
      (message "Cleared balance = %s"
	       (buffer-substring-no-properties (point)
					       (line-end-position))))))

(defun is-stdin (file)
  "True if ledger file is standard input"
  (or
   (equal file "")
   (equal file "<stdin>")
   (equal file "/dev/stdin")))

(defun ledger-reconcile-toggle ()
  (interactive)
  (let ((where (get-text-property (point) 'where))
        (account ledger-acct)
        (inhibit-read-only t)
        cleared)
    (when (is-stdin (car where))
      (with-current-buffer ledger-buf
	(goto-char (cdr where))
	(setq cleared (ledger-toggle-current-entry)))
      (if cleared
	  (add-text-properties (line-beginning-position)
			       (line-end-position)
			       (list 'face 'bold))
	  (remove-text-properties (line-beginning-position)
				  (line-end-position)
				  (list 'face))))
    (forward-line)
    (ledger-display-balance)))

(defun ledger-reconcile-refresh ()
  (interactive)
  (let ((inhibit-read-only t)
        (line (count-lines (point-min) (point))))
    (erase-buffer)
    (ledger-do-reconcile)
    (set-buffer-modified-p t)
    (goto-char (point-min))
    (forward-line line)))

(defun ledger-reconcile-refresh-after-save ()
  (let ((buf (get-buffer "*Reconcile*")))
    (if buf
        (with-current-buffer buf
          (ledger-reconcile-refresh)
          (set-buffer-modified-p nil)))))

(defun ledger-reconcile-add ()
  (interactive)
  (with-current-buffer ledger-buf
    (call-interactively #'ledger-add-entry))
  (ledger-reconcile-refresh))

(defun ledger-reconcile-delete ()
  (interactive)
  (let ((where (get-text-property (point) 'where)))
    (when (is-stdin (car where))
      (with-current-buffer ledger-buf
        (goto-char (cdr where))
        (ledger-delete-current-entry))
      (let ((inhibit-read-only t))
        (goto-char (line-beginning-position))
        (delete-region (point) (1+ (line-end-position)))
        (set-buffer-modified-p t)))))

(defun ledger-reconcile-visit ()
  (interactive)
  (let ((where (get-text-property (point) 'where)))
    (when (is-stdin (car where))
      (switch-to-buffer-other-window ledger-buf)
      (goto-char (cdr where)))))

(defun ledger-reconcile-save ()
  (interactive)
  (with-current-buffer ledger-buf
    (save-buffer))
  (set-buffer-modified-p nil)
  (ledger-display-balance))

(defun ledger-reconcile-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun ledger-reconcile-finish ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((where (get-text-property (point) 'where))
            (face  (get-text-property (point) 'face)))
        (if (and (eq face 'bold)
                 (when (is-stdin (car where))))
            (with-current-buffer ledger-buf
              (goto-char (cdr where))
              (ledger-toggle-current 'cleared))))
      (forward-line 1)))
  (ledger-reconcile-save))

(defun ledger-do-reconcile ()
  "get the uncleared transactions in the account and display them in the *Reconcile* buffer"
    (let* ((buf ledger-buf)
         (account ledger-acct)
         (items
          (with-temp-buffer
	    (ledger-exec-ledger buf (current-buffer) "--uncleared" "--real"
                                      "emacs" account)
	    (goto-char (point-min))
	    (unless (eobp)
	      (unless (looking-at "(")
		(error (buffer-string)))
	      (read (current-buffer)))))) 
      (dolist (item items)
	(let ((index 1))
	  (dolist (xact (nthcdr 5 item))
	    (let ((beg (point))
		  (where
		   (with-current-buffer buf
		     (cons
		      (nth 0 item)
		      (if ledger-clear-whole-entries
			  (save-excursion
			    (goto-line (nth 1 item))
			    (point-marker))
			  (save-excursion
			    (goto-line (nth 0 xact))
			    (point-marker)))))))
	      (insert (format "%s %-4s %-30s %-30s %15s\n"
			      (format-time-string "%Y/%m/%d" (nth 2 item))
			      (nth 3 item)
			      (nth 4 item) (nth 1 xact) (nth 2 xact)))
	      (if (nth 3 xact)
		  (set-text-properties beg (1- (point))
				       (list 'face 'bold
					     'where where))
		  (set-text-properties beg (1- (point))
				       (list 'where where))))
	    (setq index (1+ index)))))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (toggle-read-only t)))


(defun ledger-reconcile (account)
  (interactive "sAccount to reconcile: ")
  (let ((buf (current-buffer))
        (rbuf (get-buffer "*Reconcile*")))
    (if rbuf
        (kill-buffer rbuf))
    (add-hook 'after-save-hook 'ledger-reconcile-refresh-after-save)
    (with-current-buffer
        (pop-to-buffer (get-buffer-create "*Reconcile*"))
      (ledger-reconcile-mode)
      (set (make-local-variable 'ledger-buf) buf)
      (set (make-local-variable 'ledger-acct) account)
      (ledger-do-reconcile))))

(defvar ledger-reconcile-mode-abbrev-table)

(define-derived-mode ledger-reconcile-mode text-mode "Reconcile"
  "A mode for reconciling ledger entries."
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?m)] 'ledger-reconcile-visit)
    (define-key map [return] 'ledger-reconcile-visit)
    (define-key map [(control ?c) (control ?c)] 'ledger-reconcile-finish)
    (define-key map [(control ?x) (control ?s)] 'ledger-reconcile-save)
    (define-key map [(control ?l)] 'ledger-reconcile-refresh)
    (define-key map [? ] 'ledger-reconcile-toggle)
    (define-key map [?a] 'ledger-reconcile-add)
    (define-key map [?d] 'ledger-reconcile-delete)
    (define-key map [?n] 'next-line)
    (define-key map [?p] 'previous-line)
    (define-key map [?s] 'ledger-reconcile-save)
    (define-key map [?q] 'ledger-reconcile-quit)

    (define-key map [menu-bar] (make-sparse-keymap "ldg-recon-menu"))
    (define-key map [menu-bar ldg-recon-menu] (cons "Reconcile" map))
    (define-key map [menu-bar ldg-recon-menu qui] '("Quit" . ledger-reconcile-quit))
    (define-key map [menu-bar ldg-recon-menu sep1] '("--"))
    (define-key map [menu-bar ldg-recon-menu pre] '("Previous Entry" . previous-line))
    (define-key map [menu-bar ldg-recon-menu vis] '("Visit Entry" . ledger-reconcile-visit))
    (define-key map [menu-bar ldg-recon-menu nex] '("Next Entry" . next-line))
    (define-key map [menu-bar ldg-recon-menu sep2] '("--"))
    (define-key map [menu-bar ldg-recon-menu del] '("Delete Entry" . ledger-reconcile-delete))
    (define-key map [menu-bar ldg-recon-menu add] '("Add Entry" . ledger-reconcile-add))
    (define-key map [menu-bar ldg-recon-menu tog] '("Toggle Entry" . ledger-reconcile-toggle))
    (define-key map [menu-bar ldg-recon-menu sep3] '("--"))
    (define-key map [menu-bar ldg-recon-menu ref] '("Refresh" . ledger-reconcile-refresh))
    (define-key map [menu-bar ldg-recon-menu sav] '("Save" . ledger-reconcile-save))

    (use-local-map map)))

(provide 'ldg-reconcile)