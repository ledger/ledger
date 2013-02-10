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
(defvar ledger-bufs nil)
(defvar ledger-acct nil)

(defcustom ledger-recon-buffer-name "*Reconcile*"
  "Name to use for reconciliation window"
  :group 'ledger)

(defcustom ledger-fold-on-reconcile t 
  "if t, limit transactions shown in main buffer to those
   matching the reconcile regex"
  :type 'boolean
  :group 'ledger)

(defcustom ledger-buffer-tracks-reconcile-buffer t
  "if t, then when the cursor is moved to a new xact in the recon
   window, then that transaction will be shown in its source
   buffer."
  :type 'boolean
  :group 'ledger)

(defcustom ledger-reconcile-force-window-bottom nil
  "If t make the reconcile window appear along the bottom of the
   register window and resize"
  :type 'boolean
  :group 'ledger)

(defun ledger-display-balance ()
  "Calculate the cleared balance of the account being reconciled"
  (interactive)
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

(defun ledger-reconcile-get-buffer (where)
  (if (bufferp (car where))
      (car where)
      (error "buffer not set")))


(defun ledger-reconcile-toggle ()
  (interactive)
  (let ((where (get-text-property (point) 'where))
        (account ledger-acct)
        (inhibit-read-only t)
        cleared)
    (when (ledger-reconcile-get-buffer where)
      (with-current-buffer (ledger-reconcile-get-buffer where)
	(goto-char (cdr where))
	(setq cleared (ledger-toggle-current)))
	;remove the existing face and add the new face
      (remove-text-properties (line-beginning-position)
			      (line-end-position)
			      (list 'face))
      (if cleared
	  (add-text-properties (line-beginning-position)
			       (line-end-position)
			       (list 'face 'ledger-font-reconciler-cleared-face ))
	  (add-text-properties (line-beginning-position)
			       (line-end-position)
			       (list 'face 'ledger-font-reconciler-uncleared-face ))))
    (forward-line)
    (ledger-display-balance)))

(defun ledger-reconcile-new-account (account)
  (interactive "sAccount to reconcile: ")
  (set (make-local-variable 'ledger-acct) account)
  (let ((buf (current-buffer)))
    (if ledger-fold-on-reconcile
	(ledger-occur-change-regex account ledger-buf))
    (set-buffer buf)
    (ledger-reconcile-refresh)))

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
  (let ((buf (get-buffer ledger-recon-buffer-name)))
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
    (when (ledger-reconcile-get-buffer where)
      (with-current-buffer (ledger-reconcile-get-buffer where)
        (goto-char (cdr where))
        (ledger-delete-current-entry))
      (let ((inhibit-read-only t))
        (goto-char (line-beginning-position))
        (delete-region (point) (1+ (line-end-position)))
        (set-buffer-modified-p t)))))

(defun ledger-reconcile-visit (&optional come-back)
  (interactive)
  (progn
    (beginning-of-line)
    (let* ((where (get-text-property (1+ (point)) 'where))
	   (target-buffer (if where 
			      (ledger-reconcile-get-buffer where)
			      nil))
	   (cur-buf (current-buffer)))
      (when target-buffer
	(switch-to-buffer-other-window target-buffer)
	(goto-char (cdr where))
	(recenter)
	(ledger-highlight-xact-under-point)
	(if come-back
	    (switch-to-buffer-other-window cur-buf))))))

(defun ledger-reconcile-save ()
  (interactive)
  (dolist (buf (cons ledger-buf ledger-bufs))
    (with-current-buffer buf
      (save-buffer)))
  (set-buffer-modified-p nil)
  (ledger-display-balance))

(defun ledger-reconcile-quit ()
  (interactive)
  (let ((buf ledger-buf)
        (reconcile-buf (current-buffer)))
    (with-current-buffer ledger-buf
      (remove-hook 'after-save-hook 'ledger-reconcile-refresh-after-save t))

    ;Make sure you delete the window before you delete the buffer,
    ;otherwise, madness ensues
    (delete-window (get-buffer-window reconcile-buf))
    (kill-buffer (reconcile-buf))
    (if ledger-fold-on-reconcile
	(ledger-occur-quit-buffer buf))))

(defun ledger-marker-where-xact-is (emacs-xact)
  "find the position of the xact in the ledger-buf buffer using
   the emacs output from ledger, return the buffer and a marker
   to the beginning of the xact in that buffer"
  (let ((buf (if (is-stdin (nth 0 emacs-xact))
		 ledger-buf
		 (find-file-noselect (nth 0 emacs-xact)))))
    (with-current-buffer buf 
      (cons
       buf
       (save-excursion
	 (if ledger-clear-whole-entries
	     (goto-line (nth 1 emacs-xact))
	     (goto-line (nth 0 (nth 5 emacs-xact))))
	 (point-marker))))))

(defun ledger-do-reconcile ()
  "get the uncleared transactions in the account and display them
   in the *Reconcile* buffer"
  (let* ((buf ledger-buf)
         (account ledger-acct)
         (items
          (with-temp-buffer 
	    (ledger-exec-ledger buf (current-buffer) 
				"--uncleared" "--real" "emacs" account)
	    (goto-char (point-min))
	    (unless (eobp)
	      (unless (looking-at "(")
		(error (buffer-string)))
	      (read (current-buffer))))))
    (if (> (length items) 0)
	(progn
	  (dolist (item items)
	    (let ((index 1))
	      (dolist (xact (nthcdr 5 item))
		(let ((beg (point))  
		      (where (ledger-marker-where-xact-is item)))
		  (insert (format "%s %-4s %-30s %-30s %15s\n"
				  (format-time-string "%Y/%m/%d" (nth 2 item))
				  (if (nth 3 item)
				      (nth 3 item)
				      "")
				  (nth 4 item) (nth 1 xact) (nth 2 xact)))
		  (if (nth 3 xact)
		      (set-text-properties beg (1- (point))
					   (list 'face 'ledger-font-reconciler-cleared-face 
						 'where where))
		      (set-text-properties beg (1- (point))
					   (list 'face 'ledger-font-reconciler-uncleared-face 
						 'where where))))
		(setq index (1+ index)))))
	  (goto-char (point-max))
	  (delete-char -1))
	(insert (concat "There are no uncleared entries for " account)))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (toggle-read-only t)

    ; this next piece of code ensures that the last of the visible
    ; transactions in the ledger buffer is at the bottom of the
    ; main window.  The key to this is to ensure the window is selected
    ; when the buffer point is moved and recentered.  If they aren't
    ; strange things happen.
    
    (let 
	((recon-window (get-buffer-window (get-buffer ledger-recon-buffer-name))))
      (fit-window-to-buffer recon-window)
      (with-current-buffer buf
	(select-window (get-buffer-window buf))
	(goto-char (point-max))
	(recenter -1))
      
      (select-window recon-window)
      (add-hook 'post-command-hook 'ledger-reconcile-track-xact nil t)
      (ledger-reconcile-visit t))))

(defun ledger-reconcile-track-xact ()
  (if (member this-command (list 'next-line
				 'previous-line
				 'mouse-set-point
				 'ledger-reconcile-toggle))
      (if ledger-buffer-tracks-reconcile-buffer
	  (save-excursion
	    (ledger-reconcile-visit t)))))

(defun ledger-reconcile (account)
  (interactive "sAccount to reconcile: ")
  (let ((buf (current-buffer))
        (rbuf (get-buffer ledger-recon-buffer-name)))
    (if rbuf
	(progn
	  (quit-window (get-buffer-window rbuf))
	  (kill-buffer rbuf)))
    (add-hook 'after-save-hook 'ledger-reconcile-refresh-after-save nil t)
    (if ledger-fold-on-reconcile
	(ledger-occur-mode account buf))

    (with-current-buffer
	(if ledger-reconcile-force-window-bottom
            ;create the *Reconcile* window directly below the ledger
            ;buffer.
	    (progn
	      (set-window-buffer
	       (split-window (get-buffer-window (current-buffer)) nil nil) 
	       (get-buffer-create ledger-recon-buffer-name))
	      (get-buffer ledger-recon-buffer-name))
	    (pop-to-buffer (get-buffer-create ledger-recon-buffer-name)))
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
     (define-key map [(control ?l)] 'ledger-reconcile-refresh)
     (define-key map [? ] 'ledger-reconcile-toggle)
     (define-key map [?a] 'ledger-reconcile-add)
     (define-key map [?d] 'ledger-reconcile-delete)
     (define-key map [?g] 'ledger-reconcile-new-account)
     (define-key map [?n] 'next-line)
     (define-key map [?p] 'previous-line)
     (define-key map [?s] 'ledger-reconcile-save)
     (define-key map [?q] 'ledger-reconcile-quit)
     (define-key map [?b] 'ledger-display-balance)
     
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
     (define-key map [menu-bar ldg-recon-menu bal] '("Show Cleared Balance" . ledger-display-balance))
     (define-key map [menu-bar ldg-recon-menu sep4] '("--"))
     (define-key map [menu-bar ldg-recon-menu rna] '("Reconcile New Account" . ledger-reconcile-new-account))
     (define-key map [menu-bar ldg-recon-menu ref] '("Refresh" . ledger-reconcile-refresh))
     (define-key map [menu-bar ldg-recon-menu sav] '("Save" . ledger-reconcile-save))

     (use-local-map map)))

(provide 'ldg-reconcile)