;;; ledger-reconcile.el --- Helper code for use with the "ledger" command-line tool

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


;;; Commentary:
;; Code to handle reconciling Ledger files wiht outside sources

;;; Code:

(defvar ledger-buf nil)
(defvar ledger-bufs nil)
(defvar ledger-acct nil)
(defvar ledger-target nil)

(defgroup ledger-reconcile nil
	"Options for Ledger-mode reconciliation"
  :group 'ledger)

(defcustom ledger-recon-buffer-name "*Reconcile*"
  "Name to use for reconciliation window."
  :group 'ledger-reconcile)

(defcustom ledger-narrow-on-reconcile t
  "If t, limit transactions shown in main buffer to those matching the reconcile regex."
  :type 'boolean
  :group 'ledger-reconcile)

(defcustom ledger-buffer-tracks-reconcile-buffer t
  "If t, then when the cursor is moved to a new xact in the recon window.
Then that transaction will be shown in its source buffer."
  :type 'boolean
  :group 'ledger-reconcile)

(defcustom ledger-reconcile-force-window-bottom nil
  "If t make the reconcile window appear along the bottom of the register window and resize."
  :type 'boolean
  :group 'ledger-reconcile)

(defcustom ledger-reconcile-toggle-to-pending t
  "If true then toggle between uncleared and pending.
reconcile-finish will mark all pending posting cleared."
	:type 'boolean
	:group 'ledger-reconcile)

(defcustom ledger-reconcile-default-date-format "%Y/%m/%d"
  "Default date format for the reconcile buffer"
  :type 'string
  :group 'ledger-reconcile)

(defcustom ledger-reconcile-target-prompt-string "Target amount for reconciliation "
  "Default prompt for recon target prompt"
  :type 'string
  :group 'ledger-reconcile)

(defcustom ledger-reconcile-sort-key "(date)"
	"Default key for sorting reconcile buffer. For no sorting by default, use '(0)'."
	:type 'string
	:group 'ledger-reconcile)

(defun ledger-reconcile-get-cleared-or-pending-balance (buffer account)
  "Calculate the cleared or pending balance of the account."

  ;; these vars are buffer local, need to hold them for use in the
  ;; temp buffer below

  (with-temp-buffer
    ;; note that in the line below, the --format option is
    ;; separated from the actual format string.  emacs does not
    ;; split arguments like the shell does, so you need to
    ;; specify the individual fields in the command line.
    (if (ledger-exec-ledger buffer (current-buffer)
														"balance" "--limit" "cleared or pending" "--empty" "--collapse"
														"--format" "%(display_total)" account)
				(ledger-split-commodity-string
				 (buffer-substring-no-properties (point-min) (point-max))))))

(defun ledger-display-balance ()
  "Display the cleared-or-pending balance.
And calculate the target-delta of the account being reconciled."
  (interactive)
  (let* ((pending (ledger-reconcile-get-cleared-or-pending-balance ledger-buf ledger-acct)))
    (when pending
			(if ledger-target
					(message "Pending balance: %s,   Difference from target: %s"
									 (ledger-commodity-to-string pending)
									 (ledger-commodity-to-string (-commodity ledger-target pending)))
					(message "Pending balance: %s"
									 (ledger-commodity-to-string pending))))))

(defun is-stdin (file)
  "True if ledger FILE is standard input."
  (or
   (equal file "")
   (equal file "<stdin>")
   (equal file "/dev/stdin")))

(defun ledger-reconcile-get-buffer (where)
  "Return a buffer from WHERE the transaction is."
  (if (bufferp (car where))
      (car where)
      (error "Function ledger-reconcile-get-buffer: Buffer not set")))

(defun ledger-reconcile-toggle ()
  "Toggle the current transaction, and mark the recon window."
  (interactive)
  (beginning-of-line)
  (let ((where (get-text-property (point) 'where))
        (inhibit-read-only t)
        status)
    (when (ledger-reconcile-get-buffer where)
      (with-current-buffer (ledger-reconcile-get-buffer where)
				(ledger-goto-line (cdr where))
				(forward-char)
				(setq status (ledger-toggle-current (if ledger-reconcile-toggle-to-pending
																								'pending
																								'cleared))))
			;; remove the existing face and add the new face
      (remove-text-properties (line-beginning-position)
															(line-end-position)
															(list 'face))
      (cond ((eq status 'pending)
						 (add-text-properties (line-beginning-position)
																	(line-end-position)
																	(list 'face 'ledger-font-reconciler-pending-face )))
						((eq status 'cleared)
						 (add-text-properties (line-beginning-position)
																	(line-end-position)
																	(list 'face 'ledger-font-reconciler-cleared-face )))
						(t
						 (add-text-properties (line-beginning-position)
																	(line-end-position)
																	(list 'face 'ledger-font-reconciler-uncleared-face )))))
    (forward-line)
    (beginning-of-line)
    (ledger-display-balance)))

(defun ledger-reconcile-refresh ()
  "Force the reconciliation window to refresh.
Return the number of uncleared xacts found."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (prog1
				(ledger-do-reconcile ledger-reconcile-sort-key)
      (set-buffer-modified-p t))))

(defun ledger-reconcile-refresh-after-save ()
  "Refresh the recon-window after the ledger buffer is saved."
  (let ((curbuf (current-buffer))
				(curpoint (point))
				(recon-buf (get-buffer ledger-recon-buffer-name)))
    (when (buffer-live-p recon-buf)
      (with-current-buffer recon-buf
				(ledger-reconcile-refresh)
				(set-buffer-modified-p nil))
      (select-window  (get-buffer-window curbuf))
      (goto-char curpoint))))

(defun ledger-reconcile-add ()
  "Use ledger xact to add a new transaction."
  (interactive)
  (with-current-buffer ledger-buf
    (call-interactively #'ledger-add-transaction))
  (ledger-reconcile-refresh))

(defun ledger-reconcile-delete ()
  "Delete the transactions pointed to in the recon window."
  (interactive)
  (let ((where (get-text-property (point) 'where)))
    (when (ledger-reconcile-get-buffer where)
      (with-current-buffer (ledger-reconcile-get-buffer where)
        (ledger-goto-line (cdr where))
        (ledger-delete-current-transaction))
      (let ((inhibit-read-only t))
        (goto-char (line-beginning-position))
        (delete-region (point) (1+ (line-end-position)))
        (set-buffer-modified-p t)))))

(defun ledger-reconcile-visit (&optional come-back)
  "Recenter ledger buffer on transaction and COME-BACK if non-nil."
  (interactive)
  (progn
    (beginning-of-line)
    (let* ((where (get-text-property (1+ (point)) 'where))
					 (target-buffer (if where
															(ledger-reconcile-get-buffer where)
															nil))
					 (cur-win (get-buffer-window (get-buffer ledger-recon-buffer-name))))
      (when target-buffer
				(switch-to-buffer-other-window target-buffer)
				(ledger-goto-line (cdr where))
				(forward-char)
				(recenter)
				(ledger-highlight-xact-under-point)
				(forward-char -1)
				(if (and come-back cur-win)
						(select-window cur-win))))))

(defun ledger-reconcile-save ()
  "Save the ledger buffer."
  (interactive)
  (let ((curpoint (point)))
    (dolist (buf (cons ledger-buf ledger-bufs))
      (with-current-buffer buf
				(save-buffer)))
    (with-current-buffer (get-buffer ledger-recon-buffer-name)
      (set-buffer-modified-p nil)
      (ledger-display-balance)
      (goto-char curpoint)
      (ledger-reconcile-visit t))))

(defun ledger-reconcile-finish ()
  "Mark all pending posting or transactions as cleared.
Depends on ledger-reconcile-clear-whole-transactions, save the buffers
and exit reconcile mode"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((where (get-text-property (point) 'where))
            (face  (get-text-property (point) 'face)))
        (if (eq face 'ledger-font-reconciler-pending-face)
            (with-current-buffer (ledger-reconcile-get-buffer where)
              (ledger-goto-line (cdr where))
              (ledger-toggle-current 'cleared))))
      (forward-line 1)))
  (ledger-reconcile-save)
	(ledger-reconcile-quit))


(defun ledger-reconcile-quit ()
  "Quit the reconcile window without saving ledger buffer."
  (interactive)
  (let ((recon-buf (get-buffer ledger-recon-buffer-name))
				buf)
    (if recon-buf
				(with-current-buffer recon-buf
					(ledger-reconcile-quit-cleanup)
					(setq buf ledger-buf)
					;; Make sure you delete the window before you delete the buffer,
					;; otherwise, madness ensues
					(delete-window (get-buffer-window recon-buf))
					(kill-buffer recon-buf)
					(set-window-buffer (selected-window) buf)))))

(defun ledger-reconcile-quit-cleanup ()
  "Cleanup all hooks established by reconcile mode."
  (interactive)
  (let ((buf ledger-buf))
    (if (buffer-live-p buf)
				(with-current-buffer buf
					(remove-hook 'after-save-hook 'ledger-reconcile-refresh-after-save t)
					(when ledger-narrow-on-reconcile
						(ledger-occur-quit-buffer buf)
						(ledger-highlight-xact-under-point))))))

(defun ledger-marker-where-xact-is (emacs-xact posting)
  "Find the position of the EMACS-XACT in the `ledger-buf'.
POSTING is used in `ledger-clear-whole-transactions' is nil."
  (let ((buf (if (is-stdin (nth 0 emacs-xact))
								 ledger-buf
								 (find-file-noselect (nth 0 emacs-xact)))))
    (cons
     buf
     (if ledger-clear-whole-transactions
				 (nth 1 emacs-xact)  ;; return line-no of xact
				 (nth 0 posting))))) ;; return line-no of posting

(defun ledger-do-reconcile (&optional sort)
  "Return the number of uncleared transactions in the account and display them in the *Reconcile* buffer."
  (let* ((buf ledger-buf)
         (account ledger-acct)
				 (ledger-success nil)
				 (sort-by (if sort
											sort
											"(date)"))
         (xacts
          (with-temp-buffer
						(when (ledger-exec-ledger buf (current-buffer)
																			"--uncleared" "--real" "emacs" "--sort" sort-by account)
							(setq ledger-success t)
							(goto-char (point-min))
							(unless (eobp)
								(if (looking-at "(")
										(read (current-buffer)))))))) ;current-buffer is the *temp* created above
    (if (and ledger-success (> (length xacts) 0))
				(let ((date-format (cdr (assoc "date-format" ledger-environment-alist))))
					(dolist (xact xacts)
						(dolist (posting (nthcdr 5 xact))
							(let ((beg (point))
										(where (ledger-marker-where-xact-is xact posting)))
								(insert (format "%s %-4s %-30s %-30s %15s\n"
																(format-time-string (if date-format
																												date-format
																												ledger-reconcile-default-date-format) (nth 2 xact))
																(if (nth 3 xact)
																		(nth 3 xact)
																		"")
																(nth 4 xact) (nth 1 posting) (nth 2 posting)))
								(if (nth 3 posting)
										(if (eq (nth 3 posting) 'pending)
												(set-text-properties beg (1- (point))
																						 (list 'face 'ledger-font-reconciler-pending-face
																									 'where where))
												(set-text-properties beg (1- (point))
																						 (list 'face 'ledger-font-reconciler-cleared-face
																									 'where where)))
										(set-text-properties beg (1- (point))
																				 (list 'face 'ledger-font-reconciler-uncleared-face
																							 'where where))))  ))
					(goto-char (point-max))
					(delete-char -1)) ;gets rid of the extra line feed at the bottom of the list
				(if ledger-success
						(insert (concat "There are no uncleared entries for " account))
						(insert "Ledger has reported a problem.  Check *Ledger Error* buffer.")))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (toggle-read-only t)

    (ledger-reconcile-ensure-xacts-visible)
    (length xacts)))

(defun ledger-reconcile-ensure-xacts-visible ()
  "Ensures that the last of the visible transactions in the
ledger buffer is at the bottom of the main window.  The key to
this is to ensure the window is selected when the buffer point is
moved and recentered.  If they aren't strange things happen."

	(let ((recon-window (get-buffer-window (get-buffer ledger-recon-buffer-name))))
		(when recon-window
			(fit-window-to-buffer recon-window)
			(with-current-buffer buf
				(add-hook 'kill-buffer-hook 'ledger-reconcile-quit nil t)
				(if (get-buffer-window buf)
						(select-window (get-buffer-window buf)))
				(goto-char (point-max))
				(recenter -1))
			(select-window recon-window)
			(ledger-reconcile-visit t))
		(add-hook 'post-command-hook 'ledger-reconcile-track-xact nil t)))

(defun ledger-reconcile-track-xact ()
  "Force the ledger buffer to recenter on the transaction at point in the reconcile buffer."
  (if (and ledger-buffer-tracks-reconcile-buffer
					 (member this-command (list 'next-line
																			'previous-line
																			'mouse-set-point
																			'ledger-reconcile-toggle
																			'end-of-buffer
																			'beginning-of-buffer)))
      (save-excursion
				(ledger-reconcile-visit t))))

(defun ledger-reconcile-open-windows (buf rbuf)
  "Ensure that the ledger buffer BUF is split by RBUF."
  (if ledger-reconcile-force-window-bottom
      ;;create the *Reconcile* window directly below the ledger buffer.
      (set-window-buffer (split-window (get-buffer-window buf) nil nil) rbuf)
      (pop-to-buffer rbuf)))

(defun ledger-reconcile ()
  "Start reconciling, prompt for account."
  (interactive)
  (let ((account (ledger-read-account-with-prompt "Account to reconcile"))
				(buf (current-buffer))
        (rbuf (get-buffer ledger-recon-buffer-name)))

		(add-hook 'after-save-hook 'ledger-reconcile-refresh-after-save nil t)

    (if rbuf ;; *Reconcile* already exists
				(with-current-buffer rbuf
					(set 'ledger-acct account) ;; already buffer local
					(when (not (eq buf rbuf))
						;; called from some other ledger-mode buffer
						(ledger-reconcile-quit-cleanup)
						(set 'ledger-buf buf)) ;; should already be buffer-local

					(unless (get-buffer-window rbuf)
						(ledger-reconcile-open-windows buf rbuf)))

				;; no recon-buffer, starting from scratch.

				(with-current-buffer (setq rbuf
																	 (get-buffer-create ledger-recon-buffer-name))
					(ledger-reconcile-open-windows buf rbuf)
					(ledger-reconcile-mode)
					(make-local-variable 'ledger-target)
					(set (make-local-variable 'ledger-buf) buf)
					(set (make-local-variable 'ledger-acct) account)))

    ;; Narrow the ledger buffer
    (with-current-buffer rbuf
      (save-excursion
				(if ledger-narrow-on-reconcile
						(ledger-occur-mode account ledger-buf)))
      (if (> (ledger-reconcile-refresh) 0)
					(ledger-reconcile-change-target))
      (ledger-display-balance))))

(defvar ledger-reconcile-mode-abbrev-table)

(defun ledger-reconcile-change-target ()
  "Change the target amount for the reconciliation process."
  (interactive)
  (setq ledger-target (ledger-read-commodity-string ledger-reconcile-target-prompt-string)))

(defmacro ledger-reconcile-change-sort-key-and-refresh (sort-by)
	`(lambda ()
		 (interactive)

		 (setq ledger-reconcile-sort-key ,sort-by)
		 (ledger-reconcile-refresh)))

(define-derived-mode ledger-reconcile-mode text-mode "Reconcile"
	"A mode for reconciling ledger entries."
	(let ((map (make-sparse-keymap)))
		(define-key map [(control ?m)] 'ledger-reconcile-visit)
		(define-key map [return] 'ledger-reconcile-visit)
		(define-key map [(control ?l)] 'ledger-reconcile-refresh)
		(define-key map [(control ?c) (control ?c)] 'ledger-reconcile-finish)
		(define-key map [? ] 'ledger-reconcile-toggle)
		(define-key map [?a] 'ledger-reconcile-add)
		(define-key map [?d] 'ledger-reconcile-delete)
		(define-key map [?g] 'ledger-reconcile);
		(define-key map [?n] 'next-line)
		(define-key map [?p] 'previous-line)
		(define-key map [?t] 'ledger-reconcile-change-target)
		(define-key map [?s] 'ledger-reconcile-save)
		(define-key map [?q] 'ledger-reconcile-quit)
		(define-key map [?b] 'ledger-display-balance)

		(define-key map [(control ?c) (control ?o)] (ledger-reconcile-change-sort-key-and-refresh "(0)"))

		(define-key map [(control ?c) (control ?a)] (ledger-reconcile-change-sort-key-and-refresh "(amount)"))

		(define-key map [(control ?c) (control ?d)] (ledger-reconcile-change-sort-key-and-refresh "(date)"))

		(define-key map [(control ?c) (control ?p)] (ledger-reconcile-change-sort-key-and-refresh "(payee)"))

		(define-key map [menu-bar] (make-sparse-keymap "ledger-recon-menu"))
		(define-key map [menu-bar ledger-recon-menu] (cons "Reconcile" map))
		(define-key map [menu-bar ledger-recon-menu qui] '("Quit" . ledger-reconcile-quit))
		(define-key map [menu-bar ledger-recon-menu sep1] '("--"))
		(define-key map [menu-bar ledger-recon-menu pre] '("Previous Entry" . previous-line))
		(define-key map [menu-bar ledger-recon-menu vis] '("Visit Source" . ledger-reconcile-visit))
		(define-key map [menu-bar ledger-recon-menu nex] '("Next Entry" . next-line))
		(define-key map [menu-bar ledger-recon-menu sep2] '("--"))
		(define-key map [menu-bar ledger-recon-menu del] '("Delete Entry" . ledger-reconcile-delete))
		(define-key map [menu-bar ledger-recon-menu add] '("Add Entry" . ledger-reconcile-add))
		(define-key map [menu-bar ledger-recon-menu tog] '("Toggle Entry" . ledger-reconcile-toggle))
		(define-key map [menu-bar ledger-recon-menu sep3] '("--"))
		(define-key map [menu-bar ledger-recon-menu sort-orig] `("Sort by file order" . ,(ledger-reconcile-change-sort-key-and-refresh "(0)")))
		(define-key map [menu-bar ledger-recon-menu sort-amt] `("Sort by amount" . ,(ledger-reconcile-change-sort-key-and-refresh "(amount)")))
		(define-key map [menu-bar ledger-recon-menu sort-pay] `("Sort by date" . ,(ledger-reconcile-change-sort-key-and-refresh "(date)")))
		(define-key map [menu-bar ledger-recon-menu sort-dat] `("Sort by payee" . ,(ledger-reconcile-change-sort-key-and-refresh "(payee)")))
		(define-key map [menu-bar ledger-recon-menu sep4] '("--"))
		(define-key map [menu-bar ledger-recon-menu bal] '("Show Cleared Balance" . ledger-display-balance))
		(define-key map [menu-bar ledger-recon-menu tgt] '("Change Target Balance" . ledger-reconcile-change-target))
		(define-key map [menu-bar ledger-recon-menu sep5] '("--"))
		(define-key map [menu-bar ledger-recon-menu rna] '("Reconcile New Account" . ledger-reconcile))
		(define-key map [menu-bar ledger-recon-menu sep6] '("--"))
		(define-key map [menu-bar ledger-recon-menu fin] '("Finish" . ledger-reconcile-finish))
		(define-key map [menu-bar ledger-recon-menu ref] '("Refresh" . ledger-reconcile-refresh))
		(define-key map [menu-bar ledger-recon-menu sav] '("Save" . ledger-reconcile-save))

		(use-local-map map)))

(provide 'ledger-reconcile)

;;; ledger-reconcile.el ends here
