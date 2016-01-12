;;; ledger-complete.el --- Helper code for use with the "ledger" command-line tool

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
;; Functions providing payee and account auto complete.

(require 'pcomplete)

;; In-place completion support

;;; Code:

(declare-function ledger-thing-at-point "ledger-context" nil)
(declare-function ledger-add-transaction "ledger-xact" (transaction-text &optional insert-at-point))
(declare-function between "ledger-schedule" (val low high))

(defun ledger-parse-arguments ()
  "Parse whitespace separated arguments in the current region."
  ;; this is more complex than it appears to need, so that it can work
  ;; with pcomplete.  See pcomplete-parse-arguments-function for
  ;; details
  (let* ((begin (save-excursion
                  (ledger-thing-at-point) ;; leave point at beginning of thing under point
                  (point)))
         (end (point))
         begins args)
    ;; to support end of line metadata
    (save-excursion
      (when (search-backward ";"
                             (line-beginning-position) t)
        (setq begin (match-beginning 0))))
    (save-excursion
      (goto-char begin)
      (when (< (point) end)
        (skip-chars-forward " \t\n")
        (setq begins (cons (point) begins))
        (setq args (cons (buffer-substring-no-properties
                          (car begins) end)
                         args)))
      (cons (reverse args) (reverse begins)))))


(defun ledger-payees-in-buffer ()
  "Scan buffer and return list of all payees."
  (let ((origin (point))
        payees-list)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              ledger-payee-any-status-regex nil t)  ;; matches first line
        (unless (and (>= origin (match-beginning 0))
                     (< origin (match-end 0)))
          (setq payees-list (cons (match-string-no-properties 3)
                                  payees-list)))))  ;; add the payee
    ;; to the list
    (pcomplete-uniqify-list (nreverse payees-list))))


(defun ledger-find-accounts-in-buffer ()
  (interactive)
  (let ((origin (point))
        accounts
        (account-tree (list t))
        (account-elements nil)
        (seed-regex (ledger-account-any-status-with-seed-regex
                     (regexp-quote (car pcomplete-args)))))
    (save-excursion
      (goto-char (point-min))

      (dolist (account
               (delete-dups
                (progn
                  (while (re-search-forward seed-regex nil t)
                    (unless (between origin (match-beginning 0) (match-end 0))
                      (setq accounts (cons (match-string-no-properties 2) accounts))))
                  accounts)))
        (let ((root account-tree))
          (setq account-elements
                (split-string
                 account ":"))
          (while account-elements
            (let ((xact (assoc (car account-elements) root)))
              (if xact
                  (setq root (cdr xact))
                (setq xact (cons (car account-elements) (list t)))
                (nconc root (list xact))
                (setq root (cdr xact))))
            (setq account-elements (cdr account-elements))))))
    account-tree))

(defun ledger-accounts ()
  "Return a tree of all accounts in the buffer."
  (let* ((current (caar (ledger-parse-arguments)))
         (elements (and current (split-string current ":")))
         (root (ledger-find-accounts-in-buffer))
         (prefix nil))
    (while (cdr elements)
      (let ((xact (assoc (car elements) root)))
        (if xact
            (setq prefix (concat prefix (and prefix ":")
                                 (car elements))
                  root (cdr xact))
          (setq root nil elements nil)))
      (setq elements (cdr elements)))
    (setq root (delete (list (car elements) t) root))
    (and root
         (sort
          (mapcar (function
                   (lambda (x)
                     (let ((term (if prefix
                                     (concat prefix ":" (car x))
                                   (car x))))
                       (if (> (length (cdr x)) 1)
                           (concat term ":")
                         term))))
                  (cdr root))
          'string-lessp))))

(defun ledger-complete-at-point ()
  "Do appropriate completion for the thing at point."
  (interactive)
  (while (pcomplete-here
          (if (eq (save-excursion
                    (ledger-thing-at-point)) 'transaction)
              (if (null current-prefix-arg)
                  (delete
                   (caar (ledger-parse-arguments))
                   (ledger-payees-in-buffer)) ;; this completes against payee names
                (progn
                  (let ((text (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))))
                    (delete-region (line-beginning-position)
                                   (line-end-position))
                    (condition-case nil
                        (ledger-add-transaction text t)
                      (error nil)))
                  (forward-line)
                  (goto-char (line-end-position))
                  (search-backward ";" (line-beginning-position) t)
                  (skip-chars-backward " \t0123456789.,")
                  (throw 'pcompleted t)))
            (ledger-accounts)))))

(defun ledger-trim-trailing-whitespace (str)
  (replace-regexp-in-string "[ \t]*$" "" str))

(defun ledger-fully-complete-xact ()
  "Completes a transaction if there is another matching payee in the buffer.
Does not use ledger xact"
  (interactive)
  (let* ((name (ledger-trim-trailing-whitespace (caar (ledger-parse-arguments))))
         (rest-of-name name)
         xacts)
    (save-excursion
      (when (eq 'transaction (ledger-thing-at-point))
        (delete-region (point) (+ (length name) (point)))
        ;; Search backward for a matching payee
        (when (re-search-backward
               (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+\\(.*"
                       (regexp-quote name) ".*\\)" ) nil t)
          (setq rest-of-name (match-string 3))
          ;; Start copying the postings
          (forward-line)
          (while (looking-at ledger-account-any-status-regex)
            (setq xacts (cons (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))
                              xacts))
            (forward-line))
          (setq xacts (nreverse xacts)))))
    ;; Insert rest-of-name and the postings
    (when xacts
      (save-excursion
        (insert rest-of-name ?\n)
        (while xacts
          (insert (car xacts) ?\n)
          (setq xacts (cdr xacts))))
      (forward-line)
      (goto-char (line-end-position))
      (if (re-search-backward "\\(\t\\| [ \t]\\)" nil t)
          (goto-char (match-end 0))))))


(defcustom ledger-complete-ignore-case t
  "Non-nil means that ledger-complete-at-point will be case-insensitive"
  :type 'boolean
  :group 'ledger)

(defun ledger-pcomplete (&optional interactively)
  "Complete rip-off of pcomplete from pcomplete.el, only added
ledger-magic-tab in the previous commands list so that
ledger-magic-tab would cycle properly"
  (interactive "p")
  (let ((pcomplete-ignore-case ledger-complete-ignore-case))
    (if (and interactively
             pcomplete-cycle-completions
             pcomplete-current-completions
             (memq last-command '(ledger-magic-tab
                                  ledger-pcomplete
                                  pcomplete-expand-and-complete
                                  pcomplete-reverse)))
        (progn
          (delete-char (* -1 pcomplete-last-completion-length))
          (if (eq this-command 'pcomplete-reverse)
              (progn
                (push (car (last pcomplete-current-completions))
                      pcomplete-current-completions)
                (setcdr (last pcomplete-current-completions 2) nil))
            (nconc pcomplete-current-completions
                   (list (car pcomplete-current-completions)))
            (setq pcomplete-current-completions
                  (cdr pcomplete-current-completions)))
          (pcomplete-insert-entry pcomplete-last-completion-stub
                                  (car pcomplete-current-completions)
                                  nil pcomplete-last-completion-raw))
      (setq pcomplete-current-completions nil
            pcomplete-last-completion-raw nil)
      (catch 'pcompleted
        (let* (pcomplete-stub
               pcomplete-seen pcomplete-norm-func
               pcomplete-args pcomplete-last pcomplete-index
               pcomplete-autolist
               (completions (pcomplete-completions))
               (result (pcomplete-do-complete pcomplete-stub completions))
               (pcomplete-termination-string ""))
          (and result
               (not (eq (car result) 'listed))
               (cdr result)
               (pcomplete-insert-entry pcomplete-stub (cdr result)
                                       (memq (car result)
                                             '(sole shortest))
                                       pcomplete-last-completion-raw)))))))

(provide 'ledger-complete)

;;; ledger-complete.el ends here
