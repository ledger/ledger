;; $HeadURL: https://tstotts.net/pubvc/elisp-snippets/ledger-indent.el $
;; $Id: ledger-indent.el 443 2007-01-23 04:36:06Z tas $

;; Authored by Timothy Stotts in 2007.
;;
;; ledger-indent.el is a set of definitions
;; to provided additional functionality to ledger.el:
;;     - forced indentation
;;     - more extensive font-lock
;;     - word completion with M-TAB
;; 
;; This code is far from perfect or complete, but may be preferable for
;; those individuals accustom to Emacs major modes providing indentation
;; and word completion according to mode. As of early 2007,
;; ledger.el does not provide much in the the way of “tidy” editing.
;;
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information. 

;; This code is not part of GNU Emacs.

;; mandatory
(require 'ledger)

(defcustom ledger-enforce-indentation
  t
  "*When non-nil, indentation is partially enforced."
  :group 'ledger
  :type 'boolean)

(defcustom ledger-tab-stop-list
  '(0 2 60 65)
  "*List of tab stop positions used for `ledger-mode'.
This should be a list of integers, ordered from smallest to largest.
By default, this list contains only four numbers:
    first - ledger date column
    second - ledger account name column
    third - ledger unitary amount column
    fourth - ledger optional comment column

An example of the supported syntax:
2007/01/01 * Balance
  Assets:Checking                          $342.00     ; year starting amount
  Equity:Opening Balances
"
  :group 'ledger
  :type '(repeat integer))

(defcustom ledger-colon-is-part-of-word
  t
  "*Non-nil means consider the colon character `:' as part of word.
An identifier containing colons is then treated as a single word in
select and move operations.  All parts of an identifier separated by colon
are treated as single words otherwise.
"
  :type 'boolean
  :group 'ledger-indent)

(defcustom ledger-word-completion-case-sensitive
  nil
  "*Non-nil means word completion is case sensitive."
  :type 'boolean
  :group 'ledger-indent)

(defcustom ledger-indent-font-lock
  t
  "*Non-nil means use `ledger-indent' font-lock highlighting
instead of the default `legder' font-lock."
  :type 'boolean
  :group 'ledger-indent)

(defvar ledger-font-lock-keywords-orig ledger-font-lock-keywords
  "Backup of `ledger-font-lock-keywords'")

(defun ledger-after-change (beg end len)
  "Performs automatic indentation and unindentation of entries in the form of
left-tab and right-tab stops."
  (when (and ledger-enforce-indentation
             (not (member this-command
                          '(undo ledger-add-entry)
                          )))
    ;;(message "this-command: %s" this-command)
    (let* ( (col (current-column))
            (ind (nth 1 ledger-tab-stop-list))
            (tab (nth 2 ledger-tab-stop-list))
            (com (nth 3 ledger-tab-stop-list))
            (dif (- end beg))
            (ent (save-excursion
                   (move-to-column 0)
                   (looking-at "^\\([0-9]+/[0-9]+/[0-9]+\\|[~] \\S \\{4\\}\\)")))
            (pre (save-excursion
                   (forward-line -1)
                   (move-to-column 0)
                   (looking-at "^\\([0-9]+/[0-9]+/[0-9]+\\|[~] \\S \\{4\\}\\)")))
            (act (save-excursion
                   (move-to-column 0)
                   (looking-at "^\\s-+[a-zA-Z0-9_]+:[a-zA-Z0-9_]+")))
            (aut (save-excursion
                   (forward-line -1)
                   (move-to-column 0)
                   (looking-at "^[=] ")))
            (del (eq this-command 'delete-backward-char))
            (nsp (save-excursion
                   (move-end-of-line 1)
                   (skip-chars-backward " \t")
                   (current-column)))
            )

      ;; auto-indent the account name column as a left-tab
      ;; for the two lines following a starting entry line;
      ;; do similar for first line following period and automated entry.
      (when (< col ind)
        (let* (
               (pretwo (save-excursion
                         (forward-line -2)
                         (move-to-column 0)
                         (looking-at "^\\([0-9]+/[0-9]+/[0-9]+\\|[~] \\S \\{4\\}\\)")))
               (z (looking-at (concat "^[ ]\\{" (number-to-string ind) "\\}")))
               )

          (if z (forward-char ind)
            (if (and (or pre pretwo aut) (not ent)) (insert-char ?  (- ind col))))
          ))
        
      ;; auto-align the unitary amount column as a right-tab
      ;; in similar fashion as a right-justified spread sheet cell
      ;; or a word processor's right tab
      (when (and (null ent)
                 (or pre aut act)
                 (>= col (+ -10 tab))
                 (<= col (+ 3 tab))
                 (or (and (>= dif 1)(< dif 5)) (and (= 0 dif) (= 1 len))))
        (message "Align right-tab")
        (if (save-excursion
              (move-end-of-line 1)
              (>= (current-column) com))
            (skip-chars-forward "^ \t")
          (move-end-of-line 1))
        (let ( (e (- tab (current-column))) )
          (skip-chars-backward "^ \t")
          (when (>= (current-column) tab)
            (delete-char -1)
            (setq e (+ 1 e))
            (skip-chars-backward "^ \t"))
          (if (> 0 e)
              (delete-char e)
            (insert-char ?  e))
          )
        (let ((newcol (+ col
                         (cond ((> len 0) (+ 0 len))
                               ((> dif 0) (- 0 dif))
                               (t tab)))))
          (move-to-column (if (< newcol tab) newcol tab))
          )
        )

      ;; erase the right-tab indentation if the command is backspace
      ;; and nothing but whitespace exists in the right column
      (when (and del
                 (or (and (< nsp (+ tab -5))
                          (>= col (+ -10 tab)))
                     (= col (+ ind -1))))
        (message "Untab")
        (let ((p (point)))
          (skip-chars-backward " \t")
          (if (and (> col ind) (< (current-column) ind))
              (move-to-tab-stop))
          (delete-region (point) p)))
      ))
  t)

;; I couldn't put all of this in after-change due to (newline) moving
;; the point back to column 0 after applying after-change-functions.

(defun ledger-newline (&optional arg)
  "Performs the normal call to `newline'. Then moves point to first tab-stop
if and only if the indentation to first tab-stop already exists."
  (interactive "*P")
  (newline arg)
  (when ledger-enforce-indentation
    (let ( (ind (nth 1 ledger-tab-stop-list)) )
      (if (and (= 0 (current-column))
               (looking-at (concat "^[ ]\\{" (number-to-string ind) "\\}")))
          (forward-char ind))
      )))

(defun ledger-start-comment (&optional arg)
  "Start a comment. If the `point' is in the right-tab column,
the move one column forward and start a comment with `;'.
Otherwise, just insert `;' at the current `point'."
  (interactive "*P")
  (when ledger-enforce-indentation
    (let* ( (col (current-column))
            (tab (nth 2 ledger-tab-stop-list))
            (ind (and (>= col (+ -10 tab))
                      (<= col (+ 3 tab)))) )
      (when ind
        (move-to-column tab)
        (move-to-tab-stop))
      ))
  (insert ";")
  )
      
  
(defvar ledger-indent-font-lock-keywords
  '(("^[0-9./=]+\\s-+\\(?:([^)]+)\\s-+\\)?\\([^*].+\\)" 1 bold)
    ("\s \\{5,\\}\\([$][0-9][0-9,.]*\\)" 1 font-lock-constant-face)
    ("\s \\{5,\\}\\([$]-[0-9][0-9,.]*\\)" 1 font-lock-warning-face)
    ("\s \\{5,\\}\\(-?[0-9][0-9,.]*\\)" 1 font-lock-type-face)
    ("^\s +\\([A-Za-z0-9_]+:[A-Za-z0-9_]+[A-Za-z0-9_: ]*\\)" 1 font-lock-variable-name-face)
    ("^\s +\\((\\)\\([A-Za-z0-9]+:[A-Za-z0-9]+[A-Za-z0-9: ]*\\)\\()\\)" (1 font-lock-function-name-face) (2 font-lock-variable-name-face) (3 font-lock-function-name-face))
;;    ("\\<\\(days?\\|weeks?\\|months?\\|quarters?\\|years?\\|from\\|to\\|since\\|until\\)\\>" 0 font-lock-keyword-face)
;;    ("^~.*\\(every\\|monthly\\|weekly\\|daily\\)\\>" 1 font-lock-type-face)
    ("^[0-9]+[/-][0-9]+.*\\([*]\\)" 1 bold)
    ("^\\([~=]\\)\\s " 1 font-lock-function-name-face))
  "Improved expressions to highlight in Ledger mode.")

(defadvice ledger-add-entry (after after-ledger-add-entry activate)
  "After adding an entry, move the point to the correct position for further text entry."
  (when ledger-enforce-indentation
    (move-end-of-line 1)
    (insert-char ?  1)))

(defadvice ledger-mode (around indent-ledger-mode activate)
  "Advise `ledger-mode' to setup tab stops, hook indentation functions,
modify the keymap for purposes of indentation, improve the syntax table,
improve font-locking, and provide keyword expansion with `M-TAB'."
  (setq ledger-font-lock-keywords
        (if ledger-indent-font-lock
            ledger-indent-font-lock-keywords
          ledger-font-lock-keywords-orig))
  ad-do-it
  (make-local-variable 'tab-stop-list)
  (make-local-variable 'after-change-functions)
  (when ledger-enforce-indentation
    (setq tab-stop-list ledger-tab-stop-list)
    (local-set-key (kbd "TAB") 'move-to-tab-stop)
    (local-set-key (kbd "RET") 'ledger-newline)
    (local-set-key (kbd ";") 'ledger-start-comment)
    ;; This function has to be appended as last
    ;; otherwise, ledger-mode will cause strange font lock errors
    ;; under special conditions. Perhaps this means
    ;; a bug exists in ledger.el or in this source.
    (add-hook 'after-change-functions 'ledger-after-change t t))
  ;; keyword expansion
  (local-set-key (kbd "M-TAB") 'ledger-expand)
    ;; some syntax recognition for: comments
  (modify-syntax-entry ?\n ">   ")
    (modify-syntax-entry ?\; "<   ")
    (when ledger-colon-is-part-of-word
      (modify-syntax-entry ?\: "w"))
    )

(defadvice ledger-add-entry (around indent-ledger-add-entry activate)
  "Advise `ledger-add-entry' to ensure at least one blank line between
ledger entries."
  ad-do-it
  (let* ((noadd (save-excursion
                (move-to-column 0)
                (forward-line -1)
                (looking-at "^\\s-*$")
                ))
         (rem (save-excursion
                (move-to-column 0)
                (forward-line -2)
                (looking-at "^\\s-*$"))))

    (cond ((not noadd)
           (progn (move-to-column 0)
                  (newline 1)
                  (move-end-of-line 1)
                  (message "Added missing newline")))
          ((and noadd rem)
           (progn (move-to-column 0)
                  (let ((p (point)))
                    (forward-line -1)
                    (delete-region p (point)))
                  (message "Removed extra newline")))
          )
    ))

(defadvice ledger-toggle-current (after indent-ledger-toggle-current activate)
  "Advise `ledger-toggle-current' to move the point correctly if adding clear
status on entry with only date and clear status"
  (let* ((clear (save-excursion
                  (move-to-column 0)
                  (looking-at "^[0-9]+/[0-9]+/[0-9]+ \\* "))))
    (and clear
         (forward-char 3))))


;;;; the following are optional features depending on availability of libraries

(defun ledger-expand-abbrev (arg))

;; setup word expansions for ledger
(defun ledger-expand (&optional prefix-arg)
  "Perform `hippie-exp' expansion on the current word. Expansion
is performed for matching words across all open `ledger-mode'
buffers."
  (interactive "P")
  (require 'hippie-exp)
  ;; create definition for expansion function on the first call
  (when (not (commandp 'ledger-expand-abbrev))
    (fset 'ledger-expand-abbrev (make-hippie-expand-function
                                 '(try-expand-dabbrev
                                   try-expand-dabbrev-all-buffers))))
    
  (let ((case-fold-search (not ledger-word-completion-case-sensitive))
        (case-replace nil)
        (hippie-expand-only-buffers
         (or (and (boundp 'hippie-expand-only-buffers)
                  hippie-expand-only-buffers)
             '(ledger-mode))))
       (ledger-expand-abbrev prefix-arg)))


(provide 'ledger-indent)
