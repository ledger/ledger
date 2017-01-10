;;; ledger-format.el --- Format your ledger similar to `gofmt`

;; Copyright (C) 2003-2016 Alexandre Bourget (alex AT bourget DOT cc)

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
;; Function to call `ledgerfmt` from https://github.com/abourget/ledger

(defcustom ledgerfmt-command "ledgerfmt"
  "The 'ledgerfmt' command.
Some users may replace this with different invocations of 'ledgerfmt'"
  :type 'string
  :group 'ledger)

(defcustom ledgerfmt-show-errors 'buffer
  "Where to display ledgerfmt error output.
It can either be displayed in its own buffer, in the echo area, or not at all.

Please note that Emacs outputs to the echo area when writing
files and will overwrite ledgerfmt's echo output if used from inside
a `before-save-hook'."
  :type '(choice
          (const :tag "Own buffer" buffer)
          (const :tag "Echo area" echo)
          (const :tag "None" nil))
  :group 'ledger)

(defun ledgerfmt ()
  "Format the current buffer according to the ledgerfmt tool."
  (interactive)
  (let ((tmpfile (make-temp-file "ledgerfmt" nil ".ledger"))
        (patchbuf (get-buffer-create "*Ledgerfmt patch*"))
        (errbuf (if ledgerfmt-show-errors (get-buffer-create "*Ledgerfmt Errors*")))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))

    (save-restriction
      (widen)
      (if errbuf
          (with-current-buffer errbuf
            (setq buffer-read-only nil)
            (erase-buffer)))
      (with-current-buffer patchbuf
        (erase-buffer))

      (write-region nil nil tmpfile)

      ;; We're using errbuf for the mixed stdout and stderr output. This
      ;; is not an issue because ledgerfmt -w does not produce any stdout
      ;; output in case of success.
      (if (zerop (call-process ledgerfmt-command nil errbuf nil "-w" tmpfile))
          (progn
            (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                (message "Buffer is already ledgerfmted")
              (go--apply-rcs-patch patchbuf)
              (message "Applied ledgerfmt"))
            (if errbuf (ledgerfmt--kill-error-buffer errbuf)))
        (message "Could not apply ledgerfmt")
        (if errbuf (ledgerfmt--process-errors (buffer-file-name) tmpfile errbuf)))

      (kill-buffer patchbuf)
      (delete-file tmpfile))))

(defun ledgerfmt--process-errors (filename tmpfile errbuf)
  (with-current-buffer errbuf
    (if (eq ledgerfmt-show-errors 'echo)
        (progn
          (message "%s" (buffer-string))
          (ledgerfmt--kill-error-buffer errbuf))
      ;; Convert the ledgerfmt stderr to something understood by the compilation mode.
      (goto-char (point-min))
      (insert "ledgerfmt errors:\n")
      (while (search-forward-regexp (concat "^\\(" (regexp-quote tmpfile) "\\):") nil t)
        (replace-match (file-name-nondirectory filename) t t nil 1))
      (compilation-mode)
      (display-buffer errbuf))))

(defun ledgerfmt--kill-error-buffer (errbuf)
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (kill-buffer errbuf))))
