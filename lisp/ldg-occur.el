;;; ldg-mode.el --- Helper code for use with the "ledger" command-line tool

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
;; Provide code folding to ledger mode.  Adapted from original loccur
;; mode by Alexey Veretennikov <alexey dot veretennikov at gmail dot
;; com> 
;;
;; Adapted to ledger mode by Craig Earls <enderww at gmail dot
;; com>

;;; Code:

(defface ledger-occur-folded-face 
    `((t :foreground "grey70" :invisible t ))
  "Default face for Ledger occur mode hidden transactions"
  :group 'ledger-faces)

(defface ledger-occur-xact-face 
    `((t :background "blue" :weight normal ))
  "Default face for Ledger occur mode shown transactions"
  :group 'ledger-faces)

(defconst ledger-occur-overlay-property-name 'ledger-occur-custom-buffer-grep)

(defcustom ledger-occur-use-face-unfolded t 
  "if non-nil use a custom face for xacts shown in ledger-occur mode"
  :type 'boolean
  :group 'ledger)
(make-variable-buffer-local 'ledger-occur-use-face-unfolded)


(defvar ledger-occur-mode nil) ;; name of the minor mode, shown in the mode-line
(make-variable-buffer-local 'ledger-occur-mode)

(or (assq 'ledger-occur-mode minor-mode-alist)
    (nconc minor-mode-alist
           (list '(ledger-occur-mode ledger-occur-mode))))

(defvar ledger-occur-history nil
  "History of previously searched expressions for the prompt")
(make-variable-buffer-local 'ledger-occur-history)

(defvar ledger-occur-last-match nil
  "Last match found")
(make-variable-buffer-local 'ledger-occur-last-match)

(defvar ledger-occur-overlay-list nil
  "A list of currently active overlays to the ledger buffer.")
(make-variable-buffer-local 'ledger-occur-overlay-list)

(defun ledger-occur-mode (regex buffer)
  "Higlight transaction that match REGEX, hiding others

When REGEX is nil, unhide everything, and remove higlight"
  (progn
    (set-buffer buffer)
    (setq ledger-occur-mode 
	  (if (or (null regex)
		  (zerop (length regex)))
	      nil
	      (concat " Ledger-Folded: " regex)))
    (force-mode-line-update)
    (ledger-occur-remove-overlays)
    (if ledger-occur-mode
	(let* ((buffer-matches (ledger-occur-find-matches regex))
	       (ovl-bounds (ledger-occur-create-xact-overlay-bounds buffer-matches)))
	  (setq ledger-occur-overlay-list 
		(ledger-occur-create-xact-overlays ovl-bounds))
	  (setq ledger-occur-overlay-list
		(append ledger-occur-overlay-list
			(ledger-occur-create-folded-overlays buffer-matches)))
	  (setq ledger-occur-last-match regex)
	  (select-window (get-buffer-window buffer))))
    (recenter)))

(defun ledger-occur (regex)
  "Perform a simple grep in current buffer for the regular
   expression REGEX

   This command hides all xact from the current buffer except
   those containing the regular expression REGEX. A second call
   of the function unhides lines again"
  (interactive 
   (if ledger-occur-mode
       (list nil)
       (list (read-string (concat "Regexp<" (ledger-occur-prompt)
				  ">: ") "" 'ledger-occur-history ))))
  (if (string-equal "" regex) (setq regex (ledger-occur-prompt)))
  (ledger-occur-mode regex (current-buffer)))

(defun ledger-occur-prompt ()
  "Returns the default value of the prompt.

   Default value for prompt is a current word or active
   region(selection), if its size is 1 line"
  (let ((prompt
         (if (and transient-mark-mode
                  mark-active)
             (let ((pos1 (region-beginning))
                   (pos2 (region-end)))
               ;; Check if the start and the of an active region is on
               ;; the same line
               (if (= (line-number-at-pos pos1)
                      (line-number-at-pos pos2))
                   (buffer-substring-no-properties pos1 pos2)))
	     (current-word))))
    prompt))

(defun ledger-occur-create-folded-overlays(buffer-matches)
  (if buffer-matches
      (let ((overlays
             (let ((prev-end (point-min))
                   (temp (point-max)))
               (mapcar (lambda (match)
                         (progn
                           (setq temp prev-end) ;need a swap so that the
					;last form in the lambda
					;is the (make-overlay)
                           (setq prev-end (1+ (cadr match))) ;add 1 so
					;that we skip
					;the empty
					;line after
					;the xact
                           (make-overlay
                            temp
                            (car match)
                            (current-buffer) t nil)))
                       buffer-matches))))
        (mapcar (lambda (ovl)
                  (overlay-put ovl ledger-occur-overlay-property-name t)
                  (overlay-put ovl 'invisible t)
                  (overlay-put ovl 'intangible t))
                (push  (make-overlay (cadr (car(last buffer-matches)))
                                     (point-max)
                                     (current-buffer) t nil) overlays)))))


(defun ledger-occur-create-xact-overlays (ovl-bounds)
  (let ((overlays 
         (mapcar (lambda (bnd)
                   (make-overlay
                    (car bnd)
                    (cadr bnd)
                    (current-buffer) t nil))
                 ovl-bounds)))
    (mapcar (lambda (ovl)
	      (overlay-put ovl ledger-occur-overlay-property-name t)
	      (if ledger-occur-use-face-unfolded
		  (overlay-put ovl 'face 'ledger-occur-xact-face )))
            overlays)))

(defun ledger-occur-change-regex (regex buffer)
  "use this function to programatically change the overlays,
   rather than quitting out and restarting"
  (progn
    (set-buffer buffer)
    (setq ledger-occur-mode nil)
    (force-mode-line-update)
    (ledger-occur-mode regex buffer)
    (recenter)))

(defun ledger-occur-quit-buffer (buffer)
  "quits hidings transaction in the given buffer.  Used for
   coordinating ledger-occur with other buffers, like reconcile"
  (progn
    (set-buffer buffer)
    (setq ledger-occur-mode nil)
    (force-mode-line-update)
    (ledger-occur-remove-overlays)
    (recenter)))

(defun ledger-occur-remove-overlays ()
  (interactive)
  (remove-overlays (point-min) 
		   (point-max) ledger-occur-overlay-property-name t)
  (setq ledger-occur-overlay-list nil))


(defun ledger-occur-create-xact-overlay-bounds (buffer-matches)
  (let ((prev-end (point-min))
        (overlays (list)))
    (when buffer-matches
      (mapc (lambda (line)
	      (push (list (car line) (cadr line)) overlays)
	      (setq prev-end (cadr line)))
	    buffer-matches)
      (setq overlays (nreverse overlays)))))

(defun ledger-occur-find-xact-extents (pos)
  "return point for beginning of xact and and of xact containing
   position.  Requires empty line separating xacts"
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (let ((end-pos pos)
	  (beg-pos pos))
      (backward-paragraph)
      (forward-line)
      (beginning-of-line)
      (setq beg-pos (point))
      (forward-paragraph)
      (forward-line -1)
      (end-of-line)
      (setq end-pos (1+ (point)))
      (list beg-pos end-pos))))

(defun ledger-occur-find-matches (regex)
  "Returns a list of 2-number tuples, specifying begnning of the
   line and end of a line containing matching xact"
  (save-excursion
    (goto-char (point-min))
    ;; Set initial values for variables
    (let ((curpoint nil)
          (endpoint nil)
          (lines (list)))
      ;; Search loop
      (while (not (eobp))
        (setq curpoint (point))
        ;; if something found
        (when (setq endpoint (re-search-forward regex nil 'end))
          (save-excursion
	    (let ((bounds (ledger-occur-find-xact-extents (match-beginning 0))))
	      (push bounds lines)
	      (setq curpoint (cadr bounds)))) ;move to the end of the
					;xact, no need to search
					;inside it more
          (goto-char curpoint))
        (forward-line 1))
      (setq lines (nreverse lines)))))


(provide 'ldg-occur)

;;; ldg-occur.el ends here
