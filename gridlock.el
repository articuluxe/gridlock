;;; gridlock.el --- navigate among delimited fields
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, January 26, 2018
;; Version: 0.1
;; Modified Time-stamp: <2018-02-09 11:24:14 dharms>
;; Modified by: Dan Harms
;; Keywords: tools
;; URL: https://github.com/articuluxe/gridlock.git
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Navigate between delimited fields, and explicate them if possible.
;;

;;; Code:
(require 'subr-x)
(require 'ht)

(defgroup gridlock nil
  "Helper to navigate between fields, and explicate them."
  :group 'tools
  :prefix "gridlock")

(defcustom gridlock-prefix-key (kbd "C-c C-g")
  "Prefix key for `gridlock-mode'."
  :type 'vector)

(defvar-local gridlock-buffer-points
  (ht-create 'eq)
  "List of gridlock points in the current buffer.")

(defvar gridlock-anchor-regex "^"
  "Regexp identifying lines of interest.")
(defvar gridlock-field-delimiter ","
  "Regexp splitting fields per line.")
(defvar gridlock-field-regex-begin nil
  "Regexp to delimit the start of the fields to be scanned per line.
If nil, the entire string from the anchor point will be used.")
(defvar gridlock-field-regex-end nil
  "Regexp to delimit the end of the fields to be scanned per line.
If nil, the entire string to the end of line will be used.")

(defvar-local gridlock-buffer-metadata nil
  "Stores metadata about a cell.")

(defun gridlock--find-anchor-on-line (pt)
  "Find location, if any, of anchor point on line containing PT."
  (let (anchor)
    (save-excursion
      (goto-char pt)
      (goto-char (line-beginning-position))
      (when (setq anchor (search-forward-regexp gridlock-anchor-regex (line-end-position) t))
        (gridlock--on-anchor-found anchor))
      anchor)))

(defun gridlock-reset ()
  "Reset gridlock state in current buffer."
  (interactive)
  (ht-clear! gridlock-buffer-points)
  ;; todo clear metadata
  )

;;;###autoload
(defun gridlock-goto-next-line ()
  "Advance point to next anchor point."
  (interactive)
  (let ((pt (gridlock--find-next-line)))
    (if pt (goto-char pt)
      (message "No further lines."))))

(defun gridlock--find-next-line ()
  "Return location, if any, of next anchor point."
  (let ((pt (point))
        beg)
    (save-excursion
      (setq beg (search-forward-regexp gridlock-anchor-regex nil t))
      (when (eq beg pt)
        (goto-char (1+ (point)))
        (setq beg (search-forward-regexp gridlock-anchor-regex nil t)))
      (when beg
        (gridlock--on-anchor-found beg)
        beg))))

;;;###autoload
(defun gridlock-goto-prev-line ()
  "Advance point to prior anchor point."
  (interactive)
  (let ((pt (gridlock--find-prev-line)))
    (if pt (goto-char pt)
      (message "No prior lines."))))

(defun gridlock--find-prev-line ()
  "Return location, if any, of prior anchor point."
  (let ((pt (point))
        beg)
    (save-excursion
      (setq beg (search-backward-regexp gridlock-anchor-regex nil t))
      (when (eq beg pt)
        (goto-char (1- (point)))
        (setq beg (search-backward-regexp gridlock-anchor-regex nil t)))
      (when beg
        (gridlock--on-anchor-found beg)
        beg))))

(defun gridlock--on-anchor-found (beg)
  "An anchor point has been found at position BEG on a line."
  (unless (ht-contains? gridlock-buffer-points beg)
    (ht-set! gridlock-buffer-points beg
                 (save-match-data
                   (gridlock--parse-line beg))))
  ;; maybe move this to the minor-mode activation?
  (unless gridlock-buffer-metadata
    (setq gridlock-buffer-metadata
          (gridlock--get-buffer-metadata))))

(defun gridlock--parse-line (beg)
  "Parse the format of the line beginning at BEG."
  (save-excursion
    (let ((pt beg)
          (end (line-end-position))
          (idx 0)
          lst str)
      (goto-char beg)
      (and gridlock-field-regex-begin
           (not (string-empty-p gridlock-field-regex-begin))
           (search-forward-regexp gridlock-field-regex-begin end t)
           (setq beg (or (match-end 0) beg))
           (setq pt beg)
           (goto-char beg)
           gridlock-field-regex-end
           (not (string-empty-p gridlock-field-regex-end))
           (search-forward-regexp gridlock-field-regex-end end t)
           (setq end (or (match-beginning 0) end))
           ;; (message "pre: beg %d end %d" beg end)
           (goto-char beg)
           )
      (while (search-forward-regexp gridlock-field-delimiter end t)
        (setq str (buffer-substring-no-properties
                   pt (match-beginning 0)))
        ;; (message "during: pt is %d, end is %d, str is %s" pt end str)
        (push (list (cons pt (match-beginning 0)) idx str) lst)
        (setq idx (1+ idx))
        (setq pt (point)))
      (setq str (string-trim (buffer-substring-no-properties pt end)))
      ;; (message "post: end pt is %d, end is %d, str is %s" pt end str)
      (unless (string-empty-p str)
        (push (list (cons pt (+ pt (length str))) idx str) lst))
      (nreverse lst))))

(defun gridlock-field-get-bounds (field)
  "Given a record FIELD, return its bounds.
This is a cons cell (BEG . END) of the field's bounds."
  (nth 0 field))

(defun gridlock-field-get-index (field)
  "Given a record FIELD, return its index."
  (nth 1 field))

(defun gridlock-field-get-str (field)
  "Given a record FIELD, return its string value."
  (nth 2 field))

(defun gridlock-field-get-title (field)
  "Given a record FIELD, return its grid's title."
  ;; todo access metadata
  nil)

(defun gridlock-get-field-at (point)
  "Return the field, if any, that POINT lies on."
  (let ((fields (gridlock-get-fields-at point)))
    (gridlock--lookup-field-at-pos fields point)))

(defun gridlock-get-fields-at (point)
  "Return the fields, if any, existing on line containing POINT."
  (let ((anchor (gridlock--find-anchor-on-line point)))
    (ht-get gridlock-buffer-points anchor)))

(defun gridlock--lookup-field-at-pos (fields pt)
  "Lookup in field list FIELDS what field, if any, PT is within."
  (let (bounds)
    (catch 'found
      (dolist (elt fields)
        (setq bounds (gridlock-field-get-bounds elt))
        (when (and (>= pt (car bounds))
                   (< pt (cdr bounds)))
          (throw 'found elt))))))

(defun gridlock--get-buffer-metadata ()
  "Get the metadata for the current buffer."
  ;; todo
  nil)

(defun gridlock-define-keys (map)
  "Define in keymap MAP bindings for `gridlock-mode'."
  (define-key map "n" 'gridlock-goto-next-line)
  (define-key map "p" 'gridlock-goto-prev-line)
  )

(defvar gridlock-mode-map
  (let ((map (make-sparse-keymap)))
    (gridlock-define-keys map)
    map)
  "Keymap for `gridlock-mode'.")

(defun gridlock-define-prefix (map)
  "Define a prefix kepmap for `gridlock-mode' inside keymap MAP."
  (define-key map gridlock-prefix-key gridlock-mode-map))

(defvar gridlock-prefix-map
  (let ((map (make-sparse-keymap)))
    (gridlock-define-prefix map)
    map)
  "Prefix keymap for `gridlock-mode'.")

(define-minor-mode gridlock-mode
  "Navigate between and explicate fields."
  :init-value nil
  :lighter " GRID"
  :keymap gridlock-prefix-map
  (if gridlock-mode
      (progn
        (gridlock-define-prefix global-map)
        (gridlock-reset))
    t))

(provide 'gridlock)
;;; gridlock.el ends here
