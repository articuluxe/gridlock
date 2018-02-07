;;; gridlock.el --- navigate among delimited fields
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, January 26, 2018
;; Version: 0.1
;; Modified Time-stamp: <2018-02-07 12:26:09 dharms>
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

(defvar gridlock-buffer-metadata nil
  "Stores metadata about a cell.")

(defun gridlock--find-next ()
  "Find location of next anchor point."
  (let ((pt (point))
        (beg (search-forward-regexp gridlock-anchor-regex nil t)))
    (when (eq beg pt)
      (goto-char (1+ (point)))
      (setq beg (search-forward-regexp gridlock-anchor-regex nil t)))
    (when beg
      (unless (ht-contains? gridlock-buffer-points beg)
        (ht-set! gridlock-buffer-points beg
                 (save-match-data
                   (gridlock--parse-line beg)))
        (goto-char beg)))))

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
        (push (list pt idx str) lst)
        (setq idx (1+ idx))
        (setq pt (point)))
      (setq str (buffer-substring-no-properties pt end))
      ;; (message "post: end pt is %d, end is %d, str is %s" pt end str)
      (unless (string-empty-p str)
        (push (list pt idx str) lst))
      (nreverse lst))))


(defun gridlock-define-keys (map)
  "Define in keymap MAP bindings for `gridlock-mode'."
  nil)                                  ;todo

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
      (gridlock-define-prefix global-map)
    t))

(provide 'gridlock)
;;; gridlock.el ends here
