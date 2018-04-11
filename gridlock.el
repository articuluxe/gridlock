;;; gridlock.el --- navigate among delimited fields
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, January 26, 2018
;; Version: 0.1
;; Modified Time-stamp: <2018-04-11 08:34:06 dharms>
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
(require 'seq)
(require 'eieio)

(defgroup gridlock-group nil
  "Helper to navigate between fields, and explicate them."
  :group 'tools
  :prefix "gridlock")

(defvar gridlock-display-schemes nil
  "List of display schemes capable of being used by `gridlock-mode'.")

;; display schemes
;; echo
(defun gridlock-echo-on (str)
  "Show STR (the gridlock cell's title) to the user via the minibuffer."
  (message str))

(defun gridlock-echo-off (str)
  "Stop showing field info STR to the user."
  nil)

(push (cons "echo" (cons #'gridlock-echo-on #'gridlock-echo-off))
      gridlock-display-schemes)

;; postip
(declare-function pos-tip-show "pos-tip")
(defun gridlock-postip-on (str)
  "Show STR (the gridlock cell's title) to the user via `pos-tip-show'."
  (pos-tip-show str))

(defun gridlock-postip-off (str)
  "Stop showing field info STR to the user."
  nil)

(and (require 'pos-tip nil t)
     (display-graphic-p)
     (push (cons "pos-tip" (cons #'gridlock-postip-on #'gridlock-postip-off))
           gridlock-display-schemes))

;; popup
(declare-function popup-tip "popup")
(defun gridlock-popup-on (str)
  "Show STR (the gridlock cell's title) to the user via `popup-tip'."
  (popup-tip str))

(defun gridlock-popup-off (str)
  "Stop showing field info STR to the user."
  nil)

(when (require 'popup nil t)
  (push (cons "popup" (cons #'gridlock-popup-on #'gridlock-popup-off))
        gridlock-display-schemes))

;; quickpeek
(declare-function quick-peek-show "quick-peek")
(defun gridlock-quickpeek-on (str)
  "Show STR (the gridlock cell's title) to the user via `quick-peek-show'."
  (quick-peek-show str))

(declare-function quick-peek-hide "quick-peek")
(defun gridlock-quickpeek-off (str)
  "Stop showing field info STR to the user."
  (quick-peek-hide))

(when (require 'quick-peek nil t)
  (push (cons "quick-peek" (cons #'gridlock-quickpeek-on #'gridlock-quickpeek-off))
        gridlock-display-schemes))


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

(defvar gridlock-display-funcs nil
  "A cons cell (on . off) to control display of the current cell.
The car ON is a function that turns on display of the current
cell, and the cadr off is a function that turns off that display.")
(defvar gridlock-last-display-scheme nil
  "The last used display scheme.")

;; metadata
(defvar gridlock-metadata-get-title-func nil
  "Given a record FIELD, return its grid's title.
Function takes one parameter, field.")
(defvar gridlock-metadata-gather-titles-func nil
  "Return a list of all the field headings.
Takes one parameter: FIELDS, a list of fields.")

(defvar-local gridlock-current-field nil "The current field.")

;; field class
(defclass gridlock-field ()
  ((index :initarg :index
          :type (integer 0 *)
          :documentation "The index of the field on its line."
          )
   (string :initarg :string
           :initform ""
           :type string
           :documentation "The string value of the field."
           )
   (begin :initarg :begin
          :type (integer 0 *)
          :documentation "The field's beginning buffer position."
          )
   (end :initarg :end
        :type (integer 0 *)
        :documentation "The field's ending buffer position."
        )
   )
  "A field of interest.")

;; silence compiler warning
(eval-when-compile (defvar gridlock-field))

;; field accessors
(defmethod gridlock-get-index ((field gridlock-field))
  "Get the field's index in its containing line."
  (oref field index))

(defmethod gridlock-get-str ((field gridlock-field))
  "Get the field's string content."
  (oref field string))

(defmethod gridlock-get-bounds-begin ((field gridlock-field))
  "Get the field's beginning boundary."
  (oref field begin))

(defmethod gridlock-get-bounds-end ((field gridlock-field))
  "Get the field's ending boundary."
  (oref field end))

(defmacro gridlock-create-field-macro (&rest args)
  "Create a `gridlock-field' with ARGS, parameterized on Emacs version.
This is necessary as constructor arguments changed in Emacs 25."
  (if (version< emacs-version "25")
      `(apply gridlock-field "gridlock-field" ,@args)
    `(apply gridlock-field ,@args)))

(defun gridlock-create-field-compat (&rest args)
  "Create a `gridlock-field' with ARGS."
  (gridlock-create-field-macro args))

;; field creator
(defvar gridlock-create-field-func
  #'gridlock-create-field-compat
  "The default function to create a gridlock field.")

;;;###autoload
(defun gridlock-show-title ()
  "Show info about the current field."
  (interactive)
  (if gridlock-current-field
      (gridlock--show-title-helper gridlock-current-field)
    (let* ((pt (point))
           (anchor (gridlock--find-anchor-on-line pt))
           (fields (gridlock-get-fields-at anchor))
           (idx (gridlock--lookup-field-at-pos fields pt)))
      (and fields idx
           (setq gridlock-current-field (aref fields idx))
           (gridlock--show-title-helper gridlock-current-field)))))

(defun gridlock--show-title-helper (field)
  "Show info about the field FIELD."
  (when field
    (let ((func (car gridlock-display-funcs))
          (title (gridlock-field-get-title field)))
      (and func title
           (funcall func title)))))

;;;###autoload
(defun gridlock-hide-title ()
  "Stop showing the current field."
  (interactive)
  (gridlock--hide-title-helper gridlock-current-field))

(defun gridlock--hide-title-helper (field)
  "Hide info about the field FIELD."
  (when field
    (let ((func (cdr gridlock-display-funcs)))
      (and func
           (funcall func (gridlock-field-get-title field))))))

;;;###autoload
(defun gridlock-choose-display-scheme ()
  "Choose a display scheme for `gridlock-mode' cells.
The schemes are selected from `gridlock-display-schemes'."
  (interactive)
  (let ((scheme (completing-read "Display scheme: " gridlock-display-schemes nil t)))
    (gridlock--hide-title-helper gridlock-current-field)
    (and
     (gridlock-activate-display-scheme scheme)
     (setq gridlock-last-display-scheme scheme)
     (gridlock--show-title-helper gridlock-current-field))))

(defun gridlock-activate-display-scheme (name)
  "Activate the display scheme named by NAME."
  (let ((elt (assoc name gridlock-display-schemes)))
    (and
     elt
     (setq gridlock-display-funcs (cdr elt)))))

(defun gridlock-activate-one-of-display-schemes (lst)
  "Iterate through LST trying to activate the named display schemes.
LST is a list of display scheme names."
  (or (and gridlock-last-display-scheme
           (gridlock-activate-display-scheme gridlock-last-display-scheme))
      (seq-find #'gridlock-activate-display-scheme lst nil)))

;;;###autoload
(defun gridlock-jump-to-field-by-index ()
  "Jump point to the field according to index specified by the user."
  (interactive)
  (let* ((pt (point))
         (anchor (gridlock--find-anchor-on-line pt))
         (fields (gridlock-get-fields-at anchor))
         (total (length fields))
         (idx 0)
         c)
    (if (eq total 0)
        (error "No fields on current line")
      (unwind-protect
          (catch 'exit
            (while (or (< idx 1) (> idx total))
              (setq c (read-from-minibuffer
                       (format "Jump to field (must be between 1 and %d, 'q' to cancel): "
                               total)))
              (setq idx (cond ((string= c "q")
                               (throw 'exit nil))
                              ((number-or-marker-p (read c))
                               (read c))
                              (t 0))))
            (gridlock--hide-title-helper gridlock-current-field)
            (setq gridlock-current-field (aref fields (1- idx)))
            (goto-char (gridlock-get-bounds-begin gridlock-current-field)))
        (gridlock--show-title-helper gridlock-current-field)))))

(defun gridlock-jump-to-field-by-heading ()
  "Jump point to the field specified by a heading selected by the user."
  (interactive)
  (let* ((pt (point))
         (anchor (gridlock--find-anchor-on-line pt))
         (fields (gridlock-get-fields-at anchor))
         (total (length fields))
         (idx 0)
         (i 0)
         headings choice element)
    (if (eq total 0)
        (error "No fields on current line")
      (when gridlock-metadata-gather-titles-func
        (dolist (elt (funcall gridlock-metadata-gather-titles-func fields))
          (setq headings (cons (cons elt i)
                               headings))
          (setq i (1+ i))))
      (setq headings (nreverse headings))
      (unwind-protect
          (catch 'exit
            (setq choice (completing-read "Jump to heading: " headings nil t))
            (setq element (assoc choice headings))
            (if element
                (setq idx (cdr element))
              (throw 'exit nil))
            (gridlock--hide-title-helper gridlock-current-field)
            (setq gridlock-current-field (aref fields idx))
            (goto-char (gridlock-get-bounds-begin gridlock-current-field)))
        (gridlock--show-title-helper gridlock-current-field)))))

(defun gridlock-jump-to-field-by-content ()
  "Jump point to the field specified by content selected by the user."
  (interactive)
  (let* ((pt (point))
         (anchor (gridlock--find-anchor-on-line pt))
         (fields (gridlock-get-fields-at anchor))
         (total (length fields))
         (idx 0)
         (i 0)
         elt choices choice element)
    (if (eq total 0)
        (error "No fields on current line")
      (while (< i total)
        (setq elt (aref fields i))
        (setq choices (cons (cons (gridlock-get-str elt) i) choices))
        (setq i (1+ i)))
      (setq choices (nreverse choices))
      (unwind-protect
          (catch 'exit
            (setq choice (completing-read "Jump to field: " choices nil t))
            (setq element (assoc choice choices))
            (if element
                (setq idx (cdr element))
              (throw 'exit nil))
            (gridlock--hide-title-helper gridlock-current-field)
            (setq gridlock-current-field (aref fields idx))
            (goto-char (gridlock-get-bounds-begin gridlock-current-field)))
        (gridlock--show-title-helper gridlock-current-field)))))

(defun gridlock--find-anchor-on-line (pt)
  "Find location, if any, of anchor point on line containing PT."
  (let (anchor fields)
    (save-excursion
      (goto-char pt)
      (goto-char (line-beginning-position))
      (and (setq anchor (search-forward-regexp gridlock-anchor-regex (line-end-position) t))
           (not (gridlock-get-fields-at anchor))
           (setq fields (gridlock--check-anchor anchor))
           (gridlock--on-anchor-found anchor fields))
      anchor)))

(defun gridlock-reset ()
  "Reset gridlock state in current buffer."
  (interactive)
  (gridlock--hide-title-helper gridlock-current-field)
  (ht-clear! gridlock-buffer-points)
  (setq gridlock-current-field nil))

;;;###autoload
(defun gridlock-goto-next-line ()
  "Advance point to next anchor point."
  (interactive)
  (let ((prior gridlock-current-field)
        (pt (gridlock--find-next-line)))
    (if pt
        (progn
          (gridlock--hide-title-helper prior)
          (goto-char pt)
          (gridlock--show-title-helper gridlock-current-field))
      (message "No further lines.")
      (gridlock--show-title-helper gridlock-current-field))))

(defun gridlock--find-next-line ()
  "Return location, if any, of next anchor point."
  (let* ((pt (point))
         (line (line-beginning-position))
         (anchor (gridlock--find-anchor-on-line pt))
         (fields (gridlock-get-fields-at anchor))
         (idx (gridlock--lookup-field-at-pos fields pt))
         beg)
    (save-excursion
      (setq beg (search-forward-regexp gridlock-anchor-regex nil t))
      (when (and beg (save-excursion
                       (goto-char beg)
                       (eq line (line-beginning-position))))
        (goto-char (1+ (point)))
        (setq beg (search-forward-regexp gridlock-anchor-regex nil t)))
      (catch 'found
        (while beg
          (setq fields (gridlock-get-fields-at beg))
          (unless fields
            (and
             (setq fields (gridlock--check-anchor beg))
             (gridlock--on-anchor-found beg fields)))
          (if fields
              (progn
                (setq idx (if (and idx (< idx (length fields)))
                              idx
                            (1- (length fields))))
                (setq gridlock-current-field (aref fields idx))
                (throw 'found (gridlock-get-bounds-begin gridlock-current-field)))
            (setq pt beg)
            (goto-char (1+ beg))
            (setq beg
                  (if (eq (point) beg)
                      nil
                    (search-forward-regexp gridlock-anchor-regex nil t)))))))))

;;;###autoload
(defun gridlock-goto-prev-line ()
  "Advance point to prior anchor point."
  (interactive)
  (let ((prior gridlock-current-field)
        (pt (gridlock--find-prev-line)))
    (if pt
        (progn
          (gridlock--hide-title-helper prior)
          (goto-char pt)
          (gridlock--show-title-helper gridlock-current-field))
      (message "No prior lines.")
      (gridlock--show-title-helper gridlock-current-field))))

(defun gridlock--find-prev-line ()
  "Return location, if any, of prior anchor point."
  (let* ((pt (point))
         (anchor (gridlock--find-anchor-on-line pt))
         (fields (gridlock-get-fields-at anchor))
         (idx (gridlock--lookup-field-at-pos fields pt))
         beg)
    (when anchor
      (setq pt anchor))
    (save-excursion
      (goto-char pt)
      (setq beg (search-backward-regexp gridlock-anchor-regex nil t))
      (when beg (setq beg (match-end 0)))
      (when (eq beg pt)
        (goto-char (1- (point)))
        (setq beg (search-backward-regexp gridlock-anchor-regex nil t))
        (when beg (setq beg (match-end 0))))
      (catch 'found
        (while beg
          (setq fields (gridlock-get-fields-at beg))
          (unless fields
            (and
             (setq fields (gridlock--check-anchor beg))
             (gridlock--on-anchor-found beg fields)))
          (if fields
              (progn
                (setq idx (if (and idx (< idx (length fields)))
                              idx
                            (1- (length fields))))
                (setq gridlock-current-field (aref fields idx))
                (throw 'found (gridlock-get-bounds-begin gridlock-current-field)))
            (setq pt beg)
            (goto-char (1- beg))
            (setq beg (if (eq (point) beg)
                          nil
                        (search-backward-regexp gridlock-anchor-regex nil t)))
            (when beg (setq beg (match-end 0)))))))))

;;;###autoload
(defun gridlock-goto-line-start ()
  "Move point to the beginning of the first field, if any, on the current line."
  (interactive)
  (let ((prior gridlock-current-field)
        (pt (gridlock--find-first-field)))
    (if pt
        (progn
          (gridlock--hide-title-helper prior)
          (goto-char pt)
          (gridlock--show-title-helper gridlock-current-field))
      (message "No fields on current line."))))

(defun gridlock--find-first-field ()
  "Return location, if any, of first field on current line."
  (let* ((pt (point))
         (anchor (gridlock--find-anchor-on-line pt))
         (fields (gridlock-get-fields-at anchor)))
    (when (> (length fields) 0)
      (setq gridlock-current-field (aref fields 0))
      (gridlock-get-bounds-begin gridlock-current-field))))

;;;###autoload
(defun gridlock-goto-line-end ()
  "Move point to the beginning of the last field, if any, on the current line."
  (interactive)
  (let ((prior gridlock-current-field)
        (pt (gridlock--find-last-field)))
    (if pt
        (progn
          (gridlock--hide-title-helper prior)
          (goto-char pt)
          (gridlock--show-title-helper gridlock-current-field))
      (message "No fields on current line."))))

(defun gridlock--find-last-field ()
  "Return location, if any, of last field on current line."
  (let* ((pt (point))
         (anchor (gridlock--find-anchor-on-line pt))
         (fields (gridlock-get-fields-at anchor))
         (len (length fields)))
    (when (> len 0)
      (setq gridlock-current-field (aref fields (1- len)))
      (gridlock-get-bounds-begin gridlock-current-field))))

;;;###autoload
(defun gridlock-goto-next-field ()
  "Move point to the next field, if any, on current line."
  (interactive)
  (let ((prior gridlock-current-field)
        (fld (gridlock-get-next-field (point))))
    (if fld
        (progn
          (gridlock--hide-title-helper prior)
          (setq gridlock-current-field fld)
          (goto-char (gridlock-get-bounds-begin fld))
          (gridlock--show-title-helper gridlock-current-field))
      ;; todo allow wrap-around to next line
      (message "No further fields on this line.")
      (gridlock--show-title-helper gridlock-current-field))))

;;;###autoload
(defun gridlock-goto-previous-field ()
  "Move point to the previous field, if any, on current line."
  (interactive)
  (let ((prior gridlock-current-field)
        (fld (gridlock-get-previous-field (point))))
    (if fld
        (progn
          (gridlock--hide-title-helper prior)
          (setq gridlock-current-field fld)
          (goto-char (gridlock-get-bounds-begin fld))
          (gridlock--show-title-helper gridlock-current-field))
      (message "No further fields on this line.")
      (gridlock--show-title-helper gridlock-current-field))))

(defun gridlock--check-anchor (anchor)
  "Validate what fields can be parsed from anchor point ANCHOR.
If none, return nil.  If some, return those fields."
  (let ((fields (gridlock--parse-line anchor)))
    (if (> (length fields) 0) fields nil)))

(defun gridlock--on-anchor-found (anchor fields)
  "Mark that anchor point ANCHOR is associated with FIELDS."
  (unless (ht-contains? gridlock-buffer-points anchor)
    (ht-set! gridlock-buffer-points anchor fields)))

(defun gridlock--parse-line (beg)
  "Parse the format of the line beginning at BEG."
  (save-excursion
    (save-match-data
      (let ((idx 0)
            pt end lst str vec)
        (goto-char beg)
        (setq end (line-end-position))
        (and gridlock-field-regex-begin
             (not (string-empty-p gridlock-field-regex-begin))
             (goto-char (line-beginning-position))
             (search-forward-regexp gridlock-field-regex-begin end t)
             (setq beg (or (match-beginning 1)
                           (match-end 0)
                           beg))
             (goto-char beg))
        (and gridlock-field-regex-end
             (not (string-empty-p gridlock-field-regex-end))
             (search-forward-regexp gridlock-field-regex-end end t)
             (setq end (or (match-end 1)
                           (match-beginning 0)
                           end))
             ;; (message "pre: beg %d end %d" beg end)
             )
        (goto-char beg)
        (setq pt beg)
        (while (search-forward-regexp gridlock-field-delimiter end t)
          (setq str (buffer-substring-no-properties
                     pt (match-beginning 0)))
          ;; (message "during: bounds: (%d,%d) end is %d, str is %s" pt (match-beginning 0) end str)
          (push
           (funcall gridlock-create-field-func
                    :begin pt :end (match-beginning 0) :index idx :string str)
           lst)
          (setq idx (1+ idx))
          (setq pt (point)))
        (setq str (string-trim (buffer-substring-no-properties pt end)))
        ;; (message "post: end pt is %d, end is %d, str is %s" pt end str)
        (push
         (funcall gridlock-create-field-func
                  :begin pt :end (+ pt (length str)) :index idx :string str)
         lst)
        (setq vec (make-vector (length lst) nil))
        (dolist (elt (nreverse lst) vec)
          (aset vec (gridlock-get-index elt) elt)
          )))))

(defun gridlock-field-get-title (field)
  "Given a record FIELD, return its grid's title."
  (and gridlock-metadata-get-title-func
       (funcall gridlock-metadata-get-title-func field)))

(defun gridlock-get-field-at (point)
  "Return the field, if any, that POINT lies on."
  (let* ((anchor (gridlock--find-anchor-on-line point))
         (fields (gridlock-get-fields-at anchor))
         (idx (gridlock--lookup-field-at-pos fields point)))
    (and idx (aref fields idx))))

(defun gridlock-get-fields-at (anchor)
  "Return the fields, if any, existing on line associated with ANCHOR.."
  (ht-get gridlock-buffer-points anchor))

(defun gridlock--lookup-field-at-pos (fields pt &optional lax)
  "Lookup in field list FIELDS what index, if any, PT lies within.
If LAX is non-nil, PT is allowed to lie before the first, or
after the last, field."
  (let ((len (length fields))
        (i 0)
        elt min max)
    (catch 'found
      (progn
        (while (< i len)
          (setq elt (aref fields i))
          (setq min (gridlock-get-bounds-begin elt))
          (setq max (gridlock-get-bounds-end elt))
          (and (or (>= pt min)
                   (and lax (eq i 0)))
               (or (<= pt max)
                   (and lax (eq i (1- len))))
               (throw 'found i))
          (setq i (1+ i)))
        nil))))

(defun gridlock-get-previous-field (point)
  "Return the field, if any, that precedes that at POINT."
  (let* ((anchor (gridlock--find-anchor-on-line point))
         (fields (gridlock-get-fields-at anchor))
         (idx (gridlock--lookup-field-at-pos fields point))
         prev)
    (if idx
        (progn
          (and
           (setq prev (gridlock--get-previous-field fields idx))
           (aref fields prev)))
      (and fields
           (>= point (gridlock-get-bounds-end (aref fields (1- (length fields)))))
           (setq idx (gridlock--lookup-field-at-pos fields point 'lax))
           (aref fields idx)))))

(defun gridlock-get-next-field (point)
  "Return the field, if any, following that at POINT."
  (let* ((anchor (gridlock--find-anchor-on-line point))
         (fields (gridlock-get-fields-at anchor))
         (idx (gridlock--lookup-field-at-pos fields point))
         next)
    (if idx
        (progn
          (and
           (setq next (gridlock--get-next-field fields idx))
           (aref fields next)))
      (and fields
           (<= point (gridlock-get-bounds-begin (aref fields 0)))
           (setq idx (gridlock--lookup-field-at-pos fields point 'lax))
           (aref fields idx)))))

(defun gridlock--get-next-field (fields idx)
  "Return the next field from FIELDS after IDX, if it exists."
  (let ((i (1+ idx)))
    (and (< i (length fields)) i)))

(defun gridlock--get-previous-field (fields idx)
  "Return the previous field from FIELDS before IDX, if it exists."
  (when (and (<= idx (length fields))
             (> idx 0))
    (1- idx)))

(defun gridlock-define-keys (map)
  "Define in keymap MAP bindings for `gridlock-mode'."
  (define-key map (kbd "<down>") #'gridlock-goto-next-line)
  (define-key map (kbd "n") #'gridlock-goto-next-line)
  (define-key map (kbd "<up>") #'gridlock-goto-prev-line)
  (define-key map (kbd "p") #'gridlock-goto-prev-line)
  (define-key map (kbd "<left>") #'gridlock-goto-previous-field)
  (define-key map (kbd "b") #'gridlock-goto-previous-field)
  (define-key map (kbd "<right>") #'gridlock-goto-next-field)
  (define-key map (kbd "f") #'gridlock-goto-next-field)
  (define-key map "a" #'gridlock-goto-line-start)
  (define-key map "e" #'gridlock-goto-line-end)
  (define-key map (kbd "SPC") #'gridlock-show-title)
  (define-key map "`" #'gridlock-choose-display-scheme)
  (define-key map "v" #'gridlock-jump-to-field-by-index)
  (define-key map "j" #'gridlock-jump-to-field-by-heading)
  (define-key map "g" #'gridlock-jump-to-field-by-content)
  )

(defvar gridlock-mode-map
  (let ((map (make-sparse-keymap)))
    (gridlock-define-keys map)
    map)
  "Keymap for `gridlock-mode'.")

(provide 'gridlock)
;;; gridlock.el ends here
