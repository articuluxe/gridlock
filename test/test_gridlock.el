#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_gridlock.el --- gridlock test
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, February  2, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-04-06 08:51:03 dharms>
;; Modified by: Dan Harms
;; Keywords: test gridlock

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
;; Tests gridlock parsing.

;;

;;; Code:
(load-file "test/gridlock-test-common.el")
(require 'gridlock-csv)

(ert-deftest gridlock-test-search-forward ()
  "Test basic parsing and searching forward."
  (let (field)
    (cl-letf (((symbol-function 'gridlock--show-title-helper)
               (lambda(_)))
              ((symbol-function 'gridlock--hide-title-helper)
               (lambda(_))))
      (with-temp-buffer
        (insert "One,Two,Three,\n1,2,3\n4,5,6")
        (gridlock-csv-mode)
        ;; search forward from beginning
        (goto-char 1)
        (should (looking-at "One"))
        (should (eq (point) 1))
        (gridlock-goto-next-line)
        (should (looking-at "1"))
        (setq field (aref (ht-get gridlock-buffer-points (point)) 0))
        (should (eq (gridlock-get-bounds-begin field) 16))
        (should (eq (gridlock-get-bounds-end field) 17))
        (should (eq (gridlock-get-index field) 0))
        (should (string= (gridlock-get-str field) "1"))
        (setq field (aref (ht-get gridlock-buffer-points (point)) 1))
        (should (eq (gridlock-get-bounds-begin field) 18))
        (should (eq (gridlock-get-bounds-end field) 19))
        (should (eq (gridlock-get-index field) 1))
        (should (string= (gridlock-get-str field) "2"))
        (setq field (aref (ht-get gridlock-buffer-points (point)) 2))
        (should (eq (gridlock-get-bounds-begin field) 20))
        (should (eq (gridlock-get-bounds-end field) 21))
        (should (eq (gridlock-get-index field) 2))
        (should (string= (gridlock-get-str field) "3"))
        ;; search forward from middle
        (goto-char 5)
        (should (looking-at "Two"))
        (gridlock-goto-next-line)
        (should (looking-at "2"))
        ))))

(ert-deftest gridlock-test-search-backward ()
  "Test basic parsing and searching backward."
  (let (anchor fields field)
    (cl-letf (((symbol-function 'gridlock--show-title-helper)
               (lambda(_)))
              ((symbol-function 'gridlock--hide-title-helper)
               (lambda(_))))
      (with-temp-buffer
        (insert "One,Two,Three,\n1,2,3\n4,5,6")
        (gridlock-csv-mode)
        ;; search backward
        (goto-char 24)
        (should (looking-at "5"))
        (gridlock-goto-prev-line)
        (should (looking-at "2"))
        (setq anchor (gridlock--find-anchor-on-line (point)))
        (setq fields (gridlock-get-fields-at anchor))
        (should (eq (length fields) 3))
        ;; field 1
        (setq field (aref fields 0))
        (should (eq (gridlock-get-index field) 0))
        (should (string= (gridlock-get-str field) "1"))
        ;; (goto-char (car (gridlock-get-bounds field)))
        ;; (should (looking-at ","))
        (should (eq (gridlock-get-bounds-begin field) 16))
        (should (eq (gridlock-get-bounds-end field) 17))
        (should (eq (gridlock-get-index field) 0))
        (should (string= (gridlock-get-str field) "1"))
        ;; field 2
        (setq field (aref fields 1))
        (should (eq (gridlock-get-bounds-begin field) 18))
        (should (eq (gridlock-get-bounds-end field) 19))
        (should (eq (gridlock-get-index field) 1))
        (should (string= (gridlock-get-str field) "2"))
        ;; field 3
        (setq field (aref fields 2))
        (should (eq (gridlock-get-bounds-begin field) 20))
        (should (eq (gridlock-get-bounds-end field) 21))
        (should (eq (gridlock-get-index field) 2))
        (should (string= (gridlock-get-str field) "3"))

        ;; search backward from middle of field
        (goto-char 26)
        (should (looking-at "6"))
        (gridlock-goto-prev-line)
        (should (looking-at "3"))
        ))))

(defun gridlock-test-set-field-regexps (begin end)
  "Set the field regexes to BEGIN and END, respectively."
  (setq gridlock-field-regex-begin begin)
  (setq gridlock-field-regex-end end))

(defun gridlock-test-clear-field-regexps ()
  "Clear the field regexes."
  (setq gridlock-field-regex-begin nil)
  (setq gridlock-field-regex-end nil)
  (setq gridlock-csv-init-hook nil))

(ert-deftest gridlock-test-parse-delimited-fields ()
  "Test parsing delimited fields."
  (let (anchor fields field)
    (cl-letf (((symbol-function 'gridlock--show-title-helper)
               (lambda(_)))
              ((symbol-function 'gridlock--hide-title-helper)
               (lambda(_))))
      (with-temp-buffer
        (insert "One,Two,Three\n,,#1,2,3,\n#4,5,6\n,,,#7,8,9@,,,")
        (add-hook 'gridlock-csv-init-hook (apply-partially
                                           'gridlock-test-set-field-regexps
                                           "#" "@"))
        (gridlock-csv-mode)
        (goto-char 1)
        (should (looking-at "One"))

        (gridlock-goto-next-line)
        (setq anchor (gridlock--find-anchor-on-line (point)))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 0))
        (should (eq (gridlock-get-bounds-begin field) 18))
        (should (eq (gridlock-get-bounds-end field) 19))
        (should (eq (gridlock-get-index field) 0))
        (should (string= (gridlock-get-str field) "1"))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 1))
        (should (eq (gridlock-get-bounds-begin field) 20))
        (should (eq (gridlock-get-bounds-end field) 21))
        (should (eq (gridlock-get-index field) 1))
        (should (string= (gridlock-get-str field) "2"))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 2))
        (should (eq (gridlock-get-bounds-begin field) 22))
        (should (eq (gridlock-get-bounds-end field) 23))
        (should (eq (gridlock-get-index field) 2))
        (should (string= (gridlock-get-str field) "3"))

        (gridlock-goto-next-line)
        (setq anchor (gridlock--find-anchor-on-line (point)))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 0))
        (should (eq (gridlock-get-bounds-begin field) 26))
        (should (eq (gridlock-get-bounds-end field) 27))
        (should (eq (gridlock-get-index field) 0))
        (should (string= (gridlock-get-str field) "4"))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 1))
        (should (eq (gridlock-get-bounds-begin field) 28))
        (should (eq (gridlock-get-bounds-end field) 29))
        (should (eq (gridlock-get-index field) 1))
        (should (string= (gridlock-get-str field) "5"))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 2))
        (should (eq (gridlock-get-bounds-begin field) 30))
        (should (eq (gridlock-get-bounds-end field) 31))
        (should (eq (gridlock-get-index field) 2))
        (should (string= (gridlock-get-str field) "6"))

        (gridlock-goto-next-line)
        (setq anchor (gridlock--find-anchor-on-line (point)))
        (setq fields (gridlock-get-fields-at anchor))
        (should (eq (length fields) 3))
        (setq field (aref fields 0))
        (should (eq (gridlock-get-bounds-begin field) 36))
        (should (eq (gridlock-get-bounds-end field) 37))
        (should (eq (gridlock-get-index field) 0))
        (should (string= (gridlock-get-str field) "7"))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 1))
        (should (eq (gridlock-get-bounds-begin field) 38))
        (should (eq (gridlock-get-bounds-end field) 39))
        (should (eq (gridlock-get-index field) 1))
        (should (string= (gridlock-get-str field) "8"))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 2))
        (should (eq (gridlock-get-bounds-begin field) 40))
        (should (eq (gridlock-get-bounds-end field) 41))
        (should (eq (gridlock-get-index field) 2))
        (should (string= (gridlock-get-str field) "9"))
        (gridlock-test-clear-field-regexps)
        ))))

(ert-deftest gridlock-test-field-navigation ()
  "Test navigating between fields."
  (let (pt anchor field fields)
    (cl-letf (((symbol-function 'gridlock--show-title-helper)
               (lambda(_)))
              ((symbol-function 'gridlock--hide-title-helper)
               (lambda(_))))
      (with-temp-buffer
        (insert "One,Two,Three,\n#1,2,3@,o,\n4,5,6")
        (add-hook 'gridlock-csv-init-hook (apply-partially
                                           'gridlock-test-set-field-regexps
                                           "#"
                                           "@")) ;look up from 1st field
        (gridlock-csv-mode)
        (setq pt 17)
        (goto-char pt)
        (should (looking-at "1"))
        (should (eq (gridlock-get-index (gridlock-get-field-at pt)) 0))
        (setq anchor (gridlock--find-anchor-on-line pt))
        (setq fields (gridlock-get-fields-at anchor))
        (should (eq (length fields) 3))
        (setq field (aref fields 0))
        (should (string= (gridlock-get-str field) "1")) ;confirm we parsed field list
        (should (eq (gridlock--lookup-field-at-pos fields pt) 0))
        (should (eq (gridlock-get-index (gridlock-get-field-at pt)) 0))
        (should (eq (gridlock--get-next-field fields 0) 1))
        (should (not (gridlock--get-previous-field fields 0)))
        (should (not (gridlock-get-previous-field pt)))
        (should (eq (gridlock-get-index (gridlock-get-next-field pt)) 1))
        ;; look up from 2nd field
        (setq pt 19)
        (goto-char pt)
        (should (looking-at "2"))
        (setq anchor (gridlock--find-anchor-on-line pt))
        (setq fields (gridlock-get-fields-at anchor))
        (should (eq (length fields) 3))
        (setq field (aref fields 0))
        (should (string= (gridlock-get-str field) "1")) ;confirm we parsed field list
        (should (eq (gridlock--lookup-field-at-pos fields pt) 1))
        (should (eq (gridlock-get-index (gridlock-get-field-at pt)) 1))
        (should (eq (gridlock--get-next-field fields 1) 2))
        (should (eq (gridlock--get-previous-field fields 1) 0))
        (should (eq (gridlock-get-index (gridlock-get-previous-field pt)) 0))
        (should (eq (gridlock-get-index (gridlock-get-next-field pt)) 2))
        ;; look up from delimiter after 2nd field, should return the 2nd field
        (setq pt 20)
        (goto-char pt)
        (should (looking-at ","))
        (setq anchor (gridlock--find-anchor-on-line pt))
        (setq fields (gridlock-get-fields-at anchor))
        (should (eq (length fields) 3))
        (setq field (aref fields 0))
        (should (string= (gridlock-get-str field) "1")) ;confirm we parsed field list
        (should (eq (gridlock--lookup-field-at-pos fields pt) 1))
        (should (eq (gridlock-get-index (gridlock-get-field-at pt)) 1))
        (should (eq (gridlock--get-previous-field fields 1) 0))
        (should (eq (gridlock--get-next-field fields 1) 2))
        ;; look up after delimited range, should return nil
        (setq pt 23)
        (goto-char pt)
        (should (looking-at ","))
        (setq anchor (gridlock--find-anchor-on-line pt))
        (setq fields (gridlock-get-fields-at anchor))
        (should (eq (length fields) 3))
        (setq field (aref fields 0))
        (should (string= (gridlock-get-str field) "1")) ;confirm we parsed field list
        (should (not (gridlock--lookup-field-at-pos fields pt)))      ;should be nil
        (should (not (gridlock-get-field-at pt)))
        (setq field (gridlock-get-previous-field pt))
        (should (eq (gridlock-get-index field) 2))
        (should (string= "3" (gridlock-get-str field)))
        (should (not (gridlock-get-next-field pt)))
        (gridlock-test-clear-field-regexps)
        ))))

(ert-deftest gridlock-test-regex-capture ()
  "Test gridlock parsing with non-empty regex capture groups."
  (let (anchor fields field)
    (cl-letf (((symbol-function 'gridlock--show-title-helper) (lambda(_)))
              ((symbol-function 'gridlock--hide-title-helper) (lambda(_))))
      (with-temp-buffer
        (insert "One,Two,Three\nignore1,2,3me\nignore4,5,6me,,,")
        (defun gridlock-csv-test-hook ()
          (setq gridlock-anchor-regex "ore")
          ;; regexes specify capture groups
          (setq gridlock-field-regex-begin "e\\(1,\\)") ;start match at start of \1
          (setq gridlock-field-regex-end "\\(,3\\)me")) ;stop match at end of \1
        (unwind-protect
            (progn
              (add-hook 'gridlock-csv-init-hook 'gridlock-csv-test-hook)
              (gridlock-csv-mode)
              ;; go to before ignore regex
              (goto-char 15)
              (should (looking-at "ignore"))
              (setq fields (gridlock--parse-line (point)))
              (should (eq (length fields) 3))
              (should (string= (gridlock-get-str (aref fields 0))
                               "1"))
              (should (string= (gridlock-get-str (aref fields 1))
                               "2"))
              (should (string= (gridlock-get-str (aref fields 2))
                               "3"))
              ;; go to middle of ignore regex
              (goto-char 19)
              (should (looking-at "re"))
              (setq fields (gridlock--parse-line (point)))
              (should (eq (length fields) 3))
              (should (string= (gridlock-get-str (aref fields 0))
                               "1"))
              (should (string= (gridlock-get-str (aref fields 1))
                               "2"))
              (should (string= (gridlock-get-str (aref fields 2))
                               "3"))
              ;; go to end of ignore regex
              (goto-char 21)
              (should (looking-at "1"))
              (setq fields (gridlock--parse-line (point)))
              (should (eq (length fields) 3))
              (should (string= (gridlock-get-str (aref fields 0))
                               "1"))
              (should (string= (gridlock-get-str (aref fields 1))
                               "2"))
              (should (string= (gridlock-get-str (aref fields 2))
                               "3"))
              (setq gridlock-anchor-regex "^")
              (setq gridlock-field-regex-begin nil)
              (setq gridlock-field-regex-end nil))
          (remove-hook 'gridlock-csv-init-hook 'gridlock-csv-test-hook))
        ))))

(ert-run-tests-batch-and-exit (car argv))

;;; test_gridlock.el ends here
