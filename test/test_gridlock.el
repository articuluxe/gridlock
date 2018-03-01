#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_gridlock.el --- gridlock test
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, February  2, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-03-01 17:24:34 dharms>
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
        (should (equal (gridlock-field-get-bounds field)
                       '(16 . 17)))
        (should (eq (gridlock-field-get-index field) 0))
        (should (string= (gridlock-field-get-str field) "1"))
        (setq field (aref (ht-get gridlock-buffer-points (point)) 1))
        (should (equal (gridlock-field-get-bounds field)
                       '(18 . 19)))
        (should (eq (gridlock-field-get-index field) 1))
        (should (string= (gridlock-field-get-str field) "2"))
        (setq field (aref (ht-get gridlock-buffer-points (point)) 2))
        (should (equal (gridlock-field-get-bounds field)
                       '(20 . 21)))
        (should (eq (gridlock-field-get-index field) 2))
        (should (string= (gridlock-field-get-str field) "3"))
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
        (should (eq (gridlock-field-get-index field) 0))
        (should (string= (gridlock-field-get-str field) "1"))
        ;; (goto-char (car (gridlock-field-get-bounds field)))
        ;; (should (looking-at ","))
        (should (equal (gridlock-field-get-bounds field)
                       '(16 . 17)))
        (should (eq (gridlock-field-get-index field) 0))
        (should (string= (gridlock-field-get-str field) "1"))
        ;; field 2
        (setq field (aref fields 1))
        (should (equal (gridlock-field-get-bounds field)
                       '(18 . 19)))
        (should (eq (gridlock-field-get-index field) 1))
        (should (string= (gridlock-field-get-str field) "2"))
        ;; field 3
        (setq field (aref fields 2))
        (should (equal (gridlock-field-get-bounds field)
                       '(20 . 21)))
        (should (eq (gridlock-field-get-index field) 2))
        (should (string= (gridlock-field-get-str field) "3"))

        ;; search backward from middle of field
        (goto-char 26)
        (should (looking-at "6"))
        (gridlock-goto-prev-line)
        (should (looking-at "3"))
        ))))

(ert-deftest gridlock-test-parse-delimited-fields ()
  "Test parsing delimited fields."
  (let (anchor field)
    (cl-letf (((symbol-function 'gridlock--show-title-helper)
               (lambda(_)))
              ((symbol-function 'gridlock--hide-title-helper)
               (lambda(_))))
      (with-temp-buffer
        (insert "One,Two,Three\n,,#1,2,3,\n#4,5,6\n,,,#7,8,9@,,,")
        (gridlock-csv-mode)
        (setq gridlock-field-regex-begin "#")
        (setq gridlock-field-regex-end "@")
        (goto-char 1)
        (should (looking-at "One"))

        (gridlock-goto-next-line)
        (setq anchor (gridlock--find-anchor-on-line (point)))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 0))
        (should (equal (gridlock-field-get-bounds field)
                       '(18 . 19)))
        (should (eq (gridlock-field-get-index field) 0))
        (should (string= (gridlock-field-get-str field) "1"))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 1))
        (should (equal (gridlock-field-get-bounds field)
                       '(20 . 21)))
        (should (eq (gridlock-field-get-index field) 1))
        (should (string= (gridlock-field-get-str field) "2"))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 2))
        (should (equal (gridlock-field-get-bounds field)
                       '(22 . 23)))
        (should (eq (gridlock-field-get-index field) 2))
        (should (string= (gridlock-field-get-str field) "3"))

        (gridlock-goto-next-line)
        (setq anchor (gridlock--find-anchor-on-line (point)))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 0))
        (should (equal (gridlock-field-get-bounds field)
                       '(26 . 27)))
        (should (eq (gridlock-field-get-index field) 0))
        (should (string= (gridlock-field-get-str field) "4"))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 1))
        (should (equal (gridlock-field-get-bounds field)
                       '(28 . 29)))
        (should (eq (gridlock-field-get-index field) 1))
        (should (string= (gridlock-field-get-str field) "5"))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 2))
        (should (equal (gridlock-field-get-bounds field)
                       '(30 . 31)))
        (should (eq (gridlock-field-get-index field) 2))
        (should (string= (gridlock-field-get-str field) "6"))

        (gridlock-goto-next-line)
        (setq anchor (gridlock--find-anchor-on-line (point)))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 0))
        (should (equal (gridlock-field-get-bounds field)
                       '(36 . 37)))
        (should (eq (gridlock-field-get-index field) 0))
        (should (string= (gridlock-field-get-str field) "7"))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 1))
        (should (equal (gridlock-field-get-bounds field)
                       '(38 . 39)))
        (should (eq (gridlock-field-get-index field) 1))
        (should (string= (gridlock-field-get-str field) "8"))
        (setq field (aref (ht-get gridlock-buffer-points anchor) 2))
        (should (equal (gridlock-field-get-bounds field)
                       '(40 . 41)))
        (should (eq (gridlock-field-get-index field) 2))
        (should (string= (gridlock-field-get-str field) "9"))
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
        (gridlock-csv-mode)
        (setq gridlock-field-regex-begin "#")
        (setq gridlock-field-regex-end "@")      ;; look up from 1st field
        (setq pt 17)
        (goto-char pt)
        (should (looking-at "1"))
        (should (eq (gridlock-field-get-index (gridlock-get-field-at pt)) 0))
        (setq anchor (gridlock--find-anchor-on-line pt))
        (setq fields (gridlock-get-fields-at anchor))
        (should (eq (length fields) 3))
        (setq field (aref fields 0))
        (should (string= (gridlock-field-get-str field) "1")) ;confirm we parsed field list
        (should (eq (gridlock--lookup-field-at-pos fields pt) 0))
        (should (eq (gridlock-field-get-index (gridlock-get-field-at pt)) 0))
        (should (eq (gridlock--get-next-field fields 0) 1))
        (should (not (gridlock--get-previous-field fields 0)))
        (should (not (gridlock-get-previous-field pt)))
        (should (eq (gridlock-field-get-index (gridlock-get-next-field pt)) 1))
        ;; look up from 2nd field
        (setq pt 19)
        (goto-char pt)
        (should (looking-at "2"))
        (setq anchor (gridlock--find-anchor-on-line pt))
        (setq fields (gridlock-get-fields-at anchor))
        (should (eq (length fields) 3))
        (setq field (aref fields 0))
        (should (string= (gridlock-field-get-str field) "1")) ;confirm we parsed field list
        (should (eq (gridlock--lookup-field-at-pos fields pt) 1))
        (should (eq (gridlock-field-get-index (gridlock-get-field-at pt)) 1))
        (should (eq (gridlock--get-next-field fields 1) 2))
        (should (eq (gridlock--get-previous-field fields 1) 0))
        (should (eq (gridlock-field-get-index (gridlock-get-previous-field pt)) 0))
        (should (eq (gridlock-field-get-index (gridlock-get-next-field pt)) 2))
        ;; look up from delimiter after 2nd field, should return the 2nd field
        (setq pt 20)
        (goto-char pt)
        (should (looking-at ","))
        (setq anchor (gridlock--find-anchor-on-line pt))
        (setq fields (gridlock-get-fields-at anchor))
        (should (eq (length fields) 3))
        (setq field (aref fields 0))
        (should (string= (gridlock-field-get-str field) "1")) ;confirm we parsed field list
        (should (eq (gridlock--lookup-field-at-pos fields pt) 1))
        (should (eq (gridlock-field-get-index (gridlock-get-field-at pt)) 1))
        (should (eq (gridlock--get-previous-field fields 1) 0))
        (should (eq (gridlock--get-next-field fields 1) 2))
        ;; look up after delimited range, should return nil
        (setq pt 23)
        (goto-char pt)
        (should (looking-at ","))
        (setq anchor (gridlock--find-anchor-on-line pt))
        (should (eq (length fields) 3))
        (setq fields (gridlock-get-fields-at anchor))
        (setq field (aref fields 0))
        (should (string= (gridlock-field-get-str field) "1")) ;confirm we parsed field list
        (should (not (gridlock--lookup-field-at-pos fields pt)))      ;should be nil
        (should (not (gridlock-get-field-at pt)))
        (should (not (gridlock-get-previous-field pt)))
        (should (not (gridlock-get-next-field pt)))
        ))))


(ert-run-tests-batch-and-exit (car argv))

;;; test_gridlock.el ends here
