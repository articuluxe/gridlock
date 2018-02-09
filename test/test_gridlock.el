#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_gridlock.el --- gridlock test
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, February  2, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-02-09 11:24:15 dharms>
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
(require 'gridlock)

(ert-deftest gridlock-test-search-forward ()
  "Test basic parsing and searching forward."
  (let (field)
    (with-temp-buffer
      (insert "One,Two,Three,\n1,2,3\n4,5,6")
      (gridlock-mode 1)
      ;; search forward from beginning
      (goto-char 1)
      (should (looking-at "One"))
      (should (eq (point) 1))
      (gridlock-goto-next-line)
      (should (looking-at "1"))
      (setq field (nth 0 (ht-get gridlock-buffer-points (point))))
      (should (equal (gridlock-field-get-bounds field)
                     '(16 . 17)))
      (should (eq (gridlock-field-get-index field) 0))
      (should (string= (gridlock-field-get-str field) "1"))
      (setq field (nth 1 (ht-get gridlock-buffer-points (point))))
      (should (equal (gridlock-field-get-bounds field)
                     '(18 . 19)))
      (should (eq (gridlock-field-get-index field) 1))
      (should (string= (gridlock-field-get-str field) "2"))
      (setq field (nth 2 (ht-get gridlock-buffer-points (point))))
      (should (equal (gridlock-field-get-bounds field)
                     '(20 . 21)))
      (should (eq (gridlock-field-get-index field) 2))
      (should (string= (gridlock-field-get-str field) "3"))
      ;; search forward from middle
      (goto-char 5)
      (should (looking-at "Two"))
      (gridlock-goto-next-line)
      (should (looking-at "1"))
      )))

(ert-deftest gridlock-test-search-backward ()
  "Test basic parsing and searching backward."
  (let (field)
    (with-temp-buffer
      (insert "One,Two,Three,\n1,2,3\n4,5,6")
      (gridlock-mode 1)
      ;; search backward
      (goto-char 22)
      (should (looking-at "4"))
      (gridlock-goto-prev-line)
      (should (looking-at "1"))

      (setq field (nth 0 (ht-get gridlock-buffer-points (point))))
      (should (equal (gridlock-field-get-bounds field)
                     '(16 . 17)))
      (should (eq (gridlock-field-get-index field) 0))
      (should (string= (gridlock-field-get-str field) "1"))
      (setq field (nth 1 (ht-get gridlock-buffer-points (point))))
      (should (equal (gridlock-field-get-bounds field)
                     '(18 . 19)))
      (should (eq (gridlock-field-get-index field) 1))
      (should (string= (gridlock-field-get-str field) "2"))
      (setq field (nth 2 (ht-get gridlock-buffer-points (point))))
      (should (equal (gridlock-field-get-bounds field)
                     '(20 . 21)))
      (should (eq (gridlock-field-get-index field) 2))
      (should (string= (gridlock-field-get-str field) "3"))

      ;; search backward from middle of field
      (goto-char 24)
      (should (looking-at "5"))
      (gridlock-goto-prev-line)
      (should (looking-at "4"))
      )))

(ert-deftest gridlock-test-parse-delimited-fields ()
  "Test parsing delimited fields."
  (let ((gridlock-field-regex-begin "#")
        (gridlock-field-regex-end "@")
        field)
    (with-temp-buffer
      (insert "One,Two,Three\n,,#1,2,3,\n#4,5,6\n,,,#7,8,9@,,,")
      (gridlock-mode 1)
      (goto-char 1)
      (should (looking-at "One"))

      (gridlock-goto-next-line)
      (setq field (nth 0 (ht-get gridlock-buffer-points (point))))
      (should (equal (gridlock-field-get-bounds field)
                     '(18 . 19)))
      (should (eq (gridlock-field-get-index field) 0))
      (should (string= (gridlock-field-get-str field) "1"))
      (setq field (nth 1 (ht-get gridlock-buffer-points (point))))
      (should (equal (gridlock-field-get-bounds field)
                     '(20 . 21)))
      (should (eq (gridlock-field-get-index field) 1))
      (should (string= (gridlock-field-get-str field) "2"))
      (setq field (nth 2 (ht-get gridlock-buffer-points (point))))
      (should (equal (gridlock-field-get-bounds field)
                     '(22 . 23)))
      (should (eq (gridlock-field-get-index field) 2))
      (should (string= (gridlock-field-get-str field) "3"))

      (gridlock-goto-next-line)
      (setq field (nth 0 (ht-get gridlock-buffer-points (point))))
      (should (equal (gridlock-field-get-bounds field)
                     '(26 . 27)))
      (should (eq (gridlock-field-get-index field) 0))
      (should (string= (gridlock-field-get-str field) "4"))
      (setq field (nth 1 (ht-get gridlock-buffer-points (point))))
      (should (equal (gridlock-field-get-bounds field)
                     '(28 . 29)))
      (should (eq (gridlock-field-get-index field) 1))
      (should (string= (gridlock-field-get-str field) "5"))
      (setq field (nth 2 (ht-get gridlock-buffer-points (point))))
      (should (equal (gridlock-field-get-bounds field)
                     '(30 . 31)))
      (should (eq (gridlock-field-get-index field) 2))
      (should (string= (gridlock-field-get-str field) "6"))

      (gridlock-goto-next-line)
      (setq field (nth 0 (ht-get gridlock-buffer-points (point))))
      (should (equal (gridlock-field-get-bounds field)
                     '(36 . 37)))
      (should (eq (gridlock-field-get-index field) 0))
      (should (string= (gridlock-field-get-str field) "7"))
      (setq field (nth 1 (ht-get gridlock-buffer-points (point))))
      (should (equal (gridlock-field-get-bounds field)
                     '(38 . 39)))
      (should (eq (gridlock-field-get-index field) 1))
      (should (string= (gridlock-field-get-str field) "8"))
      (setq field (nth 2 (ht-get gridlock-buffer-points (point))))
      (should (equal (gridlock-field-get-bounds field)
                     '(40 . 41)))
      (should (eq (gridlock-field-get-index field) 2))
      (should (string= (gridlock-field-get-str field) "9"))
      )))

(ert-deftest gridlock-test-field-navigation ()
  "Test navigating between fields."
  (let ((gridlock-field-regex-begin "#")
        (gridlock-field-regex-end "@")
        pt field fields)
    (with-temp-buffer
      (insert "One,Two,Three,\n#1,2,3@,o,\n4,5,6")
      (gridlock-mode 1)
      ;; look up from 1st field
      (setq pt 17)
      (goto-char pt)
      (should (looking-at "1"))
      (setq fields (gridlock-get-fields-at pt))
      (setq field (nth 0 fields))
      (should (string= (gridlock-field-get-str field) "1")) ;confirm we parsed field list
      (should (eq (gridlock-field-get-index (gridlock--lookup-field-at-pos fields pt)) 0))
      (should (eq (gridlock-field-get-index (gridlock-get-field-at pt)) 0))
      ;; look up from 2nd field
      (setq pt 19)
      (goto-char pt)
      (should (looking-at "2"))
      (setq fields (gridlock-get-fields-at pt))
      (setq field (nth 0 fields))
      (should (string= (gridlock-field-get-str field) "1")) ;confirm we parsed field list
      (should (eq (gridlock-field-get-index (gridlock--lookup-field-at-pos fields pt)) 1))
      (should (eq (gridlock-field-get-index (gridlock-get-field-at pt)) 1))
      ;; look up from delimiter after 2nd field, should return nil
      (setq pt 20)
      (goto-char pt)
      (should (looking-at ","))
      (setq fields (gridlock-get-fields-at pt))
      (setq field (nth 0 fields))
      (should (string= (gridlock-field-get-str field) "1")) ;confirm we parsed field list
      (should (not (gridlock--lookup-field-at-pos fields pt)))      ;should be nil
      (should (not (gridlock-get-field-at pt)))
      ;; look up after delimited range, should return nil
      (setq pt 23)
      (goto-char pt)
      (should (looking-at ","))
      (setq fields (gridlock-get-fields-at pt))
      (setq field (nth 0 fields))
      (should (string= (gridlock-field-get-str field) "1")) ;confirm we parsed field list
      (should (not (gridlock--lookup-field-at-pos fields pt)))      ;should be nil
      (should (not (gridlock-get-field-at pt)))
      )))


(ert-run-tests-batch-and-exit (car argv))

;;; test_gridlock.el ends here
