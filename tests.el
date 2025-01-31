;;; tests for mrc.el -*- lexical-binding: t; -*-
;;;
(require 'ert)
(require 'mrc)

(ert-deftest make-subfield ()
  (should (equal (mrc-read-subfield "aSFa")
              '((code . "a")
                (value . "SFa")))))

(ert-deftest read-datafield ()
  ;; MARCBreaker format
  (should (equal (mrc-read-datafield "=016\\\\$a3155702-8 $2DE-600")
                 '((tag . "016")
                   (ind1 . " ")
                   (ind2 . " ")
                   (subfields . [((code . "a") (value . "3155702-8"))
                                 ((code . "2") (value . "DE-600"))]))))
  (should (equal (mrc-read-datafield "016##$$a3155702-8$$2DE-600")
                 '((tag . "016")
                   (ind1 . " ")
                   (ind2 . " ")
                   (subfields . [((code . "a") (value . "3155702-8"))
                                 ((code . "2") (value . "DE-600"))])))))

(ert-deftest read-controlfield ()
  ;; MARCBreaker format
  (should (equal (mrc-read-controlfield "=000 00000cy\\\\a2200000zn\\4500")
                 '((tag . "000")
                   (value . "00000cy  a2200000zn 4500"))))
  ;; MARC documentation format
  (should (equal (mrc-read-controlfield "=000 00000cy##a2200000zn#4500")
                 '((tag . "000")
                   (value . "00000cy  a2200000zn 4500"))))
  ;; no spaces before value
  (should (equal (mrc-read-controlfield "007tu")
                 '((tag . "007")
                   (value . "tu"))))
  ;; multiple spaces before value
  (should (equal (mrc-read-controlfield "007    tu")
                 '((tag . "007")
                   (value . "tu"))))
  ;; space before value, significant space in value
  (should (equal (mrc-read-controlfield "007 cr uuuuu")
                 '((tag . "007")
                   (value . "cr uuuuu")))))

;;; tests.el ends here
