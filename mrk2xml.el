;;; mrk2xml.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Stefan Schuh
;;
;; Author: Stefan Schuh <stefan.schuh@obvsg.at>
;; Maintainer: Stefan Schuh <stefan.schuh@obvsg.at>
;; Created: Februar 27, 2024
;; Modified: Februar 27, 2024
;; Version: 0.0.1
;; Keywords: convenience data
;; Homepage: https://github.com/ss/mrk2xml
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  This package contains functions to convert text in MARC BREAKER format to
;;  MARC 21-XML.
;;
;;; Code:
(require 'dom)

(defun mrk2xml-line (arg)
  "Convert ARG lines of mark breaker into MARCXML."
  (interactive "p")
  (let ((numlines (or arg 1)))
    (dotimes (i numlines)
      (let*
          ((mrkline
            (delete-and-extract-region (line-beginning-position)
                                       (line-end-position)))
           (mrc-el
            (cond ((or (string-prefix-p "LDR" mrkline)
                       (string-prefix-p "000" mrkline))
                   (mrk2xml-create-leader mrkline))
                  ((string-prefix-p "00" mrkline)
                   (mrk2xml-create-controlfield mrkline))
                  (t (mrk2xml-create-datafield mrkline)))))
        (dom-print mrc-el t t) (forward-line)))))

(defun mrk2xml-create-leader (mrk)
  "Create a dom-element for a marc:leader."
  `(leader nil ,(substring mrk 4)))

(defun mrk2xml-create-controlfield (mrk)
  "Create a dom-element for a marc:controlfield."
  (let ((tag (substring mrk 0 3))
        (value (substring mrk 4)))
    `(controlfield ((tag . ,tag)) ,value)))

(defun mrk2xml-create-datafield (mrk)
  "Create dom-element for a marc:datafield given a line of mark-breaker."
  (let* ((parts (split-string mrk "\\$\\$" t " *"))
         (tag-ind (string-replace " " "" (car parts)))
         (tag (substring tag-ind 0 3))
         (ind1 (replace-regexp-in-string "[#\\]" " " (substring tag-ind 3 4)))
         (ind2 (replace-regexp-in-string "[#\\]" " " (substring tag-ind 4 5)))
         (subfields (cdr parts)))
    `(datafield ((tag . ,tag)
                 (ind1 . ,ind1)
                 (ind2 . ,ind2))
      ,@(mrk2xml-handle-subfields subfields))))

(defun mrk2xml-handle-subfields (subfield-strings)
  "Create marc:subfields given a marc-breaker string without tag and
indicators."
  (let ((subfields))
    (dolist (sf subfield-strings subfields)
      (setq subfields
            (cons `(subfield ((code . ,(substring sf 0 1)))
                    ,(string-trim-left (substring sf 1)))
                  subfields)))
    (nreverse subfields)))

(provide 'mrk2xml)
;;; mrk2xml.el ends here
