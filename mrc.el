;;; mrc.el --- Edit MARC 21 in several serializations -*- lexical-binding: t; -*-
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
;;  MARC21-XML or JSON.
;;
;;; Code:
(require 'dom)
(require 'json)

(defun mrc-insert-ruler (arg)
  "Insert ruler to indicate character positions in controlfields.
If ARG, only print ARG character positions (or all if ARG is greater than 39)."
  (interactive "P")
  (let ((ruler "0....5....10...15...20...25...30...35..."))
    (if (and arg (< arg 39))
        (insert (substring ruler 0 (1+ arg)))
      (insert ruler))))

(defun mrc-read-datafield (mrk)
  "Create JSON object form MRK (a MARCBreaker datafield)."
  (let* ((parts (split-string mrk "\\$\\$" t " *"))
         (tag-ind (replace-regexp-in-string "^=" "" (string-replace " " "" (car parts))))
         (tag (substring tag-ind 0 3))
         (ind1 (replace-regexp-in-string "[#\\]" " " (substring tag-ind 3 4)))
         (ind2 (replace-regexp-in-string "[#\\]" " " (substring tag-ind 4 5)))
         (subfields (cdr parts)))
    `((tag . ,tag)
       (ind1 . ,ind1)
       (ind2 . ,ind2)
       (subfields . ,(vconcat (mapcar #'mrc-read-subfield subfields))))))

(defun mrc-read-subfield (sf)
  "Create a JSON object for a MARC subfield."
  `((code . ,(substring sf 0 1))
    (value . ,(string-trim-left (substring sf 1)))))

(defun mrc-read-controlfield (mrk)
  "Create JSON object for a MARC controlfield."
  `((tag . ,(substring mrk 0 3))
    (value . ,(string-trim-left (substring mrk 3)))))

(defun mrc-pretty-print-json (field)
  "Serialize and pretty print a FIELD."
  (with-temp-buffer
           (insert (json-serialize field))
           (json-pretty-print (point-min) (point-max))
           (buffer-string)))

(defun mrc-insert-json (field pretty)
  "Insert FIELD at point as JSON.
If PRETTY is non-nil, pretty print the output."
  (if pretty
      (insert (mrc-pretty-print-json field))
    (insert (json-serialize field))))

(defun mrc-field->dom (field)
  "Convert FIELD into dom object."
  (if (alist-get 'subfields field)
      (mrc-datafield->dom field)
    (mrc-controlfield->dom field)))

(defun mrc-datafield->dom (datafield)
  "Convert DATAFIELD to dom object."
  `(datafield ((tag . ,(alist-get 'tag datafield))
               (ind1 . ,(alist-get 'ind1 datafield))
               (ind2 . ,(alist-get 'ind2 datafield)))
    ,@(mapcar #'mrc-subfield->dom (alist-get 'subfields datafield))))

(defun mrc-subfield->dom (subfield)
  "Convert SUBFIELD to dom object."
  `(subfield
    ((code . , (alist-get 'code subfield)))
    ,(alist-get 'value subfield)))

(defun mrc-controlfield->dom (controlfield)
  "Convert CONTROLFIELD to dom object."
  (let ((tag (alist-get 'tag controlfield))
        (value (alist-get 'value controlfield)))
    (if (or  (string= tag "LDR") (string= tag "000"))
        `(leader nil ,value)
        `(controlfield
          ((tag . ,(alist-get 'tag controlfield)))
          ,(alist-get 'value controlfield)))))

(defun mrc-convert-line (arg format)
  "Convert ARG lines of mark breaker into FORMAT.
FORMAT can be `:xml' for MARC-XML or `:json' for JSON."
  (let ((numlines (or arg 1)))
    (dotimes (_i numlines)
      (let*
          ((mrkline
            (delete-and-extract-region (line-beginning-position)
                                       (line-end-position)))
           (mrc-object
            (if (string-match-p (regexp-quote "$$") mrkline)
                (mrc-read-datafield mrkline)
              (mrc-read-controlfield mrkline))))
        (cond ((eq format :xml)
               (dom-print (mrc-field->dom mrc-object) t t))
              ((eq format :json) (mrc-insert-json mrc-object t))
              (t (progn
                   (message "Unknown format.")
                   (insert mrkline))))
        (forward-line)))))

(defun mrc-mrk->xml (arg)
  "Convert ARG lines of MARCBreaker into MARC-XML."
  (interactive "p")
  (mrc-line arg :xml))

(defun mrc-mrk->json (arg)
  "Convert ARG lines of MARCBreaker into JSON."
  (interactive "p")
  (mrc-line arg :json))

(defun mrc-doom-bind-keys ()
  "Bind default keys for Doom Emacs.")

(provide 'mrc)
;;; mrc.el ends here
