;;; mrc.el --- edit MARC 21 in several serializations -*- lexical-binding: t; -*-
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

(defun mrc-insert-ruler (arg)
  "Insert ruler to indicate character positions in controlfields.
If ARG, only print ARG character positions (or all if ARG is greater than 39)."
  (interactive "P")
  (let ((ruler "0....5....10...15...20...25...30...35..."))
    (if (and arg (< arg 39))
        (insert (substring ruler 0 (1+ arg)))
      (insert ruler))))

(defun mrc-read-datafield (mrk)
  "Create JSON object for a MARC datafield."
  (let* ((parts (split-string mrk "\\$\\$" t " *"))
         (tag-ind (replace-regexp-in-string "^=" "" (string-replace " " "" (car parts))))
         (tag (substring tag-ind 0 3))
         (ind1 (replace-regexp-in-string "[#\\]" " " (substring tag-ind 3 4)))
         (ind2 (replace-regexp-in-string "[#\\]" " " (substring tag-ind 4 5)))
         (subfields (cdr parts)))
    `((tag . ,tag)
       (ind1 . ,ind1)
       (ind2 . ,ind2)
       (subfields . ,(mrc-handle-subfields subfields)))))

(defun mrc-handle-subfields (subfield-strings)
  "Create marc:subfields given a marc-breaker string without tag and
indicators."
  (vconcat (mapcar #'mrc-read-subfield subfield-strings)))

(defun mrc-read-subfield (sf)
  "Create a JSON object for a MARC subfield."
  `((code . ,(substring sf 0 1))
    (value . ,(string-trim-left (substring sf 1)))))
;; (cons `((code . ,(substring sf 0 1)) (value . ,(string-trim-left (substring sf 1)))))

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
  `(controlfield
    ((tag . ,(alist-get 'tag controlfield)))
    ,(alist-get 'value controlfield)))

(defun mrc-line (arg)
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
                   (mrc-create-leader mrkline))
                  ((string-prefix-p "00" mrkline)
                   (mrc-create-controlfield mrkline))
                  (t (mrc-create-datafield mrkline)))))
        (dom-print mrc-el t t) (forward-line)))))

(defun mrc-create-leader (mrk)
  "Create a dom-element for a marc:leader."
  `(leader nil ,(substring mrk 4)))

(defun mrc-create-controlfield (mrk)
  "Create a dom-element for a marc:controlfield."
  (let ((tag (substring mrk 0 3))
        (value (substring mrk 4)))
    `(controlfield ((tag . ,tag)) ,value)))

(defun mrc-create-datafield (mrk)
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
      ,@(mrc-handle-subfields subfields))))

(defun mrc-handle-subfields (subfield-strings)
  "Create marc:subfields given a marc-breaker string without tag and
indicators."
  (let ((subfields))
    (dolist (sf subfield-strings subfields)
      (setq subfields
            (cons `(subfield ((code . ,(substring sf 0 1)))
                    ,(string-trim-left (substring sf 1)))
                  subfields)))
    (nreverse subfields)))

(defun mrc-doom-bind-keys ()
  "Bind default keys for Doom Emacs.")

(provide 'mrc)
;;; mrc.el ends here
