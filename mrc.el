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

;;; convenience functions
(defun mrc-insert-ruler (arg)
  "Insert ruler to indicate character positions in controlfields.
If ARG, only print ARG character positions (or all if ARG is greater than 39)."
  (interactive "P")
  (let ((ruler "0....5....10...15...20...25...30...35..."))
    (if (and arg (< arg 39))
        (insert (substring ruler 0 (1+ arg)))
      (insert ruler))))

;;; Commands to convert buffer contents
(defun mrc-mrk->xml-line (arg)
  "Convert ARG lines of MARCBreaker into MARC-XML."
  (interactive "p")
  (mrc-convert-line arg 'xml))

(defun mrc-mrk->json-line (arg)
  "Convert ARG lines of MARCBreaker into JSON."
  (interactive "p")
  (mrc-convert-line arg 'json))

(defun mrc-mrk->json-region (start end)
  "Convert all lines in region to JSON."
  (interactive "r")
  (mrc-convert-region start end 'json))

(defun mrc-mrk->xml-region (start end)
  "Convert all lines in region to MARC-XML."
  (interactive "r")
  (mrc-convert-region start end 'xml))

(defun mrc-mrk->xml-subfields-only ()
  "Convert a line only containing subfields to MARC-XML."
  (interactive)
  (mrc-convert-subfields-only 'xml))

(defun mrc-mrk->json-subfields-only ()
  "Convert a line only containing subfields to JSON."
  (interactive)
  (mrc-convert-subfields-only 'json))

(defun mrc-convert-region (start end format)
  "Convert all lines in REGION to FORMAT."
  (let ((numlines (count-lines (point-min) (point-max))))
    (goto-char start)
    (mrc-convert-line numlines format)))

(defun mrc-convert-line (arg format)
  "Convert ARG lines of mark breaker into FORMAT.
FORMAT can be `xml' for MARC-XML or `json' for JSON."
  (let ((numlines (or arg 1)))
    (dotimes (_i numlines)
      (let*
          ((mrkline
            (substring-no-properties    ; remove text properties from string
             (delete-and-extract-region (line-beginning-position)
                                        (line-end-position))))
           (mrc-object (mrc-read-field mrkline)))
        (cond ((eq format 'xml)
               (dom-print (mrc-field->dom mrc-object) t t))
              ((eq format 'json) (mrc-insert-json mrc-object t))
              (t (progn
                   (message "Unknown format.")
                   (insert mrkline))))
        (forward-line)))))

(defun mrc-convert-subfields-only (format)
  "Convert only the subfields in a line into FORMAT."
  (let* ((mrkline
          (substring-no-properties       ; remove text properties from string
           (delete-and-extract-region (line-beginning-position)
                                      (line-end-position))))
         (subfield-strings (split-string (string-trim-left mrkline "[^$]*") "\\$\\$" t " *"))
         (subfields (mapcar #'mrc-read-subfield subfield-strings)))
    (mapc
     #'(lambda (sf)
         (cond ((eq format 'json)
                (mrc-insert-json sf t))
               ((eq format 'xml)
                (dom-print (mrc-subfield->dom sf)))
               (t (progn
                    (message "Unknown format.")
                    (insert mrkline))))
         (newline))
     subfields)))

;;; inserting JSON
(defun mrc-insert-json (field pretty)
  "Insert FIELD at point as JSON.
If PRETTY is non-nil, pretty print the output."
  (cond
   ((or (string= "000" (alist-get 'tag field))
        (string= "LDR" (alist-get 'tag field)))
    (insert (concat "\"leader\": \"" (alist-get 'value field) "\",")))
   (pretty
    (insert (concat (mrc-pretty-print-json field) ",")))
   (t (insert (concat (json-serialize field) ",")))))

(defun mrc-pretty-print-json (field)
  "Serialize and pretty print a FIELD."
  (with-temp-buffer
    (insert (json-serialize field))
    (json-pretty-print (point-min) (point-max))
    (buffer-string)))

;;; reading strings to field objects
(defun mrc-read-field (mrk)
  "Create JSON object from MRK."
  (let ((mrk (string-trim-left mrk)))
    (if
        (string-match-p (regexp-quote "$$") mrk)
        (mrc-read-datafield mrk)
      (mrc-read-controlfield mrk))))

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
  "Create a JSON object for MARC subfield SF."
  `((code . ,(substring sf 0 1))
    (value . ,(string-trim-left (substring sf 1)))))

(defun mrc-read-controlfield (cf)
  "Create JSON object for MARC controlfield CF."
  (let* ((trimmed-cf (replace-regexp-in-string "^=" "" cf))
         (tag (substring trimmed-cf 0 3))
         (value (replace-regexp-in-string  "[#\\]" " " (string-trim-left (substring trimmed-cf 3)))))
    `((tag . ,tag)
      (value . ,value))))


;;; converting field objects to dom objects
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


(defun mrc-doom-bind-keys ()
  "Bind default keys for Doom Emacs."
  (map! :after nxml-mode
        :map   nxml-mode-map
        :localleader
        (:prefix ("m" . "mrc-text-to-xml")
                 "m" #'mrc-mrk->xml-line
                 "r" #'mrc-mrk->xml-region
                 "s" #'mrc-mrk->xml-subfields-only))
  (map! :map   js-mode-map
        :localleader
        (:prefix ("m" . "mrc-text-to-json")
                 "m" #'mrc-mrk->json-line
                 "r" #'mrc-mrk->json-region
                 "s" #'mrc-mrk->json-subfields-only))
  (map! :after typescript-mode
        :map typescript-mode-map
        :localleader
        (:prefix ("m" . "mrc-text-to-json")
                 "m" #'mrc-mrk->json-line
                 "r" #'mrc-mrk->json-region
                 "s" #'mrc-mrk->json-subfields-only)))

(provide 'mrc)
;;; mrc.el ends here
