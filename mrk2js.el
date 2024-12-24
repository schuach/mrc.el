;; (defun parse-subfields (subfields)
;;   (let ((sflist ()))))
;; (setq datafield `((tag . "245")
;;                   (ind1 . "0")
;;                   (ind2 . "0")
;;                   (subfields . ,xsubfields)))

;; (setq xsubfields '[((code . "a") (value . "sfa"))
;;                   ((code . "b") (value . "sfb"))])

;; (json-serialize xsubfields)

(setq mrkline "   =24500 $$aHaupttitel$$bTitelzusatz")
(setq controlfield "001 991234567883331")
(setq leader "000 haeihaei")

(defun mrk2js-read-datafield (mrk)
  "Create JSON object for a MARC datafield."
  (let* ((parts (split-string mrk "\\$\\$" t " *"))
         (tag-ind (replace-regexp-in-string "^=" "" (string-replace " " "" (car parts))))
         (tag (substring tag-ind 0 3))
         (ind1 (replace-regexp-in-string "[#\\]" " " (substring tag-ind 3 4)))
         (ind2 (replace-regexp-in-string "[#\\]" " " (substring tag-ind 4 5)))
         (subfields (cdr parts)))
    `(datafield ((tag . ,tag)
                 (ind1 . ,ind1)
                 (ind2 . ,ind2)
                 (subfields . ,(mrk2js-handle-subfields subfields)))
      )))

(defun mrk2js-handle-subfields (subfield-strings)
  "Create marc:subfields given a marc-breaker string without tag and
indicators."
  (vconcat (mapcar #'mrk2js-read-subfield subfield-strings)))

(defun mrk2js-read-subfield (sf)
  "Create a JSON object for a MARC subfield."
  `((code . ,(substring sf 0 1))
    (value . ,(string-trim-left (substring sf 1)))))
;; (cons `((code . ,(substring sf 0 1)) (value . ,(string-trim-left (substring sf 1)))))

(defun mrk2js-read-controlfield (mrk)
  "Create JSON object for a MARC controlfield."
  `((tag . ,(substring mrk 0 3))
    (value . ,(string-trim-left (substring mrk 3)))))

(defun mrk2js-pretty-print-json (field)
  "Serialize and pretty print a FIELD."
  (with-temp-buffer
           (insert (json-serialize field))
           (json-pretty-print (point-min) (point-max))
           (buffer-string)))

(defun mrk2js-insert-json (field pretty)
  "Insert FIELD at point as JSON.
If PRETTY is non-nil, pretty print the output."
  (if pretty
      (insert (mrk2js-pretty-print-json field))
    (insert (json-serialize field))))

(mrk2js-insert-json (mrk2js-read-datafield mrkline) t)
(mrk2js-insert-json (mrk2js-read-controlfield controlfield) nil)

((lambda (x) (1+ x)) 2)
(mrk2js-handle-subfield "a bla")
