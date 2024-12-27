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



(defun mrk2js-insert-json (field pretty)
  "Insert FIELD at point as JSON.
If PRETTY is non-nil, pretty print the output."
  (if pretty
      (insert (mrk2js-pretty-print-json field))
    (insert (json-serialize field))))
(setq js-df '((tag . "245")
  (ind1 . "0")
  (ind2 . "0")
  (subfields . [((code . "a") (value . "Main title"))
                ((code . "b") (value . "remainder of title"))
                ((code . "c") (value . "statement of responsibility"))])))

(setq js-sf '((code . "a") (value . "Main title")))

(mapcar (lambda (x)
          `(subfield
            ((code . , (alist-get 'code x)))
            ,(alist-get 'value x))
          )
        (alist-get 'subfields js-df))
(mapcar #'mrc-subfield->dom (alist-get 'subfields js-df))
(mrc-controlfield->dom (mrc-read-controlfield controlfield))

(defun tst (form)
  (cond ((eq form :json)
         (message "json"))
        ((eq form :xml)
         (message "xml"))
        (t (message "fail"))))

(tst :xml)
