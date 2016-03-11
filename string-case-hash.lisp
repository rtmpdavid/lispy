;;;
;;; File: string-case-hash.lisp
;;; Author: David Selivanov <david.selivanov@aisa.ru>
;;;
;;; Created: Tuesday, February 16 2016
;;;
(defmacro string-case (keystring &body cases)
  "like case, but with strings"
  `(cond ,@(loop for case in cases
                 collecting
                 (if (cdr case) (list `(string= ,keystring ,(car case))
                                      (cadr case))
                   (list 't (car case))))))

(defmacro string-case-hash (keystring &body clauses)
  "like case, but with strings.
Clauses can be followed by a t clause,
to have a default behaviour."
  (let ((ks (gensym)))
    `(let ((,ks (sxhash ,keystring)))
       (case ,ks
         ,@(loop
            for clause in clauses
            collecting
            (if (stringp (car clause))
                (list (sxhash (car clause)) (cadr clause))
              (if (eq t (car clause))
                  clause
                (error "Non string and non-t clause in a string-case"))))))))
