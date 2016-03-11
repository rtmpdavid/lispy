(deftype elide-position () '(member :beginning :middle :end))

;; (declaim (inline elide-beginning)
;;          (inline elide-middle)
;;          (inline elide-end))

;; (defun elide-beginning (string elide-string max-len)
;;   (concatenate 'string
;;                elide-string
;;                (subseq string (+ (- (length string) max-len)
;;                                  (length elide-string)))))

;; (defun elide-middle (string elide-string max-len)
;;   (concatenate 'string
;;                (subseq string 0 (ceiling (/ (- max-len (length elide-string)) 2)))
;;                elide-string
;;                (subseq string (- (length string) (floor (/ (- max-len (length elide-string)) 2))))))

;; (defun elide-end (string elide-string max-len)
;;   (concatenate 'string
;;                (subseq string 0 (- max-len (length elide-string)))
;;                elide-string))

(defun string-elide (string &key (elide-string "..") (max-len 6) (position :middle))
  (declare (type string string)
           (type elide-position position)
           (type (integer 0 *)))
  (flet ((elide-beginning (string elide-string max-len)
           (concatenate 'string
                        elide-string
                        (subseq string (+ (- (length string) max-len)
                                          (length elide-string)))))
         (elide-middle (string elide-string max-len)
           (concatenate 'string
                        (subseq string 0 (ceiling (/ (- max-len (length elide-string)) 2)))
                        elide-string
                        (subseq string (- (length string) (floor (/ (- max-len (length elide-string)) 2))))))
         (elide-end (string elide-string max-len)
           (concatenate 'string
                        (subseq string 0 (- max-len (length elide-string)))
                        elide-string)))
    (cond
      ((< (length string) max-len) string)
      ((< max-len (length elide-string)) (subseq elide-string 0 max-len))
      ((eq position :beginning) (elide-beginning string elide-string max-len))
      ((eq position :middle) (elide-middle string elide-string max-len))
      ((eq position :end) (elide-end string elide-string max-len)))))
