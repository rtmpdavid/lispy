(ql:quickload :iterate)
(use-package :iterate)

(defparameter mutation-chance 100)
(defparameter population-size 10000)

(defun char+ (char &optional (n 0))
  (code-char (+ (char-code char) n)))

(defun char- (char &optional (n 0))
  (code-char (- (char-code char) n)))

(defun char1+ (char)
  (code-char (1+ (char-code char))))

(defun char1- (char)
  (code-char (1- (char-code char))))

(defun random-range (start end)
  (+ start (random (- end start))))

(defun string-random (len)
  (coerce
   (loop for i from 1 to len
      collecting (code-char (random-range (char-code #\Space) (1+ (char-code #\~)))))
   'string))

(defun string-fit (string1 string2)
  (loop for i from 0 to (1- (length string1))
       summing (abs (- (char-code (elt string1 i))
                       (char-code (elt string2 i))))))

(defun mutate-char (string n)
  (let* ((c (elt string n))
         (new-c (cond
                  ((zerop (random 2)) (char+ c (random 10)))
                  (t (char- c (random 10))))))
    (setf (elt string n)
          (cond
            ((char< new-c #\Space) #\Space)
            ((char> new-c #\Tilde) #\Tilde)
            (t new-c)))))

(defun mutate-string (string)
  (let ((new-string (copy-seq string)))
    (loop for i from 0 to (1- (length string))
       if (zerop (random mutation-chance))
       do (setf (elt new-string i) (mutate-char new-string i)))
    new-string))

(defun make-population (ind)
  (loop for i from 1 to population-size
       collecting (mutate-string ind)))

(defun gen1 (string)
  (setf mutation-chance (floor (/ (length string) 2.5)))
  (let* ((current-gen (string-random (length string)))
         (population (make-population current-gen))
         (old-gen current-gen))
    (format t "Gen: 0 | Fitness: ~a | ~a~%" (string-fit string current-gen) current-gen)
    (iter
      (for gen from 1)
      (if (string/= current-gen old-gen)
          (format t "Gen: ~a | Fitness: ~a | ~a~%" gen (string-fit string current-gen) current-gen))
      (until (zerop (string-fit string current-gen)))
      (setf old-gen current-gen)
      (setf current-gen (iter (for i in population)
                              (finding i minimizing (string-fit string i))))
      (setf population (make-population current-gen)))))
