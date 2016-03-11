;;;
;;; File: hackerrank-snippets.lisp
;;; Author: David Selivanov <david.selivanov@aisa.ru>
;;;
;;; Created: Thursday, March  3 2016
;;;

(defun read-n-integers (n)
  (loop for i from 1 to n
     collecting (parse-integer
                 (coerce (loop for c = (read-char t nil nil)
                            until (or (not c) (not (alphanumericp c)))
                            collecting c)
                         'string))))

(defun rotate-list (list n)
  (let ((d (mod (- n) (length list))))
    (nconc (subseq list d)
           (subseq list 0 d))))

(defun read-matrix (m n)
  (loop for i from 1 to m
     collect (read-n-integers n)))

(defun n-layers (m n)
  (/ (min m n) 2))

(defun melt (matrix m n)
  (elt (elt matrix m) n))

(defun matrix-layer (matrix layer &optional (m (length matrix)) (n (length (car matrix))))
  (let ((m-max (- m layer))
        (m-min (1- layer))
        (n-max (- n layer))
        (n-min (1- layer)))
    (append
     (loop ;;left
        for m from m-min to m-max
        collecting (melt matrix m n-min))
     (loop ;;bot
        for n from (1+ n-min) to n-max
        collecting (melt matrix m-max n))
     (loop ;;right
        for m from (1- m-max) downto m-min
        collecting (melt matrix m n-max))
     (loop ;;top
        for n from (1- n-max) downto (1+ n-min)
        collecting (melt matrix m-min n)))))

(defun layers-matrix (layers matrix m n)
  (loop for l in layers
     for li from 1
     for m-max = (- m li)
     for m-min = (1- li)
     for n-max = (- n li)
     for n-min = (1- li)
     for counter = 1
     do (loop ;;left
           for elt in (subseq l 0)
           for m from m-min to m-max
           do (setf (elt (elt matrix m) n-min) elt))
     do (incf counter (- m-max m-min))
     do (loop ;;bot
           for elt in (subseq l counter)
           for n from (1+ n-min) to n-max
           do (setf (elt (elt matrix m-max) n) elt))
     do (incf counter (- n-max n-min))
     do (loop ;;right
           for elt in (subseq l counter)
           for m from (1- m-max) downto m-min
           do (setf (elt (elt matrix m) n-max) elt))
     do (incf counter (- m-max m-min))
     do (loop ;;top
           for elt in (subseq l counter)
           for n from (1- n-max) downto (1+ n-min)
           do (setf (elt (elt matrix m-min) n) elt)))
  matrix)

(defun print-matrix (matrix)
  (loop for row in matrix
     do (loop for col in row
           do (format t "~a " col))
     do (format t "~%")))

(defun rotate-matrix (matrix m n r)
  (layers-matrix
   (loop for l from 1 to (n-layers m n)
      collecting (rotate-list (matrix-layer matrix l) r))
   (copy-seq (mapcar #'copy-seq matrix))
   m n))

(defun succ-chance (all-bubbles popped-bubbles)
  (/ (- all-bubbles popped-bubbles) all-bubbles))

;; (let ((n-bubbles (reduce #'* (read-n-integers 2) :initial-value 1)))
;;   (format t "~,10f~% "(loop for i from 0 to (1- n-bubbles)
;;                          summing (/ 1.0d0 (succ-chance n-bubbles i)))))


(defun fact (n)
  (case n
    (0 1.0d0)
    (1 1.0d0)
    (2 2.0d0)
    (3 6.0d0)
    (4 24.0d0)
    (5 120.0d0)
    (6 720.0d0)
    (7 5040.0d0)
    (8 40320.0d0)
    (9 362880.0d0)))

(defun frac (x)
  (lambda (i) (/ (expt x i) (fact i))))

(defun e^x (x)
  (reduce #'+ (mapcar (frac x) '(0 1 2 3 4 5 6 7 8 9))))

(loop for i from 1 to (parse-integer (read-line))
   do (format t "~f~%" (e^x (read-from-string (read-line)))))


(defun read-number ()
  (let ((n (read))
        (k (read)))
    (super-digit (digit-sum (* (digit-sum n) k)))))

(defun digit-sum (number)
  (loop
     with n = number
     while (> n 0)
     summing (mod n 10)
     do (setf n  (floor (/ n 10)))))


(defun super-digit (n)
  (if (> n 10) (super-digit (digit-sum n))
      n))


(defun mingle (string1 string2)
  (map nil #'(lambda (c1 c2) (format t "~c~c" c1 c2))
       string1
       string2)
  (format t "~%"))


(defun mingle2 (string1 string2)
  (mappend #'list (coerce string1 'list) (coerce string2 'list)))


(defvar pi/4 (/ pi 4.0d0))

(defun p- (p1 p2)
  (cons (- (car p1) (car p2))
        (- (cdr p1) (cdr p2))))

(defun p+ (p1 p2)
  (cons (+ (car p1) (car p2))
        (+ (cdr p1) (cdr p2))))

(defun p/ (p1 n)
  (cons (/ (car p1) n)
        (/ (cdr p1) n)))

(defun pabs (point)
  (sqrt (+ (expt (car point) 2.0d0)
           (expt (cdr point) 2.0d0))))

(defun pnorm (p)
  (p/ p (pabs p)))

(defun pang (p)
  (acos (car p)))


(defun calc-dir (bot princess)
  (if (equalp bot princess) nil
      (let ((phi (/ (pang (pnorm (p- bot princess))) pi/4)))
        (cond
          ((and (>= phi 0)
                (< phi 1)) 'left)
          ((and (>= phi 1)
                (< phi 3)) 'down)
          ((and (>= phi 3)
                (< phi 5)) 'right)
          ((and (>= phi 5)
                (< phi 7)) 'up)
          ((>= phi 7) 'left)))))

(defun read-bot ()
  (read-line)
  (let ((p nil)
        (b nil))
    (loop for c = (read-char)
       with m = 0
       with n = 0
       while (and c (or (not p) (not b)))
       do (case c
            (#\m (setf b (cons n m)))
            (#\p (setf p (cons n m)))
            (#\Newline (setf m (1+ m)
                             n 0))
            (#\- (incf n))))
    (list 'bot b 'princess p)))

(defun print-steps (positions)
  (loop
     with b = (getf positions 'bot)
     with p = (getf positions 'princess)
     for d = (calc-dir b p)
     for i from 0 to 5
     while d
     do (format t "~a~%" d)
     do (case d
          (up (decf (cdr b)))
          (down (incf (cdr b)))
          (right (incf (car b)))
          (left (decf (car b))))))


(defun %string-reduction (string i)
  (if (= i (length string)) string
      (%string-reduction (remove (aref string i) string :start (1+ i)) (1+ i))))

(defun string-reduction (string)
  (%string-reduction string 0))


(defun tree-add-leaf (node datum)
  (if (not node) (list datum)
      (append node (list datum))))
