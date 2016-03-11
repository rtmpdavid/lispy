;;;
;;; File: table.lisp
;;; Author: David Selivanov <david.selivanov@aisa.ru>
;;;
;;; Created: Tuesday, December 15 2015
;;;

(defun make-table-fun (calc-fun n-min n-max step &optional (lerp nil))
  (let* ((range (floor (/ (- n-max n-min) step)))
         (table (make-array (1+ range))))
    (flet ((step-to-index (n)
             (floor (/ (- n n-min) step))))
      (loop for i from n-min to n-max by step
         do (setf (aref table (step-to-index i)) (funcall calc-fun i)))
      (if lerp (lambda (n) (multiple-value-bind (i lin) (step-to-index n)
                             (+ (aref table i)
                                (* lin (aref table (1+ i))))))
          (lambda (n) (aref table (step-to-index n)))))))
