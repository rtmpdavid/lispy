(require 'cl-spark)

(proclaim '(optimize speed))
(proclaim '(type fixnum n max-val))


(defun rand-hyst (n max-val)
  (let ((a (loop
	      for i
	      from 0 to n
	      appending (list (random max-val))))
	(b (loop
	      for i
	      from 0 to max-val
	      appending (list (list i 0)))))
    
    (mapcar #'(lambda (x) (incf (cadr (nth x b)))) a)
    (spark:vspark b 
	      :labels (loop for i from 0 to (1- max-val) appending (list i))
	      :size 100
	      :key #'cadr)))

