(defun fun1 ()
    (let ((data (make-array 1 :adjustable t :fill-pointer 0)))
	      
	      (with-open-file (words "/home/david/src/lispy/enable1.txt" :direction :input)
		(loop for line = (read-line words nil)
		   while line
		   do (vector-push-extend (subseq line 0 (1- (length line))) data)))
	      (loop 
		 for word1 across data
		 with i = 0
		 with bigword = (make-array 2)
		 if (= 0 (mod i 100))
		 do (print (* 100 (coerce (/ i (length data)) 'float)))
		 do (loop 
		       for word2 across data
		       with temp = (make-array 2 :initial-contents (list i 0))
			 
		       if (search word2 word1)
		       do (progn (incf (aref temp 1)))
		       finally (if (> (elt temp 1) (elt bigword 1))
				   (progn (setf (aref bigword 0) (aref temp 0)
						(aref bigword 1) (aref temp 1)))))
		 do (incf i)
		 finally (format t "~a ~%" (aref data (aref bigword 0))))))



(defun fnv-1 (str)
  (let ((hash 14695981039346656037))
    (loop 
       for octet across (string-to-octets str)
       do (progn (setf hash (ldb (byte 64 0)
				 (* hash 1099511628211)))
		 (setf hash (logxor hash octet))))
       hash))


(defun fnv-1a (str)
  (let ((hash 14695981039346656037))
    (loop 
       for octet across (string-to-octets str)
       do (progn (setf hash (logxor hash octet))
		 (setf hash (ldb (byte 64 0) 
				 (* hash 1099511628211)))))
    hash))

(defun substrs (str)
  (loop for i from 0 to (length str)
     appending (loop for j from (+ 2 i) to (length str)
		  collect (subseq str i j))))


(defun fun2 ()
    (let ((data (make-hash-table))
	  (bigwords nil)
	  (bigword '("" 0 nil)))
      (with-open-file (words "enable1.txt" 
			     :direction :input)
	(loop for line = (read-line words nil)
	   while line
	   do (setf (gethash (fnv-1a 
			      (subseq line
				      0
				      (1- (length line)))) data)
		    (subseq line
			    0
			    (1- (length line))))))	

	
      (if (> (length subwords) (second bigword))
	  (setf bigword (list word 
			      (length subwords) 
			      (sort subwords #'string<)))))
	(sort bigwords (lambda (a b) (< (second a) (second b))))))

(defun load-words (&optional (hash-fun #'sxhash))
  (let ((words-hash-table (make-hash-table :hash-function hash-fun)))
    (with-open-file (words "/home/david/src/lispy/enable1.txt" :direction :input)
      (loop for line = (read-line words nil)
	 while line
	 do (setf (gethash (funcall hash-fun line) words-hash-table) line)
	 finally (return words-hash-table)))))
	   

(defun fun3 ()






