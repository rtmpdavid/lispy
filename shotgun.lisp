(defparameter frags1 (list "tgca" "taggcta" "gtcatgcttaggcta" "agcatgctgcag" "tcatgct"))
(defun find-subseqs (list)
  (let ((subseqs nil))
    (map-permutations #'(lambda (a) (if (search (car a) (cadr a))
                                        (push (car a) subseqs)))
                      list :length 2)
    subseqs))

(defun nremove-subseqs (list)
  (mapcar #'(lambda (item)
              (setf list (delete item list)))
          (remove-duplicates (find-subseqs list)))
  list)

(defun string-matching (str1 str2)
  (iter
    (for m from 0 to (length str1))
    (for v = (search (subseq str1 m) str2))
    (until (and v (= v 0)))
    (finally
     (return (if (= v 0) (- (length str1) m) 0)))))


(defun find-matches (list)
  (let ((n (1- (length list)))
        (variants nil))
    (iter (for i from 0 to n)
          (for s1 = (nth i list))
          (iter (for j from (1+ i) to n)
                (for s2 = (nth j list))
                (for dir = (string-matching s1 s2))
                (for rev = (string-matching s2 s1))
                (if (> dir 0) (push (list i j dir) variants))
                (if (> rev 0) (push (list j i rev) variants))))
    variants))

(defun remove-nth (n list)
  (append (subseq list 0 n) (nthcdr (1+ n) list)))

(defun apply-match (list match)
  (let ((s1 (nth (car match) list))
        (s2 (nth (cadr match) list)))
    (setf list (remove-nth (max (car match) (cadr match)) list))
    (setf list (remove-nth (min (car match) (cadr match)) list))
    (push (concatenate 'string (subseq s1 0 (- (length s1) (caddr match))) s2) list)))

(defun shotgun (list n)
  (format t "~aV~%" n)
  (if (= (length list) 1) list
      (let* ((seqs (nremove-subseqs (copy-seq list)))
             (matches (find-matches seqs)))
        (if matches (progn   (format t "~a<-~%" n)
                             (iter (for m in matches)
                                   (collecting (shotgun (apply-match seqs m) (1+ n)))))
            (let ((rv nil))
              (map-permutations #'(lambda (s)
                                    (format t "~a--------~%" n)
                                    (push (apply #'concatenate 'string s) rv))
                                seqs)
              rv)))))

(defun first-n (seq n)
  (if (< (length seq) n) seq
      (subseq seq 0 n)))

(defun get-best-matches (matches)
  (let ((best 0))
    (iter (for m in matches)
          (if (> (caddr m) best) (setf best (caddr m))))
    (first-n (remove-if-not #'(lambda (m) (= (caddr m) best)) matches) 1)))

(defvar funcalls)

(defun shotgun-greedy (list n)

  (format t "~a<-~%" n)
  (if (= (length list) 1) list
      (let* ((seqs (nremove-subseqs (copy-seq list)))
             (matches (get-best-matches (find-matches seqs))))
        (if matches (prog1 (iter (for m in matches)
                                 (format t "~a<--~%" n)
                                 (appending (shotgun-greedy (apply-match seqs m) (1+ n))))
                           (format t "~a-->~%" n))
            (let ((rv nil))
              (format t "~a->~%" n)
              (map-permutations #'(lambda (s)
                                    (push (apply #'concatenate 'string s) rv))
                                seqs)
              rv)))))
