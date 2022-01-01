(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
      while line
      collect line))
)

(defun count-length-diff (inp)
	(if (= (length inp) 1) 2
	(if (char= (first inp) #\\) 
		(if (char= (second inp) #\x) (+ 3 (count-length-diff (cddddr inp)))
		(+ 1 (count-length-diff (cddr inp))))
	(count-length-diff (cdr inp))))
)

(defun count-length-diff-str (str) (count-length-diff (cdr (map 'list #'identity str))))

(defparameter *inp1* "\"aaa\\\"aaa\"")
(defparameter *inp2* "\"\\x27\"")

(count-length-diff-str *inp1*)
(count-length-diff-str *inp2*)

(defun adv08 ()
	(let ((inp (get-file "8.txt")))
	(reduce #'+ (mapcar 'count-length-diff-str inp)))
)

(defun count-length-inc (inp)
	(if (null inp) 2
	(if (or (char= (first inp) #\\) (char= (first inp) #\"))
		(+ 1 (count-length-inc (cdr inp)))
		(count-length-inc (cdr inp))))
)

(defun count-length-inc-str (str) (count-length-inc (map 'list #'identity str)))

(count-length-inc-str *inp1*)
(count-length-inc-str *inp2*)

(defun adv08b ()
	(let ((inp (get-file "8.txt")))
	(reduce #'+ (mapcar 'count-length-inc-str inp)))
)

(adv08)
(adv08b)
