(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defparameter *data* (jsown:parse (file-get-contents "C:/Users/agost/Dropbox/12.txt")))

(defun calc-sum (data)
	(cond
	((numberp data) data)
	((eq (type-of data) 'cons)
			(+ (calc-sum (car data)) (calc-sum (cdr data))))
	(t 0)
	)
)

(calc-sum *data*)

(defun my-plus (a b)
	(list (+ (first a) (first b)) (and (second a) (second b)))
)


(defun calc-sum-2 (data)
	(cond
	((numberp data) data)
	((eq (type-of data) 'cons) 
		(if (eq (car data) ':OBJ)
			(if (rassoc "red" (cdr data) :test 'equal) 0 (calc-sum-2 (cdr data)))
			(+ (calc-sum-2 (car data)) (calc-sum-2 (cdr data)))))
	(t 0)
	)
)

(calc-sum-2 *data*)
