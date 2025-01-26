(defparameter *inp* '((Sugar 3 0 0 -3 2) (Sprinkles -3 3 0 0 9) (Candy -1 0 4 0 1) (Chocolate 0 0 -2 2 8)))

(defun elementwise-add (a b) (if (null a) () (cons (+ (car a) (car b)) (elementwise-add (cdr a) (cdr b)))))
(defun list-mul (a b) (if (null a) () (cons (* (car a) b) (list-mul (cdr a) b))))
(defun cap-zero (a) (if (null a) () (cons (max (car a) 0) (cap-zero (cdr a)))))

(defun score (percents)
	(reduce '* (cap-zero (reduce (lambda (x ing) (elementwise-add x (list-mul (butlast (cdr (first ing))) (second ing)))) 
	(mapcar 'list *inp* percents) :initial-value '(0 0 0 0))))
)

(defparameter *recipes*
	(loop for i from 0 to 100 appending
		(loop for j from 0 to (- 100 i) appending
			(loop for k from 0 to (- 100 i j) collect
				(list i j k (- 100 i j k))))))


(defun 500calories (percents)
	(= (reduce '+ (mapcar '* percents (mapcar 'sixth *inp*))) 500)
)

(reduce 'max (mapcar 'score *recipes*))
(reduce 'max (mapcar 'score (remove-if-not '500calories *recipes*)))
