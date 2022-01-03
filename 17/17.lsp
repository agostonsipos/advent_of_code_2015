(defparameter *inp* (sort '(33 14 18 20 45 35 16 35 1 13 18 13 50 44 48 6 24 41 30 42) #'>))
(defparameter *inp1* '(20 15 10 5 5))

(defun options (amount buckets)
	(if (< amount 0) 0
	(if (= amount 0) 1
	(if (null buckets) 0 
	(+ (options (- amount (car buckets)) (cdr buckets)) (options amount (cdr buckets)))))))


(defun options-list (amount buckets)
	(if (< amount 0) NIL
	(if (= amount 0) '(())
	(if (null buckets) NIL
	(let ((list1 (options-list (- amount (car buckets)) (cdr buckets))) (list2 (options-list amount (cdr buckets))))
	(if (and (null list1) (null list2)) NIL
	(if (null list1) list2
	(if (null list2) (mapcar (lambda (x) (cons (car buckets) x)) list1)
	(append (mapcar (lambda (x) (cons (car buckets) x)) list1) list2)))))))))

(defun same-number-start (a n)
	(if (= (first a) (second a)) (same-number-start (cdr a) (+ n 1)) n))

(defun shortest-permutations (options)
	(same-number-start (sort (mapcar 'length options) #'<) 1))
