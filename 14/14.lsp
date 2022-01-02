(defparameter *inp* '((Dancer 27 5 132) (Cupid 22 2 41) (Rudolph 11 5 48) (Donner 28 5 134) (Dasher 4 16 55) (Blitzen 14 3 38) (Prancer 3 21 40) (Comet 18 6 103) (Vixen 18 5 84)))

(defun distance (reindeer time)
	(cond
	((< time 0) 0)
	((< time (third reindeer)) (* time (second reindeer)))
	(t (+ (* (second reindeer) (third reindeer)) (distance reindeer (- time (third reindeer) (fourth reindeer))))))
)

(defun distance2 (reindeer time)
	(let ((runtime (third reindeer)) (period (+ (third reindeer) (fourth reindeer))))
	(* (second reindeer) (+ (* runtime (floor time period)) (min (mod time period) runtime))))
)

(defparameter *time* (loop for i from 1 to 2503 collect i))

(defun winning (i)
	(let ((state (nth (- i 1) (mapcar 
	(lambda (i) (mapcar (lambda (r) (list (car r) (distance r i))) *inp*))
	*time*))))
	(mapcar 'first (remove-if-not (lambda (r) (= (apply 'max (mapcar 'second state)) (second r))) state )))
)

(defun winner ()
	(let ((winners (mapcan 'winning *time*)))
	(reduce 'max (mapcar (lambda (r) (reduce (lambda (x q) (if (eq (first r) q) (+ 1 x) x)) winners :initial-value 0)) *inp*)))
)

(reduce 'max (mapcar (lambda (r) (distance2 r 2503)) *inp*))

(winner)
