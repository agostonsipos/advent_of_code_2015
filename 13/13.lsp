(defparameter *inp* '((Alice 54 Bob) (Alice -81 Carol) (Alice -42 David) (Alice 89 Eric) (Alice -89 Frank) (Alice 97 George) (Alice -94 Mallory) (Bob 3 Alice) (Bob -70 Carol) (Bob -31 David) (Bob 72 Eric) (Bob -25 Frank) (Bob -95 George) (Bob 11 Mallory) (Carol -83 Alice) (Carol 8 Bob) (Carol 35 David) (Carol 10 Eric) (Carol 61 Frank) (Carol 10 George) (Carol 29 Mallory) (David 67 Alice) (David 25 Bob) (David 48 Carol) (David -65 Eric) (David 8 Frank) (David 84 George) (David 9 Mallory) (Eric -51 Alice) (Eric -39 Bob) (Eric 84 Carol) (Eric -98 David) (Eric -20 Frank) (Eric -6 George) (Eric 60 Mallory) (Frank 51 Alice) (Frank 79 Bob) (Frank 88 Carol) (Frank 33 David) (Frank 43 Eric) (Frank 77 George) (Frank -3 Mallory) (George -14 Alice) (George -12 Bob) (George -52 Carol) (George 14 David) (George -62 Eric) (George -18 Frank) (George -17 Mallory) (Mallory -36 Alice) (Mallory 76 Bob) (Mallory -34 Carol) (Mallory 37 David) (Mallory 40 Eric) (Mallory 18 Frank) (Mallory 7 George)))

(defun dist-dir (a b)
	(let ((x (find-if (lambda (x) (and (eq (first x) a) (eq (third x) b))) *inp*))) (if x (second x) 0))
)

(defun dist (a b)
	(+ (dist-dir a b) (dist-dir b a))
)

(defun collect-points ()
	(remove-duplicates (append (mapcar 'first *inp*) (mapcar 'third *inp*)))
)

(defun all-permutations (points)
	(if (null points) '(())
	(apply 'append (loop for x in points
		collect (mapcar (lambda (p) (cons x p)) (all-permutations (remove x points))))))
)
(defun points-with-me ()
	(cons 'Agoston (collect-points))
)

(defun cost-route-aux (metric points)
	(if (= (length points) 1) 0
	(+ (funcall metric (first points) (second points)) (cost-route-aux metric (rest points))))
)

(defun cost-route (metric points)
	(+ (funcall metric (first points) (first (last points))) (cost-route-aux metric points))
)

(reduce 'max (mapcar (lambda (points) (cost-route 'dist points)) (all-permutations (collect-points))))
(reduce 'max (mapcar (lambda (points) (cost-route 'dist-with-me points)) (all-permutations (points-with-me))))
