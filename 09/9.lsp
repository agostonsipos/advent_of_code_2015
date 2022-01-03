(defparameter *inp1* '((London Dublin 464) (London Belfast 518) (Dublin Belfast 141)))
(defparameter *inp* '((Faerun Norrath 129) (Faerun Tristram 58) (Faerun AlphaCentauri 13) (Faerun Arbre 24) (Faerun Snowdin 60) (Faerun Tambi 71) (Faerun Straylight 67) (Norrath Tristram 142) (Norrath AlphaCentauri 15) (Norrath Arbre 135) (Norrath Snowdin 75) (Norrath Tambi 82) (Norrath Straylight 54) (Tristram AlphaCentauri 118) (Tristram Arbre 122) (Tristram Snowdin 103) (Tristram Tambi 49) (Tristram Straylight 97) (AlphaCentauri Arbre 116) (AlphaCentauri Snowdin 12) (AlphaCentauri Tambi 18) (AlphaCentauri Straylight 91) (Arbre Snowdin 129) (Arbre Tambi 53) (Arbre Straylight 40) (Snowdin Tambi 15) (Snowdin Straylight 99) (Tambi Straylight 70)))


(defun dist (a b)
	(third (find-if (lambda (x) (or (and (eq (first x) a) (eq (second x) b)) (and (eq (first x) b) (eq (second x) a)))) *inp*))
)

(defun collect-points ()
	(remove-duplicates (append (mapcar 'first *inp*) (mapcar 'second *inp*)))
)

(collect-points)

(defun all-permutations (points)
	(if (null points) '(())
	(apply 'append (loop for x in points
		collect (mapcar (lambda (p) (cons x p)) (all-permutations (remove x points))))))
)

(defun cost-route (points)
	(if (= (length points) 1) 0
	(+ (dist (first points) (second points)) (cost-route (rest points))))
)

(apply 'min (mapcar 'cost-route (all-permutations (collect-points))))
(apply 'max (mapcar 'cost-route (all-permutations (collect-points))))
