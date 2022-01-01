(defun look-and-say (seq res)
	(cond 
	((and (not (null (cddr seq))) (= (first seq) (second seq) (third seq)))
		(look-and-say (cdddr seq) `(,(first seq) ,3 . ,res)))
	((and (not (null (cdr seq))) (= (first seq) (second seq)))
		(look-and-say (cddr seq) `(,(first seq) ,2 . ,res)))
	((not (null seq))
		(look-and-say (cdr seq) `(,(first seq) ,1 . ,res)))
	((null seq)
		(nreverse res)))
)

(defparameter *inp* '(1 1 1 3 2 2 2 1 1 3))
(look-and-say *inp* ())

(defun adv10 (inp n)
	(let ((x inp))
	(dotimes (i n)
	(setq x (look-and-say x ())))
	x)
)

(print (length (adv10 *inp* 40)))
(print (length (adv10 *inp* 50)))
