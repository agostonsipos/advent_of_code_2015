(defun inc-char (ch) (code-char (+ 1 (char-code ch))))

(defun start-with-3seq (str)
	(and (cddr str) (char= (inc-char (inc-char (first str))) (inc-char (second str)) (third str)))
)

(defun start-with-pair (str)
	(and (cdr str) (char= (first str) (second str)))
)

(defun valid-pwd (str seq3 pair flag)
	(cond
	((null str) (and (>= seq3 1) (>= pair 2)))
	((member (car str) '(#\i #\o #\u)) NIL)
	(t
		(let ((cnd (start-with-pair str)))
		(valid-pwd (cdr str) 
			(+ seq3 (if (start-with-3seq str) 1 0))
			(+ pair (if (and flag cnd) 1 0))
			(not (and flag cnd))))))
)

(defparameter *inp* (map 'list #'identity "hepxcrrq"))

(defun inc-pwd (str)
	(labels ((inc-pwd-aux (str)
		(if (char= (car str) #\z)
			(cons #\a (inc-pwd-aux (cdr str)))
			(cons (inc-char (car str)) (cdr str)))))
	(nreverse (inc-pwd-aux (nreverse str))))
)

(defun find-pwd (str)
	(if (valid-pwd str 0 0 t) str (find-pwd (inc-pwd str)))
)

(find-pwd *inp*)
(find-pwd (inc-pwd (find-pwd *inp*)))

