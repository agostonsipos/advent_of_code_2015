(defun get-atom-length (str n)
	(if (and (< (+ n 1) (length str)) (char= (char str (+ n 1)) (char-downcase (char str (+ n 1)))))
		(get-atom-length str (+ n 1))
		n))

(defun parse (str &optional (n 0))
	(if (<= (length str) n) ()
	(let ((m (get-atom-length str n)))
	(cons (subseq str n (+ m 1)) (parse str (+ m 1))))))

(defparameter *inpstring* "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl")
;(defparameter *inpstring* "NAl")
(defparameter *inp* (parse *inpstring*))


(defparameter *rules* '(("Al" . "ThF") ("Al" . "ThRnFAr") ("B" . "BCa") ("B" . "TiB") ("B" . "TiRnFAr") ("Ca" . "CaCa") ("Ca" . "PB") ("Ca" . "PRnFAr") ("Ca" . "SiRnFYFAr") ("Ca" . "SiRnMgAr") ("Ca" . "SiTh") ("F" . "CaF") ("F" . "PMg") ("F" . "SiAl") ("H" . "CRnAlAr") ("H" . "CRnFYFYFAr") ("H" . "CRnFYMgAr") ("H" . "CRnMgYFAr") ("H" . "HCa") ("H" . "NRnFYFAr") ("H" . "NRnMgAr") ("H" . "NTh") ("H" . "OB") ("H" . "ORnFAr") ("Mg" . "BF") ("Mg" . "TiMg") ("N" . "CRnFAr") ("N" . "HSi") ("O" . "CRnFYFAr") ("O" . "CRnMgAr") ("O" . "HP") ("O" . "NRnFAr") ("O" . "OTi") ("P" . "CaP") ("P" . "PTi") ("P" . "SiRnFAr") ("Si" . "CaSi") ("Th" . "ThCa") ("Ti" . "BP") ("Ti" . "TiTi") ("e" . "HF") ("e" . "NAl") ("e" . "OMg")))
(setq *rules* (sort *rules* (lambda (a b) (if (> (length (parse (cdr a))) (length (parse (cdr b)))) t NIL))))

(defun apply-rule (inp rule)
	(if (null inp) ()
	(let ((rest (mapcar (lambda (x) (cons (car inp) x)) (apply-rule (cdr inp) rule))))
	(if (string= (car rule) (car inp))
		(cons (append (parse (cdr rule)) (cdr inp)) rest)
		rest))))

(defun apply-rules (inp rules)
	(mapcan (lambda (r) (apply-rule inp r)) rules))

(defun concat (list)
	(apply #'concatenate 'string list))

(defun adv19 ()
	(length (remove-duplicates (mapcar 'concat (apply-rules *inp* *rules*)) :test 'string-equal)))

(defun apply-rules-count (inpn rules)
	(let ((inp (car inpn)) (n (cdr inpn)))
	(mapcan (lambda (r) (mapcar (lambda (x) (cons x (+ n 1))) (apply-rule inp r))) rules)))

(defun apply-all (strings)
	(mapcan (lambda (inpn) (apply-rules-count inpn *rules*)) strings))

(apply-all '((("e") . 0)))

(defun result-reached (strings)
	(assoc-if (lambda (s) (string= (concat s) (concat *inp*))) strings))

(defun play-bfs (strings)
	(let ((x (result-reached strings)))
	(print (length strings))
	(if x x (play-bfs (apply-all strings)))))

(defparameter *cache* (make-hash-table))

(defun play-dfs (strings)
	(print (cdar strings))
	(if (null strings) NIL
	(if (> (length (caar strings)) (length *inp*)) (play-dfs (cdr strings))
	(if (string= (concat (caar strings)) *inpstring*) (car strings)
	(let ((x 
	(or (gethash (car strings) *cache*)
	(setf (gethash (car strings) *cache*)
		(play-dfs (apply-rules-count (car strings) *rules*))))))
	(if x x (play-dfs (cdr strings))))))))

(defun adv19b ()
	(play-dfs '((("e") . 0))))
