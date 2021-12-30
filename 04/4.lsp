;; Using https://github.com/Renug/Common-Lisp-MD5
;; It returns MD5 as a list of one-byte numbers

(defun key (i)
	(concatenate 'string "bgvyzdsv" (write-to-string i))
)

(defun find-key-5 (i)
	(let ((x (md5 (key i))))
	(if (and (= (first x) 0) (= (second x) 0) (< (third x) 16))
		i (find-key (+ i 1))))
)

(defun find-key-6 (i)
	(let ((x (md5 (key i))))
	(if (and (= (first x) 0) (= (second x) 0) (= (third x) 0))
		i (find-key (+ i 1))))
)
