
(defmacro defun* (name args &body body)
  "Define a function at the lisp top level."
  `(eval-when (:compile-toplevel) (defun ,name ,args ,@body)))

; (:ok (args*) expr)
(defun* normalize-then (x)
  (if (listp x)
	  (let ((with-tag
			 (case (car x)
				   (:err x)
				   (:ok x)
				   (t (cons :ok x)))))
		(case (length (cdr with-tag))
			  (2 x)
			  (1 (cons (car x) (cons '() (cdr x))))
			  (0 (error "too short"))))
	(error "not list")))

(defun* make-funs (one &optional two)
  (flet ((check (k c) 
		   (if (eq (car c) k)
			   `(lambda ,(cadr c) ,(caddr c))
			   ())))
	(if (or (and (check :ok one) (check :ok two))
			(and (check :err one) (check :err two)))
		(error "two errs or two oks")
		(let ((ok (or (check :ok one) (check :ok two)))
			  (err (or (check :err one) (check :err two))))
		  (if err
			  `(then ,ok ,err)
			  `(then ,ok))))))

(defun* get-thens (seqs)
  (flet ((get-one (s)
				(case (length s)
					  (1
					   (let ((rn (normalize-then (car s))))
						 (make-funs rn)))
					  (2 (let ((rn0 (normalize-then (car s)))
							   (rn1 (normalize-then (cadr s))))
						   (make-funs rn0 rn1))))))
		(mapcar #'get-one seqs)))
			 
(defmacro seq (promise &rest seqs)
  `(chain ,promise ,@(get-thens seqs)))

;(seq a ((:ok 5) (:err 4)))
; a.then(function() { return 5; })
; nested seqs


