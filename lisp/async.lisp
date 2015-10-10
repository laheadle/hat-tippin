
(defun channel () nil)

(setf (@ channel prototype put)
	  (lambda (payload)
		(let* ((chan this)
			   (promise (chain Q (*promise
								  (lambda (resolve reject)
									(async-when (not (empty chan))
												(chain this send-next)
												(resolve true)))))))
		  promise)))

(defun make-queue ()
  )
