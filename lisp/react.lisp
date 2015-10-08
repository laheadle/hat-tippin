;;;; 
;;; Macros to define common lisp forms in parenscript.

(defmacro defun* (name args &body body)
  "Define a function at the lisp top level."
  `(eval-when (:compile-toplevel) (defun ,name ,args ,@body)))

(defmacro defparameter (&rest args)
  "Define a parameter at the lisp top level."
  `(eval-when (:compile-toplevel) (defparameter ,@args)))

;;;; Defclass

(defparameter *react-method-names* '(set-state))

(defun* gen-method-binding (name)
  `(,name (chain this ,name (bind this))))

(defun* gen-method-macro-binding (name)
  `(,name (&rest args) `(funcall ,',name ,@args)))

(defun* gen-prop-binding (prop)
  `(,prop (@ this props ,prop)))

(defun* gen-method (args body props method-names)
  (let ((self (gensym)))
    `(lambda ,args
       (let ((,self this))
		 (symbol-macrolet
			 ((this ,self) ; fix this pointer
			  ,@(mapcar 'gen-prop-binding props)
			  ,@(mapcar 'gen-method-binding method-names))
		   (macrolet ,(mapcar 'gen-method-macro-binding method-names)
			 ,@body))))))

(defun* gen-defclass-props (props methods)
  (let ((method-names (append (mapcar #'second methods) *react-method-names*)))
    (loop for (ignore name args . body) in methods
       collect name collect (gen-method args body props method-names))))

(defmacro defclass (name props &body methods)
  (let ((props (gen-defclass-props props methods)))
    `(var ,name (chain *react (create-class (create ,@props))))))

;;;; Dom

(defparameter *react-known-tags* '(a abbr address area article aside audio b
								   base bdi bdo big blockquote body br button canvas caption cite code
								   col colgroup data datalist dd del details dfn div dl dt em embed
								   fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6 head header
								   hr html i iframe img input ins kbd keygen label legend li link main
								   map mark menu menuitem meta meter nav noscript object ol optgroup
								   option output p param pre progress q rp rt ruby s samp script
								   section select small source span strong style sub summary sup table
								   tbody td textarea tfoot th thead time title tr track u ul var video
								   wbr))

(defun* listify (x)
  (if (listp x) x (list x)))

(defun* dom-p (x)
  (and (listp x)
       (keywordp (car (listify (car x))))))

(defun* gen-tag (tag)
  (if (member tag *react-known-tags* :test 'string-equal)
      `(@ *react *d-o-m ,tag)
      tag))

(defmacro dom (expr)
  (if (not (dom-p expr))
      expr
      (let* ((head (listify (car expr)))
			 (body (cdr expr))
			 (tag (make-symbol (symbol-name (car head))))
			 (props (cdr head)))
		`(funcall ,(gen-tag tag) (create ,@props)
				  (list ,@(mapcar (lambda (x) `(dom ,x)) body))))))

;;;; EXAMPLE: todo app

(defclass todo-app ()

  (defun get-initial-state ()
    (create todos (list :a :b :c) current ""))

  (defun render-todo-item (item)
    (dom (:li item)))

  (defun handle-change (event)
    (set-state (create current (@ event target value))))

  (defun add-item ()
    (let ((current (chain this state current (trim)))
		  (todos (chain this state todos (slice))))
      (when (> (@ current length) 0)
		(chain todos (push current))
		(set-state (create todos todos current "")))))

  (defun render ()
    (with-slots (todos current) (@ this state)
      (dom (:div (:ul (chain todos (map render-todo-item)))
				 ((:input value current on-change handle-change))
				 ((:button on-click add-item) :add))))))

(let ((todo-app (chain *react (create-factory todo-app))))
  (chain *react (render
				 (dom (funcall todo-app))
				 (@ window document body))))
