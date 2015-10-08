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

(defvar *posts* ([]))

(defun push-post (post)
  (setf (aref *posts* (@ *posts* length)) post))

(defclass blog-index ()

  (defun table-of-contents ()
	(dom (:div)))

  (defun get-initial-state ()
	(create posts *posts*))

  ;; Dan Midwood http://danmidwood.com/content/2014/11/21/animated-paredit.html
  ;; http://pub.gajendra.net/src/paredit-refcard.pdf
  ;; Chris Done's ircbrowse http://ircbrowse.net/browse/lisp?q=parenscript
  ;; http://www.lispworks.com/documentation/HyperSpec/Front/index.htm

 (defun render ()
   (dom 
	(:div 
	 (:div (table-of-contents))
	 (:div (@ this state posts))))))

(defun make-instance (clazz props)
  (funcall (chain *react (create-factory clazz props))))

(defmacro defpost (class title &rest body)
  `(progn
	 (defclass ,class ()
	   (defun render ()
		 (dom (:div (:h3 ,title) ,@body))))
	 (let ((post (make-instance ,class)))
	   (push-post post))))

(defpost first "Enjoying Parenscript"
	(:p "I haven't hacked Common Lisp since I got paid to do so in the
  nineties. But ever since those days I've loved the language,
  especially the syntax and all it brings. Recently I've been coding a
  lot of javascript, which is fine, but I've often wondered what it
  would be like if I could change the language. I looked
  into " ((:a :href "") "sweet.js") " but it didn't take because I got
  the feeling it was weaker and more complex than what I was looking
  for. Well it turns out my preferred solution was hiding in plain
  sight all along. I think I heard about parenscript years ago, but it
  didn't click until last week. Since then I've been on a
  binge (meaning a few hours here and there between work and
  family).")
  (:h3 "Looking Around")
  (:p
   "Once I was interested, I started looking around. I used Chris Done's nice "
   ((:a :href "http://ircbrowse.net/browse/lisp?q=parenscript") "ircbrowse") "
  tool to troll through the irc logs looking for \"parenscript.\"
  That's how I found Jeaye's recent, working tutorial on "
   ((:a :href "http://blog.jeaye.com/2015/09/27/parenscript-ajax/") "parenscript and ajax")
   ". Also Burton Samograd's " ((:a :href "https://github.com/burtonsamograd/sigil") "Sigil")
   ", which is a command-line parenscript compiler integrated into the
node.js ecosystem -- a handy tool for a javascripter like me, and one which
lets you get started right away. Given the mountains of rust that had
accumulated on my CL skills over the last decade and a half, this was
exactly what I needed to start hacking quickly -- it gave me a
close-up view of the rewards that parenscript brings.")

  (:h3 "Pulling the Trigger")
  (:p ((:a :href "https://groups.google.com/forum/#!forum/parenscript") "The parenscript mailing list ")
"is bit hidden, requiring moderator approval to view
archives. Curious, I overcame my laziness and requested access. At
first, it looked like any other low-traffic list. But then I saw
something that blew me away. It was a posting by Olaf Ruppert titled
\"React Macros.\" This caught my attention because I had been meaning
to work with Facebook's React platform for a while, but had barely
scratched the surface. Olaf's mail was short, but very sweet. He wrote:")

  (:pre "Hello list,

I'm currently writing an react application. 
You might find these macros helpful.

best regards, olaf")

  (:p "Helpful? Olaf is clearly a master of understatement. Try
beautiful. Olaf's unassuming proffer was followed by about a hundred
lines of code that wrapped React in a fresh lispy package (actually,
about half the code is a little example app). It contains syntactic
sweeteners for React classes, methods, properties, and most important,
DOM tree generation. Olaf had thereby done away with React's grossest
feature, the JSX minilanguage, in about 10 lines of code. If your
background is anything like mine, go find and study this code (I will
probably share it more widely soon but you can't miss it on the
list). It will not only educate but insire you. I have been enjoying
it ever since, but more to the point, as soon as I saw it I was
possessed with a strong desire to run it. This was the force that
pushed me from browsing to actively hacking parenscript.")

  (:h3 "Up and Running")
  (:p "I now had a mission: to run Olaf's React example. After some
  quick calls to apt-get and npm, I had SBCL and Sigil set up, and I
  was ready to try parenscript. sigil ran just fine on Olaf's code,
  and output some js. I took a look, and it was pretty readable!
  Olaf's macros had a couple of gensyms in them so it wasn't perfectly
  clean, but parenscript did a good job of producing a straightforward
  mapping to js, at least in my first few impressions. However, the
  example didn't run. Turns out Olaf's code was written in 2013, and
  React had made some changes since then. Now I needed to debug the
  code, which I did not write, using a language/syntax I did not yet
  know, and a framework I had barely looked at.")

  (:p "It was easy! After about 20 minutes I had updated Olaf's code
  to work with the most recent version of React. (The issue was that
  react no longer lets you directly invoke the return value from "
	  ((:a :href "https://gist.github.com/sebmarkbage/d7bce729f38730399d28")
	   "createClass")
	  "). So my confidence was increased, and I experienced the thrill
	  of seeing a barebones todo list appear on screen.")

  (:h3 "Scratching my own Itch")

  (:p "Next I thought I'd try to build something of my own with
  parenscript. Recently I've been writing some js laden with promises,
  which is a bit heavyweight, syntactically speaking. Promises in
  javascript basically recreate the familiar control-flow semantics of
  sequential code for asynchronous scenarios. This is nice, but you
  end up writing a lot of functions, returning things, and passing a
  lot of arguments around in ways I was hoping to abstract. For
  example, here's a piece of javascript from my project:")
  (:pre "

function eachElector(cursor) {
	return db.nextObject(cursor).then(
		function(elector) {
			if (!elector) {
				return;
			}
			return eachVote(db.findVotes(election, elector.domain))
				.then(function() {
					return eachElector(cursor);
				});
		});
}
  ")
  (:p "I thought about what I'd rather see here, and could easily implement, and came up with:")
  (:pre "
 (defun each-elector (cursor)
   (seq (chain db next-object)
	 ((:ok (elector)
	    (if elector
		  (seq (each-vote (chain db (find-votes election (@ elector domain))))
		    ((:ok (each-elector cursor)))))))))
"
		)
  (:p "I'll do better next time, but I think this is an
  improvement. There are fewer manually created functions, for
  one. The point is, I had a design and started writing my macro. It
  didn't work, so I started manually inserting format statements all
  over the place and looking up things in the hyperspec until I
  figured out the problem. This taught me a valuable lesson:
  parenscript coding is Common Lisp coding. This is a good thing
  because CL is a powerful language with decades of experience in
  exactly the area we're using it for -- macros. But it means that
  getting comfortable with parenscript involves getting comfortable
  with CL, and this will not happen overnight. Around this time I discovered Malisper's excellent blog "
((:a :href "http://malisper.me/") "Macrology") ", which is
filled with thorough and well-written explanations of Common Lisp.")

  (:p ) ; why I chose parenscript over wisp and a few other jsy lisps

  ;; paredit, lisp for the web
  (:h3 "OMG Paredit")
  (:h3 "Common Lisp")
  (:h3 "Future plans") ; https://github.com/johnmastro/trident-mode.el/blob/master/trident-mode.el
  )

(dom ((:component router)
	  ((:component route path "/" component ))	  ))

(elts (*router
	   ((*route path "/"))))

(defrouter *router
	((*route path "/" component *index)
	 ((*route path "/articles/2" component *index))))

(with-slots ((*router *route) *react-router)
	(chain *react (render
				   (make-instance
					*router
					(create children (list (make-instance
											*route
											(create path "/"
													component (aref *posts* 0))))))
				   (@ window document body))))
