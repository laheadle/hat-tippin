
sigil=./node_modules/sigil-cli/sigil

threadTest:
	$(sigil) lisp/threads.lisp lisp/election.lisp > build/js/election.js
#	$(sigil) threads.lisp > election.js ; cat election.js

reactTest:
	$(sigil) react.lisp > react.js
	cp *.html *.js $(HOME)/Downloads/reactPlay

blog:
	$(sigil) lisp/blog.lisp > build/js/blog.js
	cp static/blog.html build/js/blog.js $(HOME)/Downloads/blog
	cp static/js/* $(HOME)/Downloads/blog/js
