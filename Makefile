
sigil=./node_modules/sigil-cli/sigil

threadTest:
	$(sigil) lisp/threads.lisp lisp/election.lisp > build/js/election.js
#	$(sigil) threads.lisp > election.js ; cat election.js

reactTest:
	$(sigil) react.lisp > react.js
	cp *.html *.js $(HOME)/Downloads/reactPlay

site:
	$(sigil) lisp/site.lisp > build/js/site.js
	cp static/index.html build/js/site.js $(HOME)/Downloads/site
	cp static/js/* $(HOME)/Downloads/site/js
