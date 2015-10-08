(defpost (first "Enjoying Parenscript")
	(:p "I haven't hacked Common Lisp since I got paid to do so in the
  nineties. But ever since those days I've loved the language,
  especially the syntax and all it brings. Recently I've been coding a
  lot of javascript, which is fine, but I've often wondered what it
  would be like if I could change the language. I looked
  into" ((:a :href "") "sweet.js") " but it didn't take because I got
  the feeling it was weaker and more complex than what I was looking
  for. Well it turns out my preferred solution was hiding in plain
  sight all along. I think I heard about parenscript years ago, but it
  didn't click until last week. Since then I've been on a
  binge (meaning a few hours here and there between work and
  family).")
  (:h3 "Up and running")
  (:p
   "Once I was interested, I started looking around. I used Chris Done's nice "
   ((:a :href "http://ircbrowse.net/browse/lisp?q=parenscript") "ircbrowse") "
  tool to troll through the irc logs looking for \"parenscript.\"
  That's how I found Jeaye's recent, working tutorial on"
   ((:a :href "http://blog.jeaye.com/2015/09/27/parenscript-ajax/") "parenscript and ajax")
   ". Also Burton Samograd's " ((:a :href "https://github.com/burtonsamograd/sigil") "Sigil")
   ", which is a command-line parenscript compiler integrated into the
node.js ecosystem -- a handy tool for a javascripter like me, and one which
lets you get started right away. Given the mountains of rust that had
accumulated on my CL skills over the last decade and a half, this was
exactly what I needed to start hacking quickly -- and getting a
close-up view of the rewards that parenscript brings.")

										; Join mailing list, see the react post.

  (:p "After some quick calls to apt-get and npm, I had SBCL and sigil
  set up, and I was ready to try parenscript. Recently I've been
  writing some promise-heavy js, which is a bit heavyweight,
  syntactically speaking. Promises in javascript basically recreate
  the familiar control-flow semantics of sequential code for
  asynchronous scenarios. This is nice, but you end up writing a lot
  of functions, returning things, and passing a lot of arguments
  around in ways I was hoping to abstract.")

  (:p "For example, here's a piece of javascript from my project:")
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
  with CL, and this will not happen overnight. Mention macro guy's CL
  content which rules.")

  (:p ) ; why I chose parenscript over wisp and a few other jsy lisps

  ;;react, macro guy, sigil, paredit, lisp for the web
  (:h3 "OMG Paredit")
  (:h3 "Common Lisp")
  )
