/* (DEFMACRO DEFUN* (NAME ARGS &BODY BODY)
     Define a function at the lisp top level.
     `(EVAL-WHEN (COMPILE-TOPLEVEL) (DEFUN ,NAME ,ARGS ,@BODY))) */

/* (DEFMACRO DEFPARAMETER (&REST ARGS)
     Define a parameter at the lisp top level.
     `(EVAL-WHEN (COMPILE-TOPLEVEL) (DEFPARAMETER ,@ARGS))) */

/* (DEFPARAMETER *REACT-METHOD-NAMES* '(SET-STATE)) */

/* (DEFUN* GEN-METHOD-BINDING (NAME) `(,NAME (CHAIN THIS ,NAME (BIND THIS)))) */

/* (DEFUN* GEN-METHOD-MACRO-BINDING (NAME)
    `(,NAME (&REST ARGS) `(FUNCALL ,',NAME ,@ARGS))) */

/* (DEFUN* GEN-PROP-BINDING (PROP) `(,PROP (@ THIS PROPS ,PROP))) */

/* (DEFUN* GEN-METHOD (ARGS BODY PROPS METHOD-NAMES)
    (LET ((SELF (GENSYM)))
      `(LAMBDA ,ARGS
         (LET ((,SELF THIS))
           (SYMBOL-MACROLET ((THIS ,SELF)
                             (BACKQ-COMMA-AT (MAPCAR 'GEN-PROP-BINDING PROPS))
                             (BACKQ-COMMA-AT
                              (MAPCAR 'GEN-METHOD-BINDING METHOD-NAMES)))
             (MACROLET (BACKQ-COMMA
                        (MAPCAR (QUOTE GEN-METHOD-MACRO-BINDING)
                          METHOD-NAMES))
               ,@BODY)))))) */

/* (DEFUN* GEN-DEFCLASS-PROPS (PROPS METHODS)
    (LET ((METHOD-NAMES
           (APPEND (MAPCAR #'SECOND METHODS) *REACT-METHOD-NAMES*)))
      (LOOP FOR (IGNORE NAME ARGS . BODY) IN METHODS
            COLLECT NAME
            COLLECT (GEN-METHOD ARGS BODY PROPS METHOD-NAMES)))) */

/* (DEFUN* MAKE-MAKE-CLASS-NAME (NAME)
    (FLET ((CHOP (S)
             (STRING-LEFT-TRIM * S)))
      (MAKE-SYMBOL (CONCATENATE 'STRING MAKE- (CHOP (STRING NAME)))))) */

/* (DEFMACRO DEFCLASS (NAME PROPS &BODY METHODS)
     (LET ((PROPS (GEN-DEFCLASS-PROPS PROPS METHODS)))
       `(PROGN
         (VAR ,NAME (CHAIN *REACT (CREATE-CLASS (CREATE ,@PROPS))))
         (DEFUN ,(MAKE-MAKE-CLASS-NAME NAME) ,NIL
           (LET ((FAC (CHAIN *REACT (CREATE-FACTORY ,NAME))))
             (CHAIN FAC (APPLY FAC ARGUMENTS))))))) */

/* (DEFPARAMETER *REACT-KNOWN-TAGS*
     '(A ABBR ADDRESS AREA ARTICLE ASIDE AUDIO B BASE BDI BDO BIG BLOCKQUOTE
       BODY BR BUTTON CANVAS CAPTION CITE CODE COL COLGROUP DATA DATALIST DD
       DEL DETAILS DFN DIV DL DT EM EMBED FIELDSET FIGCAPTION FIGURE FOOTER
       FORM H1 H2 H3 H4 H5 H6 HEAD HEADER HR HTML I IFRAME IMG INPUT INS KBD
       KEYGEN LABEL LEGEND LI LINK MAIN MAP MARK MENU MENUITEM META METER NAV
       NOSCRIPT OBJECT OL OPTGROUP OPTION OUTPUT P PARAM PRE PROGRESS Q RP RT
       RUBY S SAMP SCRIPT SECTION SELECT SMALL SOURCE SPAN STRONG STYLE SUB
       SUMMARY SUP TABLE TBODY TD TEXTAREA TFOOT TH THEAD TIME TITLE TR TRACK U
       UL VAR VIDEO WBR)) */

/* (DEFUN* LISTIFY (X)
    (IF (LISTP X)
        X
        (LIST X))) */

/* (DEFUN* DOM-P (X) (AND (LISTP X) (KEYWORDP (CAR (LISTIFY (CAR X)))))) */

/* (DEFUN* GEN-TAG (TAG)
    (IF (MEMBER TAG *REACT-KNOWN-TAGS* TEST 'STRING-EQUAL)
        `(@ *REACT *D-O-M ,TAG)
        TAG)) */

/* (DEFMACRO DOM (EXPR)
     (IF (NOT (DOM-P EXPR))
         EXPR
         (LET* ((HEAD (LISTIFY (CAR EXPR)))
                (BODY (CDR EXPR))
                (TAG (MAKE-SYMBOL (SYMBOL-NAME (CAR HEAD))))
                (PROPS (CDR HEAD)))
           `(FUNCALL ,(GEN-TAG TAG) (CREATE ,@PROPS)
                     (LIST ,@(MAPCAR (LAMBDA (X) `(DOM ,X)) BODY)))))) */

/* (DEFMACRO PUSH (ELT ARRAY) `(CHAIN ,ARRAY (PUSH ,ELT))) */

/* (DEFUN ARRAY-COPY (A) (CHAIN A (SPLICE))) */
function arrayCopy(a) {
    return a.splice();
};
/* (DEFMACRO MAKE-INSTANCE (CLAZZ PROPS &REST CHILDREN)
     `(FUNCALL (CHAIN *REACT (CREATE-FACTORY ,CLAZZ)) ,PROPS ,@CHILDREN)) */

/* (DEFMACRO F (ARGS &REST BODY) `(LAMBDA ,ARGS ,@BODY)) */

/* (DEFVAR NEXT-KEY
     (FUNCALL
      (LAMBDA ()
        (LET ((K 0))
          (LAMBDA () (INCF K) K))))) */
var nextKey = (function () {
    var k = 0;
    return function () {
        ++k;
        return k;
    };
})();
/* (DEFUN ADD-KEY (PROPS)
     (LET ((PROPS (OR PROPS (CREATE))))
       (SETF (@ PROPS KEY) (NEXT-KEY))
       PROPS)) */
function addKey(props) {
    var props1 = props || {  };
    props1.key = nextKey();
    return props1;
};
/* (DEFMACRO MAKE-KEYED-INSTANCE (CLAZZ PROPS &REST CHILDREN)
     `(MAKE-INSTANCE ,CLAZZ (ADD-KEY ,PROPS) ,@CHILDREN)) */

/* (DEFVAR *POSTS* ([])) */
var POSTS = [];
/* (DEFUN PUSH-POST (POST) (PUSH POST *POSTS*)) */
function pushPost(post) {
    return POSTS.push(post);
};
/* (DEFUN GET-POST (ID) (AREF *POSTS* ID)) */
function getPost(id) {
    return POSTS[id];
};
/* (DEFCLASS *BLOG (CHILDREN)
             (DEFUN RENDER ()
               (DOM
                (DIV
                 (MAKE-KEYED-INSTANCE (@ *REACT-ROUTER *LINK) (CREATE TO /)
                  (DOM ((H1 KEY (NEXT-KEY)) Hat Tippin')))
                 (H4 by Lyn Headley) CHILDREN)))) */
var Blog = React.createClass({ render : function () {
    var g1266 = this;
    return React.DOM.div({  }, [React.createFactory(ReactRouter.Link)(addKey({ to : '/' }), React.DOM.h1({ 'key' : nextKey() }, ['Hat Tippin\''])), React.DOM.h4({  }, ['by Lyn Headley']), g1266.props.children]);
} });
function makeBlog() {
    var fac = React.createFactory(Blog);
    return fac.apply(fac, arguments);
};
/* (DEFCLASS *BLOG-INDEX NIL (DEFUN TABLE-OF-CONTENTS () (DOM (DIV)))
             (DEFUN GET-INITIAL-STATE () (CREATE POSTS *POSTS*))
             (DEFUN RENDER ()
               (DOM
                (DIV (DIV (TABLE-OF-CONTENTS))
                 (DIV
                  (CHAIN THIS STATE POSTS
                   (MAP
                    (LAMBDA (POST I)
                      (MAKE-KEYED-INSTANCE (@ *REACT-ROUTER *LINK)
                       (CREATE TO (+ /posts/ I)) (@ POST PROPS TITLE)))))))))) */
var BlogIndex = React.createClass({ tableOfContents : function () {
    var g1269 = this;
    return React.DOM.div({  }, []);
},
                                    getInitialState : function () {
    var g1270 = this;
    return { posts : POSTS };
},
                                    render : function () {
    var g1271 = this;
    return React.DOM.div({  }, [React.DOM.div({  }, [g1271.tableOfContents.bind(g1271)()]), React.DOM.div({  }, [g1271.state.posts.map(function (post, i) {
        return React.createFactory(ReactRouter.Link)(addKey({ to : '/posts/' + i }), post.props.title);
    })])]);
}
                                  });
function makeBlogIndex() {
    var fac = React.createFactory(BlogIndex);
    return fac.apply(fac, arguments);
};
/* (DEFCLASS *POST (PARAMS) (DEFUN RENDER () (GET-POST (CHAIN PARAMS ID)))) */
var Post = React.createClass({ render : function () {
    var g1284 = this;
    return getPost(g1284.props.params.id);
} });
function makePost() {
    var fac = React.createFactory(Post);
    return fac.apply(fac, arguments);
};
/* (DEFMACRO DEFPOST (CLASS TITLE &REST BODY)
     `(PROGN
       (DEFCLASS ,CLASS (TITLE)
                 (DEFUN RENDER ,NIL (DOM (DIV (H1 ,TITLE) ,@BODY))))
       (LET ((POST (,(MAKE-MAKE-CLASS-NAME CLASS) (CREATE TITLE ,TITLE))))
         (PUSH-POST POST)))) */

/* (DEFPOST FIRST Enjoying Parenscript (Mostly) (P (I October 2015))
    (P I haven't hacked Common Lisp since I got paid to do so in the
  nineties. But ever since those days I've loved the language,
  especially the syntax and all it brings. Recently I've been coding a
  lot of javascript, which is fine, but I've often wondered what it
  would be like if I could change the language. I looked
  into
     ((A HREF ) sweet.js)  but it didn't take because I got
  the feeling it was weaker and more complex than what I was looking
  for. I also looked
  at
     ((A HREF https://github.com/Gozala/wisp) Wisp) , which
  looks like a nice js lisp, shares a lot of syntax with clojure, and
  isn't clojure. Well it turns out the apple of my eye was hiding in
  plain sight all along. I think I heard about parenscript years ago,
  but it didn't click until last week. Since then I've been on a
  binge (meaning a few hours here and there between work and
  family).)
    (H3 Looking Around)
    (P
     Once I was interested, I started looking around. I used Chris Done's nice
     ((A HREF http://ircbrowse.net/browse/lisp?q=parenscript) ircbrowse) 
  tool to troll through the irc logs looking for "parenscript."
  That's how I found Jeaye's recent, working tutorial on
     ((A HREF http://blog.jeaye.com/2015/09/27/parenscript-ajax/)
      parenscript and ajax)
     . Also Burton Samograd's
     ((A HREF https://github.com/burtonsamograd/sigil) Sigil)
     , which is a command-line parenscript compiler integrated into the
node.js ecosystem -- a handy tool for a javascripter like me, and one which
lets you get started right away. Given the mountains of rust that had
accumulated on my CL skills over the last decade and a half, this was
exactly what I needed to start hacking quickly -- it gave me a
close-up view of the rewards that parenscript brings.)
    (H3 Pulling the Trigger)
    (P
     ((A HREF https://groups.google.com/forum/#!forum/parenscript)
      The parenscript mailing list )
     is bit hidden, requiring moderator approval to view
archives. Curious, I overcame my laziness and requested access. At
first, it looked like any other low-traffic list. But then I saw
something that blew me away. It was a posting by Olaf Ruppert titled
"React Macros." This caught my attention because I had been meaning
to work with Facebook's React platform for a while, but had barely
scratched the surface. Olaf's mail was short, but very sweet. He wrote:)
    (PRE Hello list,

I'm currently writing an react application. 
You might find these macros helpful.

best regards, olaf)
    (P Helpful? Olaf is clearly a master of understatement. Try
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
pushed me from browsing to actively hacking parenscript.)
    (H3 Up and Running)
    (P I now had a mission: to run Olaf's React example. After some
  quick calls to apt-get and npm, I had SBCL and Sigil set up, and I
  was ready to try parenscript. sigil ran just fine on Olaf's code,
  and output some js. I took a look, and it was pretty readable!
  Olaf's macros had a couple of gensyms in them so it wasn't perfectly
  clean, but parenscript did a good job of producing a straightforward
  mapping to js, at least in my first few impressions. However, the
  example didn't run. Turns out Olaf's code was written in 2013, and
  React had made some changes since then. Now I needed to debug the
  code, which I did not write, using a language/syntax I did not yet
  know, and a framework I had barely looked at.)
    (P It was easy! After about 20 minutes I had updated Olaf's code
  to work with the most recent version of React. (The issue was that
  react no longer lets you directly invoke the return value from
     ((A HREF https://gist.github.com/sebmarkbage/d7bce729f38730399d28)
      createClass)
     ). So my confidence was increased, and I experienced the thrill
	  of seeing a barebones todo list appear on screen.)
    (H3 Scratching my own Itch)
    (P Next I thought I'd try to build something of my own with
  parenscript. Recently I've been writing some js laden with promises,
  which is a bit heavyweight, syntactically speaking. Promises in
  javascript basically recreate the familiar control-flow semantics of
  sequential code for asynchronous scenarios. This is nice, but you
  end up writing a lot of functions, returning things, and passing a
  lot of arguments around in ways I was hoping to abstract. For
  example, here's a piece of javascript from my project:)
    (PRE 

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
  )
    (P
     I thought about what I'd rather see here, and could easily implement, and came up with:)
    (PRE 
 (defun each-elector (cursor)
   (seq (chain db (next-object cursor))
	 ((:ok (elector)
	    (if elector
		  (seq (each-vote (chain db (find-votes election (@ elector domain))))
		    ((:ok (each-elector cursor)))))))))
)
    (P I'll do better next time, but I think this is an
  improvement. There are fewer manually created functions, for
  one. The point is, I had a design and started writing my macro. It
  didn't work, so I started manually inserting format statements all
  over the place and looking up things in the hyperspec until I
  figured out the problem. This taught me a valuable lesson:
  parenscript coding is Common Lisp coding. This is a good thing
  because CL is a powerful language with decades of experience in
  exactly the area we're using it for -- macros. But it means that
  getting comfortable with parenscript involves getting comfortable
  with CL, and this will not happen overnight. Around this time I discovered Malisper's excellent blog
     ((A HREF http://malisper.me/) Macrology) , which is
filled with thorough and well-written explanations of Common Lisp.)
    (H3 OMG Paredit)
    (P I heard about this one years ago, but am only now getting into
  it. Paredit, which allows you to edit syntax trees rather than
  characters and lines, has been a revelation. This is a truly great
  answer for the "why Lisp?" question: because your editor is
  operating on full syntax trees, your mind sees code and structure in
  a direct way, not through the muddy lens of characters and
  lines. Funny thing is that I didn't find the learning curve to be
  all that steep, now that the ground has been cleared by Dan
  Midwood's
     ((A HREF http://danmidwood.com/content/2014/11/21/animated-paredit.html)
      Animated Guide to Paredit)
      and Dan Cross's
     ((A HREF http://pub.gajendra.net/src/paredit-refcard.pdf)
      Paredit Reference Card)
      (and don't miss
     ((A HREF http://emacsrocks.com/e14.html) Emacs Rocks! episode 14)
      for inspiration and concrete use cases ).
     In addition to nimbleness, Paredit has given me a new
	  sense of security, and helped me to fearlessly hack away at large
	  s-expressions, with the knowledge that I will only destroy
	  things in targeted ways.)
    (H3 Writing the Engine for this Blog Post)
    (P I don't have a blog, but I wanted to share. I figured "How
  hard could it be to throw together a crappy little blog engine using
  react?" This turned out to be a real pain! I wanted a basic setup
  with a list of posts and a home page link, a template that would
  render around every page, and a few routes. It seems like everybody
  is
  using
     ((A HREF https://github.com/rackt/react-router) React
  Router)
     , so I did too. Ouch. Painful, and in several ways. React
  itself is no picnic. It has these conventions you have to follow
  like generating a special unique key for every "dynamically
  created" child component, which in my case was every child
  component (working on a macro to automate this). React says "hey,
  no problem, don't use JSX if you don't want to," and then every
  single example uses JSX, so you have to figure out the mapping
  yourself. And then react-router presupposes es6 and something like
  browserify in all of its docs, so more translations, not to mention
  the code I downloaded was out of sync with the documentation. And
  when something doesn't work, you end up having to look inside these
  frameworks themselves, which are actually quite opaque. Anyway,
  enough complaining, but suffice it to say I spent a lot of time
  debugging this (like three hours). Here it is, it mostly works.)
    (H3 Future plans)
    (P First I need to lock down my environment. Sigil is great for
starting, but I think I need something more emacs-based, so I can just
look at expansions of lisp into javascript at the touch of a
key. Heck, I can barely use SLIME! So that is next, along with better
quicklisp and asdf understanding. But I am eager to try
     ((A HREF
       https://github.com/johnmastro/trident-mode.el/blob/master/trident-mode.el)
      Trident
  Mode)
     , which they say makes emacs fall in love with parenscript. I also
want to try
Abo-Abo's
     ((A HREF https://github.com/abo-abo/lispy) Lispy)
      which is like a souped-up vi for paredit in emacs or something. As for projects, I'm wondering how well we could do a parenscript implementation of
     ((A HREF https://github.com/clojure/core.async) Clojure's core.async) .)) */
var first = React.createClass({ render : function () {
    var g1294 = this;
    return React.DOM.div({  }, [React.DOM.h1({  }, ['Enjoying Parenscript (Mostly)']), React.DOM.p({  }, [React.DOM.i({  }, ['October 2015'])]), React.DOM.p({  }, ['I haven\'t hacked Common Lisp since I got paid to do so in the\n  nineties. But ever since those days I\'ve loved the language,\n  especially the syntax and all it brings. Recently I\'ve been coding a\n  lot of javascript, which is fine, but I\'ve often wondered what it\n  would be like if I could change the language. I looked\n  into ', React.DOM.a({ 'href' : '' }, ['sweet.js']), ' but it didn\'t take because I got\n  the feeling it was weaker and more complex than what I was looking\n  for. I also looked\n  at ', React.DOM.a({ 'href' : 'https://github.com/Gozala/wisp' }, ['Wisp']), ', which\n  looks like a nice js lisp, shares a lot of syntax with clojure, and\n  isn\'t clojure. Well it turns out the apple of my eye was hiding in\n  plain sight all along. I think I heard about parenscript years ago,\n  but it didn\'t click until last week. Since then I\'ve been on a\n  binge (meaning a few hours here and there between work and\n  family).']), React.DOM.h3({  }, ['Looking Around']), React.DOM.p({  }, ['Once I was interested, I started looking around. I used Chris Done\'s nice ', React.DOM.a({ 'href' : 'http://ircbrowse.net/browse/lisp?q=parenscript' }, ['ircbrowse']), '\n  tool to troll through the irc logs looking for "parenscript."\n  That\'s how I found Jeaye\'s recent, working tutorial on ', React.DOM.a({ 'href' : 'http://blog.jeaye.com/2015/09/27/parenscript-ajax/' }, ['parenscript and ajax']), '. Also Burton Samograd\'s ', React.DOM.a({ 'href' : 'https://github.com/burtonsamograd/sigil' }, ['Sigil']), ', which is a command-line parenscript compiler integrated into the\nnode.js ecosystem -- a handy tool for a javascripter like me, and one which\nlets you get started right away. Given the mountains of rust that had\naccumulated on my CL skills over the last decade and a half, this was\nexactly what I needed to start hacking quickly -- it gave me a\nclose-up view of the rewards that parenscript brings.']), React.DOM.h3({  }, ['Pulling the Trigger']), React.DOM.p({  }, [React.DOM.a({ 'href' : 'https://groups.google.com/forum/#!forum/parenscript' }, ['The parenscript mailing list ']), 'is bit hidden, requiring moderator approval to view\narchives. Curious, I overcame my laziness and requested access. At\nfirst, it looked like any other low-traffic list. But then I saw\nsomething that blew me away. It was a posting by Olaf Ruppert titled\n"React Macros." This caught my attention because I had been meaning\nto work with Facebook\'s React platform for a while, but had barely\nscratched the surface. Olaf\'s mail was short, but very sweet. He wrote:']), React.DOM.pre({  }, ['Hello list,\n\nI\'m currently writing an react application. \nYou might find these macros helpful.\n\nbest regards, olaf']), React.DOM.p({  }, ['Helpful? Olaf is clearly a master of understatement. Try\nbeautiful. Olaf\'s unassuming proffer was followed by about a hundred\nlines of code that wrapped React in a fresh lispy package (actually,\nabout half the code is a little example app). It contains syntactic\nsweeteners for React classes, methods, properties, and most important,\nDOM tree generation. Olaf had thereby done away with React\'s grossest\nfeature, the JSX minilanguage, in about 10 lines of code. If your\nbackground is anything like mine, go find and study this code (I will\nprobably share it more widely soon but you can\'t miss it on the\nlist). It will not only educate but insire you. I have been enjoying\nit ever since, but more to the point, as soon as I saw it I was\npossessed with a strong desire to run it. This was the force that\npushed me from browsing to actively hacking parenscript.']), React.DOM.h3({  }, ['Up and Running']), React.DOM.p({  }, ['I now had a mission: to run Olaf\'s React example. After some\n  quick calls to apt-get and npm, I had SBCL and Sigil set up, and I\n  was ready to try parenscript. sigil ran just fine on Olaf\'s code,\n  and output some js. I took a look, and it was pretty readable!\n  Olaf\'s macros had a couple of gensyms in them so it wasn\'t perfectly\n  clean, but parenscript did a good job of producing a straightforward\n  mapping to js, at least in my first few impressions. However, the\n  example didn\'t run. Turns out Olaf\'s code was written in 2013, and\n  React had made some changes since then. Now I needed to debug the\n  code, which I did not write, using a language/syntax I did not yet\n  know, and a framework I had barely looked at.']), React.DOM.p({  }, ['It was easy! After about 20 minutes I had updated Olaf\'s code\n  to work with the most recent version of React. (The issue was that\n  react no longer lets you directly invoke the return value from ', React.DOM.a({ 'href' : 'https://gist.github.com/sebmarkbage/d7bce729f38730399d28' }, ['createClass']), '). So my confidence was increased, and I experienced the thrill\n\t  of seeing a barebones todo list appear on screen.']), React.DOM.h3({  }, ['Scratching my own Itch']), React.DOM.p({  }, ['Next I thought I\'d try to build something of my own with\n  parenscript. Recently I\'ve been writing some js laden with promises,\n  which is a bit heavyweight, syntactically speaking. Promises in\n  javascript basically recreate the familiar control-flow semantics of\n  sequential code for asynchronous scenarios. This is nice, but you\n  end up writing a lot of functions, returning things, and passing a\n  lot of arguments around in ways I was hoping to abstract. For\n  example, here\'s a piece of javascript from my project:']), React.DOM.pre({  }, ['\n\nfunction eachElector(cursor) {\n\treturn db.nextObject(cursor).then(\n\t\tfunction(elector) {\n\t\t\tif (!elector) {\n\t\t\t\treturn;\n\t\t\t}\n\t\t\treturn eachVote(db.findVotes(election, elector.domain))\n\t\t\t\t.then(function() {\n\t\t\t\t\treturn eachElector(cursor);\n\t\t\t\t});\n\t\t});\n}\n  ']), React.DOM.p({  }, ['I thought about what I\'d rather see here, and could easily implement, and came up with:']), React.DOM.pre({  }, ['\n (defun each-elector (cursor)\n   (seq (chain db (next-object cursor))\n\t ((:ok (elector)\n\t    (if elector\n\t\t  (seq (each-vote (chain db (find-votes election (@ elector domain))))\n\t\t    ((:ok (each-elector cursor)))))))))\n']), React.DOM.p({  }, ['I\'ll do better next time, but I think this is an\n  improvement. There are fewer manually created functions, for\n  one. The point is, I had a design and started writing my macro. It\n  didn\'t work, so I started manually inserting format statements all\n  over the place and looking up things in the hyperspec until I\n  figured out the problem. This taught me a valuable lesson:\n  parenscript coding is Common Lisp coding. This is a good thing\n  because CL is a powerful language with decades of experience in\n  exactly the area we\'re using it for -- macros. But it means that\n  getting comfortable with parenscript involves getting comfortable\n  with CL, and this will not happen overnight. Around this time I discovered Malisper\'s excellent blog ', React.DOM.a({ 'href' : 'http://malisper.me/' }, ['Macrology']), ', which is\nfilled with thorough and well-written explanations of Common Lisp.']), React.DOM.h3({  }, ['OMG Paredit']), React.DOM.p({  }, ['I heard about this one years ago, but am only now getting into\n  it. Paredit, which allows you to edit syntax trees rather than\n  characters and lines, has been a revelation. This is a truly great\n  answer for the "why Lisp?" question: because your editor is\n  operating on full syntax trees, your mind sees code and structure in\n  a direct way, not through the muddy lens of characters and\n  lines. Funny thing is that I didn\'t find the learning curve to be\n  all that steep, now that the ground has been cleared by Dan\n  Midwood\'s ', React.DOM.a({ 'href' : 'http://danmidwood.com/content/2014/11/21/animated-paredit.html' }, ['Animated Guide to Paredit']), ' and Dan Cross\'s ', React.DOM.a({ 'href' : 'http://pub.gajendra.net/src/paredit-refcard.pdf' }, ['Paredit Reference Card']), ' (and don\'t miss ', React.DOM.a({ 'href' : 'http://emacsrocks.com/e14.html' }, ['Emacs Rocks! episode 14']), ' for inspiration and concrete use cases', ').', 'In addition to nimbleness, Paredit has given me a new\n\t  sense of security, and helped me to fearlessly hack away at large\n\t  s-expressions, with the knowledge that I will only destroy\n\t  things in targeted ways.']), React.DOM.h3({  }, ['Writing the Engine for this Blog Post']), React.DOM.p({  }, ['I don\'t have a blog, but I wanted to share. I figured "How\n  hard could it be to throw together a crappy little blog engine using\n  react?" This turned out to be a real pain! I wanted a basic setup\n  with a list of posts and a home page link, a template that would\n  render around every page, and a few routes. It seems like everybody\n  is\n  using ', React.DOM.a({ 'href' : 'https://github.com/rackt/react-router' }, ['React\n  Router']), ', so I did too. Ouch. Painful, and in several ways. React\n  itself is no picnic. It has these conventions you have to follow\n  like generating a special unique key for every "dynamically\n  created" child component, which in my case was every child\n  component (working on a macro to automate this). React says "hey,\n  no problem, don\'t use JSX if you don\'t want to," and then every\n  single example uses JSX, so you have to figure out the mapping\n  yourself. And then react-router presupposes es6 and something like\n  browserify in all of its docs, so more translations, not to mention\n  the code I downloaded was out of sync with the documentation. And\n  when something doesn\'t work, you end up having to look inside these\n  frameworks themselves, which are actually quite opaque. Anyway,\n  enough complaining, but suffice it to say I spent a lot of time\n  debugging this (like three hours). Here it is, it mostly works.']), React.DOM.h3({  }, ['Future plans']), React.DOM.p({  }, ['First I need to lock down my environment. Sigil is great for\nstarting, but I think I need something more emacs-based, so I can just\nlook at expansions of lisp into javascript at the touch of a\nkey. Heck, I can barely use SLIME! So that is next, along with better\nquicklisp and asdf understanding. But I am eager to try ', React.DOM.a({ 'href' : 'https://github.com/johnmastro/trident-mode.el/blob/master/trident-mode.el' }, ['Trident\n  Mode']), ', which they say makes emacs fall in love with parenscript. I also\nwant to try\nAbo-Abo\'s ', React.DOM.a({ 'href' : 'https://github.com/abo-abo/lispy' }, ['Lispy']), ' which is like a souped-up vi for paredit in emacs or something. As for projects, I\'m wondering how well we could do a parenscript implementation of ', React.DOM.a({ 'href' : 'https://github.com/clojure/core.async' }, ['Clojure\'s core.async']), '.'])]);
} });
function makeFirst() {
    var fac = React.createFactory(first);
    return fac.apply(fac, arguments);
};
(function () {
    var post = makeFirst({ title : 'Enjoying Parenscript (Mostly)' });
    return pushPost(post);
})();
/* (WITH-SLOTS (*ROUTER *ROUTE *INDEX-ROUTE)
       *REACT-ROUTER
     (CHAIN *REACT-D-O-M
      (RENDER
       (MAKE-INSTANCE *ROUTER NIL
                      (MAKE-KEYED-INSTANCE *ROUTE
                       (CREATE PATH / COMPONENT *BLOG)
                       (MAKE-KEYED-INSTANCE *INDEX-ROUTE
                        (CREATE COMPONENT *BLOG-INDEX))
                       (MAKE-KEYED-INSTANCE *ROUTE
                        (CREATE COMPONENT *POST PATH posts/:id))))
       (CHAIN DOCUMENT (GET-ELEMENT-BY-ID container))))) */
ReactDOM.render(React.createFactory(ReactRouter.Router)(null, React.createFactory(ReactRouter.Route)(addKey({ path : '/', component : Blog }), React.createFactory(ReactRouter.IndexRoute)(addKey({ component : BlogIndex })), React.createFactory(ReactRouter.Route)(addKey({ component : Post, 'path' : 'posts/:id' })))), document.getElementById('container'));
