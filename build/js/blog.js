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

/* (DEFMACRO DEFCLASS (NAME PROPS &BODY METHODS)
     (LET ((PROPS (GEN-DEFCLASS-PROPS PROPS METHODS)))
       `(VAR ,NAME (CHAIN *REACT (CREATE-CLASS (CREATE ,@PROPS)))))) */

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

/* (DEFUN PUSH (ELT ARRAY) (SETF (AREF ARRAY (@ ARRAY LENGTH)) ELT)) */
function push(elt, array) {
    return array[array.length] = elt;
};
/* (DEFUN ARRAY-COPY (A) (CHAIN A (SPLICE))) */
function arrayCopy(a) {
    return a.splice();
};
/* (DEFUN MAKE-INSTANCE (CLAZZ PROPS &REST CHILDREN)
     (LET ((ARGS (ARRAY-COPY CHILDREN)))
       (CHAIN ARGS (UNSHIFT PROPS))
       (APPLY (CHAIN *REACT (CREATE-FACTORY CLAZZ)) ARGS))) */
function makeInstance(clazz, props) {
    var children = [];
    for (var i1 = 0; i1 < arguments.length - 2; i1 += 1) {
        children[i1] = arguments[i1 + 2];
    };
    var args = arrayCopy(children);
    args.unshift(props);
    return React.createFactory(clazz).apply(this, args);
};
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
/* (DEFUN FLATTEN (&REST ARGS)
     (LET* ((ARR ([]))
            (ARRAYP
             (F (ELT) (AND (OBJECTP ELT) (@ ELT SPLICE) (@ ELT CONCAT))))
            (PUSHABLE (F (ELT) (AND (NOT (UNDEFINED ELT)) (NOT (ARRAYP ELT))))))
       (DOLIST (ELT ARGS)
         (COND ((PUSHABLE ELT) (PUSH ELT ARR))
               ((ARRAYP ELT) (SETF ARR (CHAIN ARR (CONCAT (FLATTEN ELT)))))))
       ARR)) */
function flatten() {
    var args = [];
    for (var i2 = 0; i2 < arguments.length - 0; i2 += 1) {
        args[i2] = arguments[i2 + 0];
    };
    var arr = [];
    var arrayp = function (elt) {
        return typeof elt === 'object' && elt.splice && elt.concat;
    };
    var pushable = function (elt) {
        return 'undefined' !== typeof elt && !arrayp(elt);
    };
    for (var elt = null, _js_idx3 = 0; _js_idx3 < args.length; _js_idx3 += 1) {
        elt = args[_js_idx3];
        if (pushable(elt)) {
            push(elt, arr);
        } else if (arrayp(elt)) {
            arr = arr.concat(flatten(elt));
        };
    };
    return arr;
};
/* (DEFUN MAKE-KEYED-INSTANCE (CLAZZ PROPS CHILDREN)
     (LET ((PROPS (OR PROPS (CREATE))))
       (SETF (@ PROPS KEY) (NEXT-KEY))
       (APPLY MAKE-INSTANCE (FLATTEN CLAZZ PROPS CHILDREN)))) */
function makeKeyedInstance(clazz, props, children) {
    var props4 = props || {  };
    props4.key = nextKey();
    return makeInstance.apply(this, flatten(clazz, props4, children));
};
/* (DEFVAR *POSTS* ([])) */
var POSTS = [];
/* (DEFUN PUSH-POST (POST) (PUSH POST *POSTS*)) */
function pushPost(post) {
    return push(post, POSTS);
};
/* (DEFUN GET-POST (ID) (AREF *POSTS* ID)) */
function getPost(id) {
    return POSTS[id];
};
/* (DEFCLASS *BLOG (CHILDREN) (DEFUN RENDER () (DOM CHILDREN))) */
var Blog = React.createClass({ render : function () {
    var g1245 = this;
    return g1245.props.children;
} });
/* (DEFCLASS *BLOG-INDEX NIL (DEFUN TABLE-OF-CONTENTS () (DOM (DIV)))
             (DEFUN GET-INITIAL-STATE () (CREATE POSTS *POSTS*))
             (DEFUN RENDER ()
               (DOM
                (DIV (DIV (TABLE-OF-CONTENTS))
                 (DIV
                  (CHAIN THIS STATE POSTS
                   (MAP
                    (LAMBDA (POST I)
                      (MAKE-INSTANCE (@ *REACT-ROUTER *LINK)
                                     (CREATE TO POST PARAMS
                                      (CREATE ID I))))))))))) */
var BlogIndex = React.createClass({ tableOfContents : function () {
    var g1248 = this;
    return React.DOM.div({  }, []);
},
                                    getInitialState : function () {
    var g1249 = this;
    return { posts : POSTS };
},
                                    render : function () {
    var g1250 = this;
    return React.DOM.div({  }, [React.DOM.div({  }, [g1250.tableOfContents.bind(g1250)()]), React.DOM.div({  }, [g1250.state.posts.map(function (post, i) {
        return makeInstance(ReactRouter.Link, { to : post, params : { id : i } });
    })])]);
}
                                  });
/* (DEFCLASS *POST (ID) (DEFUN RENDER () (GET-POST ID))) */
var Post = React.createClass({ render : function () {
    var g1263 = this;
    return getPost(g1263.props.id);
} });
/* (DEFMACRO DEFPOST (CLASS TITLE &REST BODY)
     `(PROGN
       (DEFCLASS ,CLASS ,NIL
                 (DEFUN RENDER ,NIL (DOM (DIV (H3 ,TITLE) ,@BODY))))
       (LET ((POST (MAKE-INSTANCE ,CLASS)))
         (PUSH-POST POST)))) */

/* (DEFPOST FIRST Enjoying Parenscript
    (P I haven't hacked Common Lisp since I got paid to do so in the
  nineties. But ever since those days I've loved the language,
  especially the syntax and all it brings. Recently I've been coding a
  lot of javascript, which is fine, but I've often wondered what it
  would be like if I could change the language. I looked
  into
     ((A HREF ) sweet.js)  but it didn't take because I got
  the feeling it was weaker and more complex than what I was looking
  for. Well it turns out my preferred solution was hiding in plain
  sight all along. I think I heard about parenscript years ago, but it
  didn't click until last week. Since then I've been on a
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
   (seq (chain db next-object)
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
    (P) (H3 OMG Paredit) (H3 Common Lisp) (H3 Future plans)) */
var first = React.createClass({ render : function () {
    var g1273 = this;
    return React.DOM.div({  }, [React.DOM.h3({  }, ['Enjoying Parenscript']), React.DOM.p({  }, ['I haven\'t hacked Common Lisp since I got paid to do so in the\n  nineties. But ever since those days I\'ve loved the language,\n  especially the syntax and all it brings. Recently I\'ve been coding a\n  lot of javascript, which is fine, but I\'ve often wondered what it\n  would be like if I could change the language. I looked\n  into ', React.DOM.a({ 'href' : '' }, ['sweet.js']), ' but it didn\'t take because I got\n  the feeling it was weaker and more complex than what I was looking\n  for. Well it turns out my preferred solution was hiding in plain\n  sight all along. I think I heard about parenscript years ago, but it\n  didn\'t click until last week. Since then I\'ve been on a\n  binge (meaning a few hours here and there between work and\n  family).']), React.DOM.h3({  }, ['Looking Around']), React.DOM.p({  }, ['Once I was interested, I started looking around. I used Chris Done\'s nice ', React.DOM.a({ 'href' : 'http://ircbrowse.net/browse/lisp?q=parenscript' }, ['ircbrowse']), '\n  tool to troll through the irc logs looking for "parenscript."\n  That\'s how I found Jeaye\'s recent, working tutorial on ', React.DOM.a({ 'href' : 'http://blog.jeaye.com/2015/09/27/parenscript-ajax/' }, ['parenscript and ajax']), '. Also Burton Samograd\'s ', React.DOM.a({ 'href' : 'https://github.com/burtonsamograd/sigil' }, ['Sigil']), ', which is a command-line parenscript compiler integrated into the\nnode.js ecosystem -- a handy tool for a javascripter like me, and one which\nlets you get started right away. Given the mountains of rust that had\naccumulated on my CL skills over the last decade and a half, this was\nexactly what I needed to start hacking quickly -- it gave me a\nclose-up view of the rewards that parenscript brings.']), React.DOM.h3({  }, ['Pulling the Trigger']), React.DOM.p({  }, [React.DOM.a({ 'href' : 'https://groups.google.com/forum/#!forum/parenscript' }, ['The parenscript mailing list ']), 'is bit hidden, requiring moderator approval to view\narchives. Curious, I overcame my laziness and requested access. At\nfirst, it looked like any other low-traffic list. But then I saw\nsomething that blew me away. It was a posting by Olaf Ruppert titled\n"React Macros." This caught my attention because I had been meaning\nto work with Facebook\'s React platform for a while, but had barely\nscratched the surface. Olaf\'s mail was short, but very sweet. He wrote:']), React.DOM.pre({  }, ['Hello list,\n\nI\'m currently writing an react application. \nYou might find these macros helpful.\n\nbest regards, olaf']), React.DOM.p({  }, ['Helpful? Olaf is clearly a master of understatement. Try\nbeautiful. Olaf\'s unassuming proffer was followed by about a hundred\nlines of code that wrapped React in a fresh lispy package (actually,\nabout half the code is a little example app). It contains syntactic\nsweeteners for React classes, methods, properties, and most important,\nDOM tree generation. Olaf had thereby done away with React\'s grossest\nfeature, the JSX minilanguage, in about 10 lines of code. If your\nbackground is anything like mine, go find and study this code (I will\nprobably share it more widely soon but you can\'t miss it on the\nlist). It will not only educate but insire you. I have been enjoying\nit ever since, but more to the point, as soon as I saw it I was\npossessed with a strong desire to run it. This was the force that\npushed me from browsing to actively hacking parenscript.']), React.DOM.h3({  }, ['Up and Running']), React.DOM.p({  }, ['I now had a mission: to run Olaf\'s React example. After some\n  quick calls to apt-get and npm, I had SBCL and Sigil set up, and I\n  was ready to try parenscript. sigil ran just fine on Olaf\'s code,\n  and output some js. I took a look, and it was pretty readable!\n  Olaf\'s macros had a couple of gensyms in them so it wasn\'t perfectly\n  clean, but parenscript did a good job of producing a straightforward\n  mapping to js, at least in my first few impressions. However, the\n  example didn\'t run. Turns out Olaf\'s code was written in 2013, and\n  React had made some changes since then. Now I needed to debug the\n  code, which I did not write, using a language/syntax I did not yet\n  know, and a framework I had barely looked at.']), React.DOM.p({  }, ['It was easy! After about 20 minutes I had updated Olaf\'s code\n  to work with the most recent version of React. (The issue was that\n  react no longer lets you directly invoke the return value from ', React.DOM.a({ 'href' : 'https://gist.github.com/sebmarkbage/d7bce729f38730399d28' }, ['createClass']), '). So my confidence was increased, and I experienced the thrill\n\t  of seeing a barebones todo list appear on screen.']), React.DOM.h3({  }, ['Scratching my own Itch']), React.DOM.p({  }, ['Next I thought I\'d try to build something of my own with\n  parenscript. Recently I\'ve been writing some js laden with promises,\n  which is a bit heavyweight, syntactically speaking. Promises in\n  javascript basically recreate the familiar control-flow semantics of\n  sequential code for asynchronous scenarios. This is nice, but you\n  end up writing a lot of functions, returning things, and passing a\n  lot of arguments around in ways I was hoping to abstract. For\n  example, here\'s a piece of javascript from my project:']), React.DOM.pre({  }, ['\n\nfunction eachElector(cursor) {\n\treturn db.nextObject(cursor).then(\n\t\tfunction(elector) {\n\t\t\tif (!elector) {\n\t\t\t\treturn;\n\t\t\t}\n\t\t\treturn eachVote(db.findVotes(election, elector.domain))\n\t\t\t\t.then(function() {\n\t\t\t\t\treturn eachElector(cursor);\n\t\t\t\t});\n\t\t});\n}\n  ']), React.DOM.p({  }, ['I thought about what I\'d rather see here, and could easily implement, and came up with:']), React.DOM.pre({  }, ['\n (defun each-elector (cursor)\n   (seq (chain db next-object)\n\t ((:ok (elector)\n\t    (if elector\n\t\t  (seq (each-vote (chain db (find-votes election (@ elector domain))))\n\t\t    ((:ok (each-elector cursor)))))))))\n']), React.DOM.p({  }, ['I\'ll do better next time, but I think this is an\n  improvement. There are fewer manually created functions, for\n  one. The point is, I had a design and started writing my macro. It\n  didn\'t work, so I started manually inserting format statements all\n  over the place and looking up things in the hyperspec until I\n  figured out the problem. This taught me a valuable lesson:\n  parenscript coding is Common Lisp coding. This is a good thing\n  because CL is a powerful language with decades of experience in\n  exactly the area we\'re using it for -- macros. But it means that\n  getting comfortable with parenscript involves getting comfortable\n  with CL, and this will not happen overnight. Around this time I discovered Malisper\'s excellent blog ', React.DOM.a({ 'href' : 'http://malisper.me/' }, ['Macrology']), ', which is\nfilled with thorough and well-written explanations of Common Lisp.']), React.DOM.p({  }, []), React.DOM.h3({  }, ['OMG Paredit']), React.DOM.h3({  }, ['Common Lisp']), React.DOM.h3({  }, ['Future plans'])]);
} });
(function () {
    var post = makeInstance(first);
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
                        (CREATE COMPONENT *POST PATH posts/id))))
       (CHAIN DOCUMENT (GET-ELEMENT-BY-ID container))))) */
ReactDOM.render(makeInstance(ReactRouter.Router, null, makeKeyedInstance(ReactRouter.Route, { path : '/', component : Blog }, makeKeyedInstance(ReactRouter.IndexRoute, { component : BlogIndex }), makeKeyedInstance(ReactRouter.Route, { component : Post, 'path' : 'posts/id' }))), document.getElementById('container'));
