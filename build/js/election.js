/* (DEFMACRO DEFUN* (NAME ARGS &BODY BODY)
     Define a function at the lisp top level.
     `(EVAL-WHEN (COMPILE-TOPLEVEL) (DEFUN ,NAME ,ARGS ,@BODY))) */

/* (DEFUN* NORMALIZE (X) (FORMAT *QUERY-IO* nor ~s~% X)
    (IF (LISTP X)
        (LET ((WITH-TAG (CASE (CAR X) (ERR X) (OK X) (T (CONS OK X)))))
          (CASE (LENGTH (CDR WITH-TAG))
            (2 X)
            (1 (CONS (CAR X) (CONS 'NIL (CDR X))))
            (0 (ERROR too short))))
        (ERROR not list))) */

/* (DEFUN* MAKE-FUNS (ONE &OPTIONAL TWO) (FORMAT *QUERY-IO* mf ~s ~s~% ONE TWO)
    (FLET ((CHECK (K C)
             (FORMAT *QUERY-IO* c ~s ~s~% K C)
             (IF (EQ (CAR C) K)
                 `(LAMBDA ,(CADR C) ,(CADDR C))
                 NIL)))
      (IF (OR (AND (CHECK OK ONE) (CHECK OK TWO))
              (AND (CHECK ERR ONE) (CHECK ERR TWO)))
          (ERROR two errs or two oks)
          (LET ((OK (OR (CHECK OK ONE) (CHECK OK TWO)))
                (ERR (OR (CHECK ERR ONE) (CHECK ERR TWO))))
            (FORMAT *QUERY-IO* ok ~s err ~s~% OK ERR)
            (IF ERR
                `(THEN ,OK ,ERR)
                `(THEN ,OK)))))) */

/* (DEFUN* GET-THENS (SEQS) (FORMAT *QUERY-IO* gt ~s~% SEQS)
    (FLET ((GET-ONE (S)
             (FORMAT *QUERY-IO* go ~s~% S)
             (CASE (LENGTH S)
               (1
                (LET ((RN (NORMALIZE (CAR S))))
                  (MAKE-FUNS RN)))
               (2
                (LET ((RN0 (NORMALIZE (CAR S))) (RN1 (NORMALIZE (CADR S))))
                  (MAKE-FUNS RN0 RN1))))))
      (MAPCAR #'GET-ONE SEQS))) */

/* (DEFMACRO SEQ (PROMISE &REST SEQS)
     (FORMAT *QUERY-IO* s ~s ~s~% PROMISE SEQS)
     `(CHAIN ,PROMISE ,@(GET-THENS SEQS))) */

/* (VAR _DB (REQUIRE ./db)) */
var _db = require('./db');
/* (VAR DO-VOTE (REQUIRE ./vote)) */
var doVote = require('./vote');
/* (VAR CLIB (REQUIRE ./clib)) */
var clib = require('./clib');
/* (VAR DB (NEW (_DB))) */
var db = new _db();
/* (VAR *Q (REQUIRE q)) */
var Q = require('q');
/* (VAR NUM-VOTING-THREADS 2) */
var numVotingThreads = 2;
/* (VAR MAX-ELECTORS 0) */
var maxElectors = 0;
/* (DEFUN ELECTION-RUN ()
     (LET ((ELECTION (NEW (*DATE))))
       (FLET ((INSERT-RUN ()
                (CHAIN DB
                 (INSERT-RUN
                  (CREATE ELECTION ELECTION FINISHED (NEW (*DATE)) TYPE
                   ELECTION))))
              (INSERT-CANDIDATES ()
                (CHAIN CONSOLE (LOG INSERT-CANDIDATES))
                (LET* ((CUTOFF (- (CHAIN *DATE (NOW)) (@ CLIB ONE-MONTH)))
                       (CURSOR
                        (CHAIN DB (GET-EACH-CANDIDATE-PAGE-SINCE CUTOFF))))
                  (FLET ((DO-INSERT-CANDIDATE (COUNT)
                           (CHAIN PROCESS STDOUT
                            (WRITE (+ (@ C-PAGE DOMAIN) - COUNT ;)))
                           (CHAIN DB
                            (INSERT-CANDIDATE
                             (CREATE DOMAIN (@ C-PAGE DOMAIN) ELECTION ELECTION
                              POWER COUNT VOTING-PAGE (@ C-PAGE PAGE) STATUS
                              NEW DETAILS NIL))))
                         (INSERT-CANDIDATE (C-PAGE)
                           (SEQ
                            (CHAIN DB
                             (COUNT-CANDIDATE-PAGES-UNDER (@ C-PAGE DOMAIN)
                              CUTOFF))
                            ((OK (COUNT) (DO-INSERT-CANDIDATE COUNT)))))
                         (REC ()
                           (SEQ (CHAIN DB (NEXT-OBJECT CURSOR))
                            ((OK (C-PAGE)
                              (IF (C-PAGE)
                                  (SEQ
                                   (CHAIN DB
                                    (FIND-ONE-CANDIDATE-BY-DOMAIN-AND-ELECTION
                                     (@ C-PAGE DOMAIN) ELECTION))
                                   ((OK (ALREADY)
                                     (AND (NOT ALREADY)
                                          (INSERT-CANDIDATE C-PAGE))))
                                   ((OK (REC))))))))))
                    (REC))))
              (VOTE ()
                (CHAIN PROCESS STDOUT (WRITE vote))
                (LET ((CURSOR (CHAIN DB (FIND-ELECTORS ELECTION)))
                      (NUM-ELECTORS 0))
                  (FLET ((LOOP0 ()
                           (SEQ (CHAIN DB (NEXTOBJECT CURSOR))
                            ((OK (ELECTOR)
                              (IF (OR (NOT ELECTOR)
                                      (AND MAX-ELECTORS
                                           (>= (++ NUM-ELECTORS)
                                               MAX-ELECTORS)))
                                  UNDEFINED
                                  (PROGN
                                   (CHAIN PROCESS STDOUT (WRITE v))
                                   (SEQ (DO-VOTE DB ELECTION ELECTOR)
                                    ((OK (RESOURCES)
                                      (PROGN
                                       (CHAIN PROCESS STDOUT
                                        (WRITE
                                         (+ (@ ELECTOR DOMAIN) #r= RESOURCES
                                            .)))
                                       '(VOTED)))
                                     (ERR (ERR) (ARRAY FAILED (@ ERR STACK))))
                                    ((OK (STATUS)
                                      (SEQ
                                       (CHAIN DB
                                        (UPDATE-ELECTOR-STATUS ELECTOR
                                         (AREF STATUS 0) (AREF STATUS 1)))
                                       ((OK (LOOP0))))))))))))))
                    (LET ((THREADS ([])))
                      (DOTIMES (I NUM-VOTING-THREADS)
                        (CHAIN THREADS (PUSH (LOOP0))))
                      (CHAIN Q (ALL THREADS))))))
              (TALLY ()
                (FLET ((CALC-RANK (POWER)
                         (IF (< POWER 5)
                             1
                             (AND (< POWER 20) 2 3)))
                       (EACH-VOTE (CURSOR)
                         (LET ((VOTE NIL))
                           (FLET ((DO-P-RANK ()
                                    (SEQ
                                     (CHAIN DB
                                      (FIND-P-RANK ELECTION
                                       (@ VOTE UNDER-POWER)))
                                     ((OK (P-RANK)
                                       (IF P-RANK
                                           (PROGN
                                            (CHAIN PROCESS STDOUT (WRITE .ri))
                                            (CHAIN DB
                                             (INCREMENT-P-RANK P-RANK
                                              (CALC-RANK (@ VOTE POWER)))))
                                           (PROGN
                                            (CHAIN PROCESS STDOUT (WRITE .r))
                                            (CHAIN DB INSERT-P-RANK
                                             (CREATE ELECTION ELECTION
                                              UNDER-POWER (@ VOTE UNDER-POWER)
                                              RANK
                                              (CALC-RANK
                                               (@ VOTE POWER))))))))))
                                  (DO-DEPENDENCE ()
                                    (SEQ
                                     (CHAIN DB
                                      (FIND-DEPENDENCE ELECTION
                                       (@ VOTE ELECTOR)))
                                     ((OK (DEPENDENCE)
                                       (IF (DEPENDENCE)
                                           (PROGN
                                            (CHAIN PROCESS STDOUT (WRITE .di))
                                            (CHAIN DB
                                             (INCREMENT-DEPENDENCE DEPENDENCE
                                              1)))
                                           (PROGN
                                            (CHAIN PROCESS STDOUT WRITE .d)
                                            (CHAIN DB
                                             (INSERT-DEPENDENCE
                                              (CREATE ELECTION ELECTION ELECTOR
                                               (@ VOTE ELECTOR) DEPENDENCE
                                               1))))))))))
                             (SEQ (CHAIN DB (NEXT-OBJECT CURSOR))
                              ((OK (_VOTE) (SETQ VOTE _VOTE)
                                (IF VOTE
                                    (SEQ (DO-P-RANK) ((OK (DO-DEPENDENCE)))
                                     ((OK (EACH-VOTE CURSOR)))))))))))
                       (EACH-ELECTOR (CURSOR)
                         (SEQ (CHAIN DB NEXT-OBJECT)
                          ((OK (ELECTOR)
                            (IF ELECTOR
                                (SEQ
                                 (EACH-VOTE
                                  (CHAIN DB
                                   (FIND-VOTES ELECTION (@ ELECTOR DOMAIN))))
                                 ((OK (EACH-ELECTOR CURSOR))))))))))
                  (EACH-ELECTOR (CHAIN DB (FIND-ELECTORS ELECTION)))))
              (MAIN ()
                (SEQ (INSERT-CANDIDATES) ((OK (VOTE))) ((OK (INSERT-RUN)))
                 ((OK (TALLY))))))
         (CREATE MAIN MAIN)))) */
function electionRun() {
    var election = new Date();
    var insertRun = function () {
        return db.insertRun({ 'election' : election,
                              'finished' : new Date(),
                              'type' : 'election'
                            });
    };
    var insertCandidates = function () {
        console.log('insert-candidates');
        var cutoff = Date.now() - clib.oneMonth;
        var cursor = db.getEachCandidatePageSince(cutoff);
        var doInsertCandidate = function (count) {
            process.stdout.write(cPage.domain + '-' + count + ';');
            return db.insertCandidate({ 'domain' : cPage.domain,
                                        'election' : election,
                                        'power' : count,
                                        'voting-page' : cPage.page,
                                        'status' : 'new',
                                        'details' : null
                                      });
        };
        var insertCandidate = function (cPage) {
            return db.countCandidatePagesUnder(cPage.domain, cutoff).then(function (count) {
                return doInsertCandidate(count);
            });
        };
        var rec = function () {
            return db.nextObject(cursor).then(function (cPage) {
                return cPage() ? db.findOneCandidateByDomainAndElection(cPage.domain, election).then(function (already) {
                    return !already && insertCandidate(cPage);
                }).then(function () {
                    return rec();
                }) : null;
            });
        };
        return rec();
    };
    var vote = function () {
        process.stdout.write('vote');
        var cursor = db.findElectors(election);
        var numElectors = 0;
        var loop0 = function () {
            return db.nextobject(cursor).then(function (elector) {
                if (!elector || maxElectors && plus(numElectors) >= maxElectors) {
                    return undefined;
                } else {
                    process.stdout.write('v');
                    return doVote(db, election, elector).then(function (resources) {
                        process.stdout.write(elector.domain + '#r=' + resources + '.');
                        return ['voted'];
                    }, function (err) {
                        return ['failed', err.stack];
                    }).then(function (status) {
                        return db.updateElectorStatus(elector, status[0], status[1]).then(function () {
                            return loop0();
                        });
                    });
                };
            });
        };
        var threads = [];
        for (var i = 0; i < numVotingThreads; i += 1) {
            threads.push(loop0());
        };
        return q.all(threads);
    };
    var tally = function () {
        var calcRank = function (power) {
            return power < 5 ? 1 : power < 20 && 2 && 3;
        };
        var eachVote = function (cursor) {
            var vote = null;
            var doPRank = function () {
                return db.findPRank(election, vote.underPower).then(function (pRank) {
                    if (pRank) {
                        process.stdout.write('.ri');
                        return db.incrementPRank(pRank, calcRank(vote.power));
                    } else {
                        process.stdout.write('.r');
                        return db.insertPRank.create('election', election, 'under-power', vote.underPower, 'rank', calcRank(vote.power));
                    };
                });
            };
            var doDependence = function () {
                return db.findDependence(election, vote.elector).then(function (dependence) {
                    if (dependence()) {
                        process.stdout.write('.di');
                        return db.incrementDependence(dependence, 1);
                    } else {
                        process.stdout.write['.d'];
                        return db.insertDependence({ 'election' : election,
                                                     'elector' : vote.elector,
                                                     'dependence' : 1
                                                   });
                    };
                });
            };
            return db.nextObject(cursor).then(null);
        };
        var eachElector = function (cursor) {
            return db.nextObject.then(function (elector) {
                return elector ? eachVote(db.findVotes(election, elector.domain)).then(function () {
                    return eachElector(cursor);
                }) : null;
            });
        };
        return eachElector(db.findElectors(election));
    };
    var main = function () {
        return insertCandidates().then(function () {
            return vote();
        }).then(function () {
            return insertRun();
        }).then(function () {
            return tally();
        });
    };
    return { 'main' : main };
};

