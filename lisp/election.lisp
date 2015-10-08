(var _db (require "./db"))
(var do-vote (require "./vote"))
(var clib (require "./clib"))
(var db (new (_db)))
(var *q (require "q"))

(var num-voting-threads 2)
(var max-electors 0)

(defun election-run()
  (let ((election (new (*date))))

	(flet
		((insert-run ()
		   (chain db (insert-run (create :election election
										 :finished (new (*date))
										 :type :election))))
		 (insert-candidates ()
		   (chain console (log :insert-candidates))
		   (let* ((cutoff (- (chain *date (now)) (@ clib one-month)))
				  (cursor (chain db (get-each-candidate-page-since cutoff))))
			 (flet
				 ((do-insert-candidate (c-page count) 
					(chain process stdout
						   (write (+ (@ c-page domain) "-" count ";")))
					(chain db (insert-candidate
							   (create :domain (@ c-page domain)
									   :election election
									   :power count
									   :voting-page (@ c-page page)
									   :status :new
									   :details nil))))
				  (insert-candidate (c-page)
					(seq
					 (chain db (count-candidate-pages-under (@ c-page domain) cutoff))
					 ((:ok (count)
						   (do-insert-candidate c-page count)))))
				  (rec ()
					(seq
					 (chain db (next-object cursor))
					 ((:ok (c-page)
						   (if (c-page)
							   (seq
								(chain db (find-one-candidate-by-domain-and-election
										   (@ c-page domain) election))
								((:ok (already)
									  (and (not already) (insert-candidate c-page))))
								((:ok (rec))))))))))
			   (rec))))
		 (vote ()
				 (chain process stdout (write "vote"))
				 (let ((cursor (chain db (find-electors election)))
					   (num-electors 0))
				   (flet
					   ((loop0 ()
						  (seq
						   (chain db (nextObject cursor))
						   ((:ok (elector)
								 (if (or (not elector)
										 (and max-electors
											  (>= (++ num-electors) max-electors)))
									 undefined
									 (progn
									   (chain process stdout (write "v"))
									   (seq
										(do-vote db election elector)
										((:ok (resources) 
											  (progn
												(chain process stdout
													   (write (+ (@ elector domain)
																 "#r=" resources ".")))
												'(:voted)))
										 (:err (err) (array :failed (@ err stack))))
										((:ok (status)
											  (seq
											   (chain db
													  (update-elector-status elector
																			 (aref status 0)
																			 (aref status 1)))
											   ((:ok (loop0))))))))))))))
					 (let ((threads ([])))
					   (dotimes (i num-voting-threads)
						 (chain threads (push (loop0))))
					   (chain Q (all threads))))))
		 (tally ()
		   (flet ((calc-rank (power) 
					(if (< power 5) 1 (and (< power 20) 2 3)))
				  (each-vote (cursor)
					(let ((vote nil))
					  (flet ((do-p-rank ()
							   (seq
								(chain db (find-p-rank election
													   (@ vote under-power)))
								((:ok (p-rank)
									  (if p-rank
										  (progn 
											(chain process stdout (write ".ri"))
											(chain db (increment-p-rank
													   p-rank 
													   (calc-rank (@ vote power)))))
										  (progn
											(chain process stdout (write ".r"))
											(chain db insert-p-rank
												   (create :election election
														   :under-power (@ vote under-power)
														   :rank (calc-rank (@ vote power))))))))))
							 (do-dependence ()
							   (seq (chain db (find-dependence election (@ vote elector)))
									((:ok (dependence)
										  (if (dependence)
											  (progn (chain process stdout (write ".di"))
													 (chain db (increment-dependence
																dependence 1)))
											  (progn (chain process stdout write ".d")
													 (chain db (insert-dependence
																(create :election election
																		:elector (@ vote elector)
																		:dependence 1))))))))))
						(seq (chain db (next-object cursor))
							 ((:ok (_vote)
								   (setq vote _vote)
								   (if vote
									   (seq (do-p-rank)
											((:ok (do-dependence)))
											((:ok (each-vote cursor)))))
								   ))))))
				  (each-elector (cursor)
					(seq (chain db next-object)
						 ((:ok (elector)
							   (if elector
								   (seq (each-vote (chain db (find-votes election (@ elector domain))))
										((:ok (each-elector cursor))))))))))
			 (each-elector (chain db (find-electors election)))))

		 (main ()
		   (seq (insert-candidates)
				((:ok (vote)))
				((:ok (insert-run)))
				((:ok (tally))))))
	  (create :main main))))



