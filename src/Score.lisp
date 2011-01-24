;;; This function will parse a whole score file to lists to be processed  
;;; We might want to track line/similar things for better error reporting later

(defun read-score-file (filename)
	(with-open-file (stream filename)
		(let ((end (gensym "end")))
			(loop for i = (read stream nil end)
				while (not (eq i end)) collect i)))) 

;;; Very rought testing right now

(defun test-ee-parse ()
	(let ((forms (read-score-file "../tests/ps2/test0.score")))
		;; forms has the lists of the 
		(when forms
			(let ((inner (car forms)))
				; first entry here should be a def-ee-fun
				; notice will be proper parsing here, this is just for test
				(when (eql (car inner) 'def-ee-fun)
					(loop for i in (nthcdr 3 inner) do	; skip the function name and assume no in-parameters
						; here we can start pushing out the instructions but just printing now
						(print i)))))))

