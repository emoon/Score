
(defun read-score-file (filename)
  "Read a score for processing, This code is very basic and should be rewritten to handle some better error messages when parsing and such"
  (with-open-file (stream filename)
    (let ((end (gensym "end")))
      (loop for i = (read stream nil end)
  		while (not (eq i end)) collect i)))) 

