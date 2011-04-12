
; structs 

(defstruct elf-header
    ident
	type
	machine
	version
	entry
	phoff
	shoff
	flags
	ehsize
	phentsize
	phnum
	shentsize
	shnum
	shstrndx)

;
;
;
;

(defun read-string (stream length)
  (let ((string (make-string length)))
    (dotimes (i length)
      (setf (char string i) (code-char (read-byte in))))
    string))

;
;
;

(defun read-elf-file (filename)
  (with-open-file (stream "c:/temp2/test.o" :element-type '(unsigned-byte 8))
  	(let ((header make-elf-header))
  	  (setf (elf-header-ident header) (read-string stream 16))
  	  
  	  
  	  )))


