
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
  	  (setf (elf-header-type header) (read-half stream))
  	  (setf (elf-header-machine header) (read-half stream))
  	  (setf (elf-header-version header) (read-word stream))
  	  (setf (elf-header-entry header) (read-addr stream))
  	  (setf (elf-header-phoff header) (read-off stream))
  	  (setf (elf-header-shoff header) (read-off stream))
  	  (setf (elf-header-flags header) (read-word stream))
  	  (setf (elf-header-ehsize header) (read-half stream))
  	  (setf (elf-header-phentsize header) (read-half stream))
  	  (setf (elf-header-phnum header) (read-half stream))
  	  (setf (elf-header-shentsize header) (read-half stream))
  	  (setf (elf-header-shnum header) (read-half stream))
  	  (setf (elf-header-shstrndx header) (read-half stream)) header)))


