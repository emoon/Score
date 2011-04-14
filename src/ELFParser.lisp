
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

(defstruct elf-section 
	name
	type
	flags
	addr
	offset
	size
	link
	info
	addralign
	entsize)

;
;
;
;

(defun read-string (stream length)
  (let ((string (make-string length)))
    (dotimes (i length)
      (setf (char string i) (code-char (read-byte stream))))
    string))

;
;
;

(defun read-half (stream)
  (let* ((l (read-byte stream))
         (r (read-byte stream)))
    (logior (ash r 8) l)))

(defun read-word (stream)
  (let* ((l (read-half stream))
	     (r (read-half stream)))
    (logior (ash r 16) l)))

(defun read-sword (stream) (read-word stream))
(defun read-off (stream) (read-word stream))
(defun read-addr (stream) (read-word stream))

;
;
;
;

(defun read-header (stream)
  (let ((header (make-elf-header)))
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
	(setf (elf-header-shstrndx header) (read-half stream)) header))

;
;
;
;

(defun read-section-header (stream)
  (let ((header (make-elf-section)))
  	(setf (elf-section-name header) (read-word stream))
  	(setf (elf-section-type header) (read-word stream))
  	(setf (elf-section-flags header) (read-word stream))
  	(setf (elf-section-addr header) (read-addr stream))
  	(setf (elf-section-offset header) (read-off stream))
  	(setf (elf-section-size header) (read-word stream))
  	(setf (elf-section-link header) (read-word stream))
  	(setf (elf-section-info header) (read-word stream))
  	(setf (elf-section-addralign header) (read-word stream))
  	(setf (elf-section-entsize header) (read-word stream)) header))

;
;
;
;

(defun read-elf-file ()
  (with-open-file (stream "c:/temp2/test.o" :element-type '(unsigned-byte 8))
  	(let ((header (read-header stream)))
  	  ; set offset to section start
  	  (file-position stream (elf-header-shoff header))
  	  (format t "elf header ~%")
  	  (print header)
  	  (format t "~%")
  	  (format t "~%")
  	  ; loop all sections
  	  (loop for i from 0 to (elf-header-shnum header) do
  	  	(print (read-section-header stream)))
  	  
	  )))

