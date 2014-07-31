
; structs 

(defconstant +null+ (code-char 0))

(defstruct elf-header ident type machine version entry phoff shoff flags ehsize phentsize phnum shentsize shnum shstrndx)
(defstruct elf-section name-string name type flags addr offset size link info addralign entsize data)
(defstruct elf-symbol-table name-string name value size info other shndx)
(defstruct elf-file header sections symbol-table function-table text-offset data-offset rodata-offset bssdata-offset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers


(defun read-string (stream length)
  (let ((string (make-string length)))
    (dotimes (i length)
      (setf (char string i) (code-char (read-byte stream))))
    string))

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

(defun read-null-terminated-ascii (in)
  (with-output-to-string (s)
    (loop for char = (code-char (read-byte in))
      until (char= char +null+) do (write-char char s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun file-pos-rel (stream start-pos offset)
  (file-position stream (+ start-pos offset)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this code can be very simplified

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-symtab-entry (stream)
  (let ((symtab (make-elf-symbol-table)))
    (setf (elf-symbol-table-name symtab) (read-word stream))
    (setf (elf-symbol-table-value symtab) (read-addr stream))
    (setf (elf-symbol-table-size symtab) (read-word stream))
    (setf (elf-symbol-table-info symtab) (read-byte stream))
    (setf (elf-symbol-table-other symtab) (read-byte stream))
    (setf (elf-symbol-table-shndx symtab) (read-half stream)) symtab))

(defun read-symtable (stream file-offset offset size entsize)
  (file-pos-rel stream file-offset offset)
  (let* ((sym-count (/ size entsize))
       (sym-tab (make-array sym-count)))
  (loop for i from 0 below sym-count do
    (setf (aref sym-tab i) (read-symtab-entry stream))) sym-tab))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-section (sections name)
  (find-if #'(lambda (x) (string= (elf-section-name-string x) name)) sections))

(defun read-section-data (stream file-offset sections name)
  (let ((section (find-section sections name)))
    (when section
      (file-pos-rel stream file-offset (elf-section-offset section))
      (setf (elf-section-data section) (make-array (elf-section-size section) :element-type 'unsigned-byte))
      (read-sequence (elf-section-data section) stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun elf-read-stream (file-offset stream)
  (declare (optimize (speed 0) (safety 3) (debug 3)))

  (let ((elf-file (make-elf-file))
        (header (read-header stream)))

    (setf (elf-file-header elf-file) header)
    (file-pos-rel stream file-offset (elf-header-shoff header))

    ; read all sections
  
    (let ((sections (make-array (elf-header-shnum header) :element-type 'elf-section)))
      (loop for i from 0 below (elf-header-shnum header) do
        (setf (aref sections i) (read-section-header stream)))

      (setf (elf-file-sections elf-file) sections)

      ; resolve the names for the sections
  
      (let ((symtab-section-offset (elf-section-offset (aref sections (elf-header-shstrndx header)))))
        (loop for i from 0 below (elf-header-shnum header) do
          ; set file-position to where the string is
          (file-pos-rel stream file-offset (+ symtab-section-offset (elf-section-name (aref sections i))))
          (setf (elf-section-name-string (aref sections i)) (read-null-terminated-ascii stream))))

      ; Setup the symbols table

      (let ((symtab-section (find-section sections ".symtab")))

      (when symtab-section
        (setf (elf-file-symbol-table elf-file) (read-symtable stream file-offset
                                               (elf-section-offset symtab-section) 
                                               (elf-section-size symtab-section)  
                                               (elf-section-entsize symtab-section)))
      ; resolve the names
  
        (let ((symtab-section-offset (elf-section-offset (aref sections (elf-section-link symtab-section))))
              (symbol-table (elf-file-symbol-table elf-file))
              (symbol-hash (make-hash-table)))
          (loop for i from 0 below (length symbol-table) do
            (file-pos-rel stream file-offset (+ symtab-section-offset (elf-symbol-table-name (aref symbol-table i))))
            (setf (elf-symbol-table-name-string (aref symbol-table i)) (read-null-terminated-ascii stream)))

          ; create a hash-table with function mappings
    
          (loop for i from 0 below (length symbol-table) do
            (when (eq (logand (symbol-table-info (aref symbol-table i)) #xf) 2)
            (setf (gethash (elf-symbol-table-name-string (aref symbol-table i)) symbol-hash) 
                           (elf-symbol-table-value (aref symbol-table i)))))

          (setf (elf-file-function-table elf-file) symbol-hash))
  
        ; read the data's to the sections that needs it

        (read-section-data stream file-offset sections ".text")
        (read-section-data stream file-offset sections ".data")
        (read-section-data stream file-offset sections ".rodata")))) elf-file)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun elf-file-read (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (elf-read-stream 0 stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Archiver Reader

(defstruct archive-header name date uid gid mode size fmag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-seq (stream length)
  (let ((temp (make-array length :element-type 'unsigned-byte)))
    (read-sequence temp stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun archive-header-read (stream)
  (let ((header (make-archive-header)))
    (setf (archive-header-name header) (read-string stream 16))
    (setf (archive-header-date header) (parse-integer (read-string stream 12)))
    (setf (archive-header-uid header) (read-seq stream 6))
    (setf (archive-header-gid header) (read-seq stream 6))
    (setf (archive-header-mode header) (read-seq stream 8))
    (setf (archive-header-size header) (parse-integer (read-string stream 10)))
    (setf (archive-header-fmag header) (read-string stream 2)) 
    (setf (archive-header-size header) (logand (+ (archive-header-size header) 1) (lognot 1))) header))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun archive-read-file ()
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (with-open-file (stream "c:/temp2/Libc.a" :element-type '(unsigned-byte 8))

  ; Make sure we have the correct header

  (let ((header-string (read-string stream 8)))
    (unless (search "!<arch>" header-string)
      (return-from archive-read-file nil))

  ; scan for files 

  (loop for i from 0 to 1000000 do
    (let ((temp (archive-header-read stream)))
      (unless (search "`" (archive-header-fmag temp))
         (return-from archive-read-file))

    (pprint temp)

       ; parse elf file 

    (let ((current-pos (file-position stream)))
      (unless (eq (search "/ " (archive-header-name temp)) 0)
        (elf-read-stream current-pos stream))

         ; search to next entry
        
      (file-position stream (+ current-pos (archive-header-size temp)))))))))


