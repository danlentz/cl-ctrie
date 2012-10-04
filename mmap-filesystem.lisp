(in-package :manardb)

(defun schema ()
  (loop for m across *mtagmaps*
    when m collect (mtagmap-schema m)))


(defun single-expression-file (filename)
  (with-open-file (file filename :if-does-not-exist nil)
    (when file
     (with-standard-io-syntax
       (let (*read-eval*)
	 (read file))))))

(defun (setf single-expression-file) (value filename)
  (with-open-file (file filename
                    :direction :output
                    :if-does-not-exist :create
                    :if-exists :supersede)
    (with-standard-io-syntax
      (prin1 value file)
      (terpri file))))


(defun write-schema (filename)
  (setf (single-expression-file filename) (schema)))

(defun read-schema (filename)
  (assert (probe-file filename))
  (single-expression-file filename))

(defun pathname-to-special-file (dirname filename)
  (merge-pathnames (make-pathname :name filename :type nil) dirname))

(defun pathname-to-schema (dirname)
  (pathname-to-special-file dirname "schema"))

(defmacro dir-version (pathname)
  `(single-expression-file (pathname-to-special-file ,pathname "version")))

(defmacro dir-schema (pathname)
  `(single-expression-file (pathname-to-schema ,pathname)))

(defmacro dir-replacement-target (pathname)
  `(single-expression-file
     (pathname-to-special-file ,pathname "replacement-target")))


(defun schema-superset-p (stored-schema our-supported-schema)
  (loop 
    for (aname atag alayout) in stored-schema
    always 
    (progn
      (assert
        (loop for (bname btag blayout) in our-supported-schema
          thereis (when (= atag btag)
                    (assert (layout-compatible-p alayout blayout)
                      (aname bname atag btag alayout blayout)
                      "Stored class ~A (~A) has a different layout from defined class ~A: ~A"
                      aname atag bname alayout)
                    t))
        (aname atag)
        "No support for stored class ~A (~A)" aname atag)
      t)))


(defun copy-all-mmaps (from-dir to-dir)
  (let (version)
    (tagbody 
     restart
       (setf version (dir-version from-dir))
       (assert version (from-dir to-dir) "Trying to copy from a datastore that has no version")
       (when (probe-file to-dir)
	 (mapc #'delete-file (osicat:list-directory to-dir))
	 (assert (not (osicat:list-directory to-dir))))
       (ensure-directories-exist to-dir)
       (assert (probe-file to-dir))
       (let ((files
	      (osicat:list-directory from-dir)))
	 (loop for f in files do
           (alexandria:copy-file f
             (merge-pathnames (make-pathname :type (pathname-type f) :name (pathname-name f))
               to-dir) :element-type '(unsigned-byte 8))))
       (unless (equalp version (dir-version from-dir))
	 (go restart)))))


(defun replace-all-mmaps (from-dir to-dir version)
  (setf (dir-replacement-target from-dir) to-dir)
  (let ((tmpdir (tmpdir)) done)
    (osicat-posix:rename
      (translate-logical-pathname to-dir)
      (translate-logical-pathname tmpdir))
    (unwind-protect
      (progn
        (assert (version-equalp version (dir-version tmpdir)) () 
          "Database updated to a new version (~A) while attempting to replace version ~A" 
          (dir-version tmpdir) version)
        (osicat-posix:rename
          (translate-logical-pathname from-dir)
          (translate-logical-pathname to-dir))
        (setf done t))
      (unless done
        (osicat-posix:rename
          (translate-logical-pathname tmpdir)
          (translate-logical-pathname to-dir))
        (setf (dir-replacement-target from-dir) nil)))
    (osicat:delete-directory-and-files tmpdir)))


(defun str (&rest args)
  (with-standard-io-syntax
    (let (*print-readably* *print-escape*)
      (format nil "~{~A~}" args))))


(defun tmpdir ()
  (ensure-directories-exist (merge-pathnames "tmp/" *mmap-base-pathname*))
  (loop 
    for counter = (random most-positive-fixnum)
    for name = (str "tmp/"
                 (short-site-name) "-"
                 (osicat-posix:getpid) "-"
                 (osicat-posix:gettimeofday) "-"
                 counter "/")
    for path = (merge-pathnames name  *mmap-base-pathname*)
    do (when (nth-value 1 (ensure-directories-exist path)) (return path))))


(defun maindir ()
  (merge-pathnames "main/" *mmap-base-pathname*))


(defmacro with-transaction ((&key message on-restart) &body body)
  "Copy the datastore to a temporary directory; open the datastore
from this temporary directory and execute BODY. If, at the end of
BODY, the main datastore has had another transaction on it, then run
ON-RESTART and restart the procedure. Otherwise atomically replace
the main datastore.

Should be safe, as it uses lockfiles. The initial copy will retry if a
transaction occurs while it is being performed.

It is slow copying the datastore. (A better copy than
alexandria:copy-file should be easy to implement.)

The proposed reflink(2) system call would make a radically more
efficient implementation possible."
  (alexandria:with-unique-names (restart transaction)
    `(flet ((,transaction ()
              ,@body)
             (,restart ()
               ,on-restart)) 
       (transact 
         :message ,message
         :body #',transaction
         :on-restart #',restart))))


(defun version-equalp (a b)
  (equalp a b))


(defun check-schema (&optional (dir *mmap-pathname-defaults*))
  (let ((schema (dir-schema dir)))
    (assert (schema-superset-p schema (schema))
      (schema) "Schema in ~A in not compatible" (maindir))))


(defun build-version (&optional (counter 0))
  `(,counter 
    ,(osicat-posix:getpid) 
    ,@(multiple-value-list (osicat-posix:gettimeofday)) 
    ,(random most-positive-fixnum)))

(defvar *transaction-copy-fail-restart-sleep* 10)


(defun transact (&key body on-restart message) 
  (declare (dynamic-extent body on-restart message)
    (optimize safety debug))
  (close-all-mmaps)
  (assert (not *stored-symbols*))
  (let ((tmpdir (tmpdir)) *stored-symbols* (*mmap-may-allocate* t))
    (unwind-protect
      (tagbody restart
        (close-all-mmaps)
        (handler-bind
          ((error (lambda (err)
                    (warn "Copying mmap files from ~A to ~A in transaction ~A failed: ~A"
                      (maindir) tmpdir message err)
                    (sleep (random *transaction-copy-fail-restart-sleep*)))))
          (copy-all-mmaps (maindir) tmpdir))
        (let ((*mmap-pathname-defaults* tmpdir))
          (check-schema)
          (open-all-mmaps)
          (return-from transact
            (multiple-value-prog1 (funcall body)
              (let ((version (dir-version tmpdir)))
                (setf (dir-version tmpdir) (build-version (1+ (first version))))                
                (handler-case (progn
                                (replace-all-mmaps tmpdir (maindir) version)
                                (setf tmpdir nil))
                  (error (err)
                    (warn "Restarting manardb transaction ~A: ~A" message err)
                    (funcall on-restart)
                    (go restart))))))))
      (when tmpdir (ignore-errors
                     (osicat:delete-directory-and-files tmpdir)))
      (close-all-mmaps))))


(defun clean-mmap-dir (&optional (dir *mmap-base-pathname*))
  "Unsafely remove all temporary directories from failed transactions
that were not cleaned up because the transactor crashed. Not tested or used."
  (osicat:delete-directory-and-files (merge-pathnames "tmp/" dir)))


(defun use-mmap-dir (dir &key (if-does-not-exist :create))
  "Set the memory mapped datastore to map files inside DIR."
  (close-all-mmaps)
  (let ((maindir   
          (let ((*mmap-base-pathname* dir))
            (maindir))))
    (cond ((probe-file (pathname-to-schema maindir))
            (check-schema maindir)
            (setf *mmap-base-pathname* dir)
            (setf *mmap-pathname-defaults* maindir)
            (open-all-mmaps)
            dir)
      (t
        (ecase if-does-not-exist
          (:create
            (ensure-directories-exist maindir)
            (setf (dir-schema maindir) (schema))
            (setf (dir-version maindir) (build-version))
            (use-mmap-dir dir :if-does-not-exist :error))
          (:error
            (error "Directory ~A does not contain a memory mapped datastore." dir))
          ((nil)))))))


(defun ensure-manardb (&optional (default-location (compute-default-location-for-store)))
  (unless (and (boundp '*mmap-base-pathname*) *mmap-base-pathname*)
    (use-mmap-dir default-location)))

                        
(defun instantiate-default-mm-object (mptr)
  (funcall (slot-value (mtagmap-class (mtagmap (mptr-tag mptr))) 'default-instantiator)
    (mptr-index mptr)))


(defmacro with-object-cache ((name &key (test ''equal)) &body body)
  "Lexically bind a function with NAME for BODY that, when passed an
object, will either instantiate a new memory mapped object for it, or
if the object is equal under TEST to a previous object passed to NAME,
will return the same memory mapped object."
  (alexandria:with-unique-names (cache string)
    `(let ((,cache (make-hash-table :test ,test)))
       (flet ((,name (,string)
		(or (gethash ,string ,cache)
                  (setf (gethash ,string ,cache)
                    (instantiate-default-mm-object
                      (lisp-object-to-mptr ,string))))))
	 ,@body))))

