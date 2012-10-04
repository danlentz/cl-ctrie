;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :manardb)


(defun finalize-all-mmaps ()
  (loop for m across *mtagmaps* 
	when m do (mtagmap-finalize m)))


(defun clear-caches-hard ()
  (loop
    for package in (list-all-packages)
    do (do-all-symbols (sym package)
         (remf (symbol-plist sym) 'mm-symbol))))


(defun clear-caches ()
  (loop for sym in *stored-symbols* 
	for removed = (remprop sym 'mm-symbol)
	do (assert removed)) 
  (setf *stored-symbols* nil))


(defun verify-caches-cleared ()
  (loop for package in (list-all-packages)
    do (do-all-symbols (sym package)
         (assert (not (get sym 'mm-symbol))))))


(defun close-all-mmaps ()
  "Closes the datastore, unmapping and closing all files. Afterwards,
  a new datastore can be opened in a different locaiton."
  (clear-caches)
  (loop for m across *mtagmaps*
    do (when m (mtagmap-close m))))


(defun open-all-mmaps ()
  "Maps the datastore into memory."
  (finalize-all-mmaps)
  (assert (or (not (mtagmap-closed-p (mtagmap 0))) (not *stored-symbols*)))
  (loop for m across *mtagmaps*
    do (when m (when (mtagmap-closed-p m) (mtagmap-open m)) (mtagmap-check m))))


(defun shrink-all-mmaps ()
  "Truncate all mmaps to smallest size (rounded up to the nearest
  page) which can contain all their data."
  (loop for m across *mtagmaps* 
    do (when (and m (not (mtagmap-closed-p m))) (mtagmap-shrink m))))


(defun wipe-all-mmaps ()
  "Delete all objects from all classes."
  (clear-caches)
  (loop for m across *mtagmaps*
	when (and m (not (mtagmap-closed-p m)))
	do (setf (mtagmap-next m) (mtagmap-first-index m)) (mtagmap-shrink m)))


(defun print-all-mmaps (&optional (stream *standard-output*))
  "Describe the state of the datastore"
  (loop for m across *mtagmaps*
	when (and m (not (mtagmap-closed-p m)))
	do (format stream "~&~A~%" m)))

(define-lisp-object-to-mptr)


