(defpackage #:breeze.actor
  (:documentation "Simple actor model.")
  (:use #:cl)
  (:import-from #:breeze.channel
                #:channel
                #:make-channel
                #:emptyp)
  (:export #:actor
           #:id
           #:fn
           #:created-at
           #:thread
           #:channel-in
           #:channel-out
           #:find-actor
           ;; TODO rename deregister-actor
           #:deregister
           #:clear-actors))

(in-package #:breeze.actor)



;;;; Actors

(defvar *actors* (make-hash-table)
  "Collection of command-handlers")

(defvar *actor-id-counter* 0
  "Counter used to generate ids for actors")

;; (setf *actor-id-counter*  0)

(defvar *actors-lock* (bt:make-lock)
  "Mutex for *actors*")

(defun generate-actor-id ()
  (bt:with-lock-held (*actors-lock*)
    (loop
      for id = (incf *actor-id-counter*)
      unless (gethash id *actors*)
        do (setf (gethash id *actors*) t)
           (return id))))


;; TODO defgeneric
(defun register (actor)
  (bt:with-lock-held (*actors-lock*)
    (setf (gethash (id actor) *actors*) actor)))

(defun deregister (actor)
  (bt:with-lock-held (*actors-lock*)
    (remhash (if (numberp actor) actor (id actor)) *actors*)))

(defun find-actor (id &key errorp)
  (check-type id integer)
  (let ((actor (bt:with-lock-held (*actors-lock*)
                 (gethash id *actors*))))
    (when (and errorp (null actor))
      (error "Failed to find command with id ~S." id))
    actor))

;; TODO garbage collect the *actors* that are done (when?)
;; TODO How to detect if something went wrong?

#++
(bt:with-lock-held (*actors-lock*)
  (maphash (lambda (id actor)
             (when (or (donep actor)
                       (not (bt:thread-alive-p (thread actor))))
               (remhash id *actors*))
             ;; (break "~s ~s" id actor)
             )
           *actors*))

#++
(hash-table-count *actors*)

(defun clear-actors ()
  "Forget all the actors"
  (bt:with-lock-held (*actors-lock*)
    (clrhash *actors*)))

;; TODO move to thread.lisp
(defclass actor ()
  ((id
    :initform (generate-actor-id)
    :accessor id
    :documentation "The id of the actor.")
   (fn
    :initform nil
    :initarg :fn
    :accessor fn
    :documentation "The function to be executed by the actor.")
   (created-at
    :initform (get-internal-real-time)
    :accessor created-at
    :documentation "When (universal-time0 was this actor created?")
   (thread
    :initform nil
    :initarg :thread
    :accessor thread
    :documentation "The thread running the actor.")
   (channel-in
    :initform (make-channel)
    :accessor channel-in
    :type channel
    :documentation "The channel to send response to the actor.")
   (channel-out
    :initform (make-channel)
    :accessor channel-out
    :type channel
    :documentation "The channel to receive requests from the actor."))
  (:documentation "Contains the runtime data used by the actor."))

(defmethod initialize-instance :after ((actor actor) &key)
  (register actor))
