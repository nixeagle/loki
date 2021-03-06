(in-package :loki-object-system)

(setf (direct-cell *base* "kind") "Base")
(setf (direct-cell *base* "inspect") "Base")
(setf (direct-cell *base* "notice") "Base")

(setf (direct-cell *base* "hash")
      (make-method ()
        "Hash on Base is as if `eq' is used.

This does not seem to be returning the right hash for two otherwise eq
loki objects."
        (sxhash |self|)))

(setf (direct-cell *base* "cell")
      (make-method (cell-name)
        "Return a cell as if by `cell'."
        (etypecase cell-name
          (string-object (cell |self| (string-data cell-name)))
          (symbol-object (cell |self| (symbol-name (symbol-data cell-name)))))))

(setf (direct-cell *base* "==")
      (make-method (other)
        "True iff SELF is `eq' to OTHER."
        (declare (type object other))
        (eq |self| other)) )

(setf (direct-cell *base* "=")
      (make-method (place value)
        "Set PLACE on @ to VALUE."
        (setf (direct-cell @ place) value)))

(setf (direct-cell *base* "documentation")
      (make-method ()
        "Get documentation string for the receiver."
        (docstring @)))

(setf (direct-cell *base* "documentation=")
      (make-method (text)
        "Make TEXT the documentation string of receiver."
        (declare (type string-object text))
        (setf (docstring @) (string-data text))
        text))


;;;object context message reciever
;;; reciever is @ or self
;;;
;;; currentMessage is message that initiated activation of method
;;;
;;; surroundingContext object that represents the context where this
;;; method was called from

(add-direct-mimic *Ground* *Base*)

(setf (direct-cell *Ground* "Ground") *Ground*)
(setf (direct-cell *Ground* "kind") "Ground")


(add-direct-mimic *origin* *ground*)
(setf (direct-cell *origin* "kind") "Origin")


(defparameter *Default-Behavior-Definitions* (make-object))
(setf (direct-cell *default-behavior-definitions* "kind")
      "DefaultBehavior Definitions")
(setf (direct-cell *default-behavior-definitions* "inspect")
      "DefaultBehavior Definitions")
(setf (direct-cell *default-behavior-definitions* "notice")
      "DefaultBehavior Definitions")
(setf (direct-cell *default-behavior-definitions* "method")
      (make-method (&optional body)
        "Very crude version of Ioke `method'.

We do not handle any arg parsing or optional user supplied documentation
strings. These will be added in the future as the user level interface
starts to improve.

For starters this really begs to be a macro."
        (let ((this-method
               (make-method-object :lambda-list ()
                                   :forms (list body)
                                   :docstring ""
                                   :declarations ())))
          (setf (method-function this-method)
                (lambda () body))
          this-method)))

(add-direct-mimic *ground* *default-behavior-definitions*)

(defpackage #:loki-user
  (:use))
