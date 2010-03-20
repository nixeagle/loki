(in-package :loki-object-system)

(defstruct (method-object
             (:conc-name method-)
             (:include data-mixin)
             (:constructor))
  (declarations '() :type list)
  (function nil :type (or null function)) ;Slot is temp nil at load/compile
  (lambda-list '() :type list)
  (forms '() :type list))

(defmethod print-object ((obj method-object) stream)
  (if *print-readably*
      (format *standard-output*
              "#S(~S :direct-mimics ~S :direct-cells ~S :lambda-list ~S :forms ~S :docstring ~S :declarations ~S)"
              (type-of obj) (direct-mimics obj)
              (direct-cells obj) (method-lambda-list obj)
              (method-forms obj)
              (docstring obj)
              (method-declarations obj))
      (call-next-method)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-method-lambda-function (this-method declarations docstring
                                      arguments forms)
    "Create a method lambda form for THIS-METHOD.

THIS-METHOD is a loki `method-object'. The remaining parameters should go
in the equivalent places that they go in for a commmon lisp function.

The insertion of the DOCSTRING, DECLARATIONS, ARGUMENTS and FORM on
THIS-METHOD is handled elsewhere and is not the concern of this
function.

The expected return value of this is a lambda form. Refer to the common
lisp MOP for semantics."
    `(lambda (message receiver context ,@arguments)
       ,docstring
       (declare (type object context message receiver))
       ,@declarations
       (let ((this-method ,this-method)
             (|self| (or receiver *ground*))
             (@ (or receiver *ground*))
             (|currentMessage| message)
             (context (or context *context*))
             (|surroundingContext| *surrounding-context*))
         (declare (ignorable this-method |self| @ |currentMessage|
                             context |surroundingContext|))
         ,@forms))))

(defmacro make-method-lambda (this-method declarations docstring
                              arguments forms)
  "Macro helper we use for now to take the generated lambda form and
create a compiled closure"
  (make-method-lambda-function this-method declarations docstring
                               arguments forms))


(defmethod make-load-form ((self method-object) &optional env)
  (declare (ignore env))
  (values
   `(make-method-object :direct-mimics ',(direct-mimics self)
                        :direct-cells ',(direct-cells self)
                        :lambda-list ',(method-lambda-list self)
                        :docstring ',(docstring self)
                        :declarations ',(method-declarations self)
                        :forms ',(method-forms self))
   `(setf (method-function ',self)
          (make-method-lambda ,self
                              ,(method-declarations self)
                              ,(docstring self)
                              ,(method-lambda-list self)
                              ,(method-forms self)))))

(defun make-method-function (lambda-list body)
  "Take lambda-list and a body and return a loki method-object."
  (multiple-value-bind (forms declarations docstring)
      (parse-body body :documentation t :whole t)
    (let ((this-method (make-method-object :lambda-list lambda-list
                                           :forms forms
                                           :docstring docstring
                                           :declarations declarations)))
      (setf (method-function this-method)
            (compile nil (make-method-lambda-function this-method declarations
                                          docstring lambda-list forms)))
      this-method)))

(defmacro make-method (lambda-list &body body)
  `(make-method-function ',lambda-list ',@body))
