(in-package :loki-object-system)

(defun call-method (object receiver &rest args)
  "Apply ARGS to OBJECT's `method-function'."
  (declare (type method-object object))
  (apply (method-function object) (make-object)
         receiver (make-object) args))

(defun call (receiver method-name &rest args)
  (declare (type object receiver))
  (apply (method-function (cell receiver method-name))
         (make-object) receiver (make-object) args))