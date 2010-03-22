(in-package :loki-object-system)
(define-constant +standard-read-string+
    (get-macro-character #\" (copy-readtable nil)))

(defun string-reader (stream char)
  "Convert lisp strings to loki strings.

We do this with the readtable as opposed to doing it elsewhere because
every loki string is represented to loki programmers as a loki object."
  (make-string-object :data (funcall +standard-read-string+ stream char)))

(defun comma-reader (stream char)
  (values))

(defun call-with-loki-readtable (thunk)
  (declare (type function thunk))
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (set-macro-character #\" #'string-reader)
    (set-syntax-from-char #\, #\Space)
    (set-macro-character #\, #'comma-reader)
    (let ((*package* (find-package :loki-user)))
      (walk-loki-list (cl:funcall thunk)))))

(defun walk-loki-list (list)
  (declare (type list list))
  (mapcar #'make-simple-data-object
          list))

(defun make-simple-data-object (input)
  (typecase input
    (complex (make-complex-object :data input))
    (integer (make-integer-object :data input))
    (rational (make-rational-object :data input))
    (string (make-string-object :data input))
    (symbol (princ-to-string input))
    (cons (cons (make-simple-data-object (car input))
                (and (not (endp (cdr input)))
                     (make-simple-data-object (cdr input)))))
    (otherwise input)))



(defmacro with-loki-syntax (&body body)
  `(call-with-loki-readtable (lambda () ,@body)))


(defun loki-repeating-read (input)
  (declare (type (or stream string) input))
  (let ((input (if (stringp input)
                   (make-string-input-stream input)
                   input)))
    (with-loki-syntax
      (do* ((x nil (read input))
            (result () (push x result)))
           ((eq nil (peek-char t input nil nil)) (nreverse result))
        (print x *trace-output*)))))


(defmacro transform (input)
  "Convert INPUT to lisp compile it and run it.

Right now this is the primary way to convert ioke/loki syntax to the
internal lisp representation."
  (let ((tree (loki-repeating-read input)))
    `(call *ground* ,(car tree)
           ,@(cadr tree))))