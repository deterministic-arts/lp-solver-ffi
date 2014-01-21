
(in-package "LP-SOLVER-FFI")

(defctype lprec :pointer)

(define-foreign-library liblpsolve
  (:unix "liblpsolve55.so"))

(use-foreign-library liblpsolve)

(defcfun ("make_lp" lps-make-lp) lprec (rows :int) (columns :int))
(defcfun ("delete_lp" lps-delete-lp) :void (ptr lprec))
(defcfun ("set_add_rowmode" lps-set-add-rowmode) :uchar (lp lprec) (enable :uchar))
(defcfun ("is_add_rowmode" lps-is-add-rowmode) :uchar (lp lprec))
(defcfun ("get_verbose" lps-get-verbose) :int (lp lprec))
(defcfun ("set_verbose" lps-set-verbose) :void (lp lprec) (level :int))
(defcfun ("is_maxim" lps-is-maxim) :uchar (lp lprec))
(defcfun ("set_maxim" lps-set-maxim) :void (lp lprec))
(defcfun ("set_minim" lps-set-minim) :void (lp lprec))
(defcfun ("get_Ncolumns" lps-get-ncolumns) :int (lp lprec))
(defcfun ("get_Nrows" lps-get-nrows) :int (lp lprec))
(defcfun ("set_obj_fnex" lps-set-obj-fnex) :uchar (lp lprec) (count :int) (coeffs :pointer) (places :pointer))
(defcfun ("get_rowex" lps-get-rowex) :int (lp lprec) (row :int) (cbuf (:pointer :double)) (pbuf (:pointer :int)))
(defcfun ("set_rowex" lps-set-rowex) :uchar (lp lprec) (row :int) (count :int) (coeffs (:pointer :double)) (places (:pointer :int)))
(defcfun ("get_rh" lps-get-rh) :double (lp lprec) (row :int))
(defcfun ("set_rh" lps-set-rh) :uchar (lp lprec) (row :int) (val :double))
(defcfun ("get_constr_type" lps-get-constr-type) :int (lp lprec) (row :int))
(defcfun ("set_constr_type" lps-set-constr-type) :uchar (lp lprec) (row :int) (type :int))
(defcfun ("is_int" lps-is-int) :uchar (lp lprec) (column :int))
(defcfun ("set_int" lps-set-int) :uchar (lp lprec) (column :int) (flag :uchar))
(defcfun ("solve" lps-solve) :int (lp lprec))
(defcfun ("get_objective" lps-get-objective) :double (lp lprec))
(defcfun ("get_variables" lps-get-variables) :uchar (lp lprec) (buffer (:pointer :double)))
(defcfun ("add_SOS" lps-add-sos) :uchar (lp lprec) (ignored :pointer) (type :int) (priority :int) (count :int) (vars (:pointer :int)) (weights (:pointer :double)))
(defcfun ("add_constraintex" lps-add-constraintex) :uchar (lp lprec) (count :int) (coeffs (:pointer :double)) (places (:pointer :int)) (type :int) (rhs :double))
(defcfun ("get_status" lps-get-status) :int (lp lprec))
(defcfun ("get_statustext" lps-get-statustext) :string (lp lprec) (code :int))

(define-condition solver-error (error)
  ((operation
     :type t :initarg :operation :initform nil
     :reader solver-error-operation)
   (status 
     :type t :initarg :status :initform nil
     :reader solver-error-status)
   (message
     :type (or null string) :initarg :message :initform nil
     :reader solver-error-message))
  (:report (lambda (condition stream)
             (format stream "LP solve operation ~S failed~@[ with status ~S~]~@[: ~A~]"
                     (solver-error-operation condition)
                     (solver-error-status condition)
                     (solver-error-message condition)))))

(defun solver-error (operation status &optional (control nil) &rest params)
  (if (null control)
      (error 'solver-error :operation operation :status status)
      (error 'solver-error :operation operation :status status (apply #'format nil control params))))


(deftype row-index ()
  '(integer 0 #x7fffffff))

(deftype column-index ()
  '(integer 0 #x7fffffff))


(defstruct (lp (:copier nil)
               (:constructor create-lp-wrapper (pointer)))
  (pointer nil)
  (capacity 0 :type (unsigned-byte 32))
  (cbuffer nil)
  (pbuffer nil))


(declaim (inline lpp))

(defun lpp (lp)
  (let ((ptr (lp-pointer lp)))
    (if (not ptr)
        (error "attempt to use dead LP instance")
        ptr)))

(defun lp-solver-error (lp operation)
  (let ((status (lps-get-status (lpp lp))))
    (solver-error operation status)))


(defmethod print-object ((object lp) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((ptr (lp-pointer object)))
      (if (not ptr)
          (write-string "(dead)" stream)
          (format stream "~Dx~D" (lps-get-nrows ptr) (lps-get-ncolumns ptr))))))


(defun call-with-copy-buffers (function lp size)
  (let ((capacity (lp-capacity lp)))
    (when (< capacity size)
      (let ((cbuf (foreign-alloc :double :count size)))
        (unwind-protect (let ((pbuf (foreign-alloc :int :count size)))
                          (unwind-protect (progn
                                            (rotatef (lp-cbuffer lp) cbuf)
                                            (rotatef (lp-pbuffer lp) pbuf)
                                            (setf (lp-capacity lp) size))
                            (when pbuf (foreign-free pbuf))))
          (when cbuf (foreign-free cbuf)))))
    (funcall function (lp-cbuffer lp) (lp-pbuffer lp))))


(defmacro with-copy-buffers (((cbuf pbuf) lp size) &body body)
  `(call-with-copy-buffers (lambda (,cbuf ,pbuf) ,@body) ,lp ,size))


(defun create-lp (&key (rows 0) (columns 0))
  (check-type rows row-index)
  (check-type columns column-index)
  (let ((pointer (lps-make-lp rows columns)))
    (if (or (not pointer) (null-pointer-p pointer))
        (solver-error `(create-lp :rows ,rows :columns ,columns) nil)
        (create-lp-wrapper pointer))))


(defun destroy-lp (lp)
  (let ((pointer nil) (cbuffer nil) (pbuffer nil))
    (rotatef pointer (lp-pointer lp))
    (rotatef cbuffer (lp-cbuffer lp))
    (rotatef pbuffer (lp-pbuffer lp))
    (setf (lp-capacity lp) 0)
    (when pointer (lps-delete-lp pointer))
    (when cbuffer (foreign-free cbuffer))
    (when pbuffer (foreign-free pbuffer))
    lp))


(defun call-with-lp (function &key (rows 0) (columns 0))
  (let ((lp (create-lp :rows rows :columns columns)))
    (unwind-protect (funcall function lp)
      (destroy-lp lp))))


(defmacro with-lp ((var &rest options) &body body)
  `(call-with-lp (lambda (,var) ,@body) ,@options))



(defun incremental-growth-p (lp)
  (not (zerop (lps-is-add-rowmode (lpp lp)))))

(defun (setf incremental-growth-p) (new-mode lp)
  (lps-set-add-rowmode (lpp lp) (if new-mode 1 0))
  new-mode)


(defun verbosity-level (lp)
  (let ((code (lps-get-verbose (lpp lp))))
    (values (case code
              ((0) :neutral)
              ((1) :critical)
              ((2) :severe)
              ((3) :important)
              ((4) :normal)
              ((5) :detailed)
              ((6) :full)
              (otherwise :unknown))
            code)))

(defun (setf verbosity-level) (new-level lp)
  (lps-set-verbose (lpp lp) 
                   (ecase new-level
                     ((:neutral) 0)
                     ((:critical) 1)
                     ((:severe) 2)
                     ((:important) 3)
                     ((:normal) 4)
                     ((:detailed) 5)
                     ((:full) 6)))
  new-level)


(defun objective-mode (lp)
  (if (not (zerop (lps-is-maxim (lpp lp))))
      :maximize
      :minimize))

(defun (setf objective-mode) (new-mode lp)
  (ecase new-mode
    ((:maximize) (lps-set-maxim (lpp lp)))
    ((:minimize) (lps-set-minim (lpp lp))))
  new-mode)


(defun row-count (lp)
  (lps-get-nrows (lpp lp)))

(defun column-count (lp)
  (lps-get-ncolumns (lpp lp)))



(defun constraint-type->code (type)
  (cond
    ((string= type "<=") 1)
    ((string= type ">=") 2)
    ((string= type "=") 3)
    (t (error 'type-error :datum type :expected-type '(member <= >= =)))))

(defun code->constraint-type (code)
  (case code
    ((1) '<=)
    ((2) '>=)
    ((3) '=)
    (otherwise nil)))


(defun constraint-type (lp row)
  (code->constraint-type (lps-get-constr-type (lpp lp) row)))

(defun (setf constraint-type) (new-type lp row)
  (if (zerop (lps-set-constr-type (lpp lp) row (constraint-type->code new-type)))
      (solver-error `((setf constraint-type) ,new-type ,row) nil)
      new-type))



(defun constraint-parameters (lp row)
  (with-copy-buffers ((coeffs places) lp (1+ (column-count lp)))
    (let ((count (lps-get-rowex (lpp lp) row coeffs places)))
      (if (< count 0)
          (solver-error `(constraint-parameters ,row) nil)
          (let ((buffer (make-array count :element-type 't)))
            (loop
              :for p :upfrom 0 :below count
              :do (setf (aref buffer p)
                        (cons (mem-aref coeffs :double p)
                              (mem-aref places :int p))))
            buffer)))))

(defun (setf constraint-parameters) (new-values lp row)
  (let ((count (length new-values)))
    (with-copy-buffers ((coeffs places) lp count)
      (let ((k 0))
        (map nil (lambda (pair)
                   (setf (mem-aref coeffs :double k) (coerce (car pair) 'double-float))
                   (setf (mem-aref places :int k) (coerce (cdr pair) 'column-index))
                   (incf k))
             new-values)
        (if (zerop (lps-set-rowex (lpp lp) row count coeffs places))
            (solver-error `((setf constraint-parameters) ,new-values ,row) nil)
            new-values)))))


(defun objective-parameters (lp)
  (constraint-parameters lp 0))

(defun (setf objective-parameters) (new-values lp)
  (let ((count (length new-values)))
    (with-copy-buffers ((coeffs places) lp count)
      (let ((k 0))
        (map nil (lambda (pair)
                   (setf (mem-aref coeffs :double k) (coerce (car pair) 'double-float))
                   (setf (mem-aref places :int k) (coerce (cdr pair) 'column-index))
                   (incf k))
             new-values)
        (if (zerop (lps-set-obj-fnex (lpp lp) count coeffs places))
            (solver-error `((setf objective-parameters) ,new-values) nil)
            new-values)))))
  




(defun right-hand-value (lp row)
  (lps-get-rh (lpp lp) row))

(defun (setf right-hand-value) (new-value lp row)
  (if (zerop (lps-set-rh (lpp lp) row (coerce new-value 'double-float)))
      (solver-error `((setf right-hand-value) ,new-value ,row) nil)
      new-value))


(defun integer-variable-p (lp column)
  (not (zerop (lps-is-int (lpp lp) column))))

(defun (setf integer-variable-p) (new-value lp column)
  (if (zerop (lps-set-int (lpp lp) column (if new-value 1 0)))
      (solver-error `((setf integer-variable-p) ,new-value ,column) nil)
      new-value))



(defun solve-program (lp)
  (let ((code (lps-solve (lpp lp))))
    (values (case code
              ((0) :optimal)
              ((1) :suboptimal)
              ((2) :infeasible)
              ((3) :unbounded)
              ((4) :degenerate)
              ((5) :numerical-failure)
              (otherwise :unknown))
            code)))

(defun objective-value (lp)
  (lps-get-objective (lpp lp)))

(defun solution-values (lp)
  (let ((size (column-count lp)))
    (with-copy-buffers ((coeffs places) lp size)
      (declare (ignore places))
      (if (zerop (lps-get-variables (lpp lp) coeffs))
          (solver-error `(solution-values) nil)
          (let ((vector (make-array size :element-type 'double-float)))
            (loop
              :for k :upfrom 0 :below size
              :do (setf (aref vector k) (mem-aref coeffs :double k))
              :finally (return vector)))))))



(defun add-sos-constraint (lp type vars &key (priority 0) (weights nil))
  (let ((count (length vars))
        (wptr (null-pointer)))
    (with-copy-buffers ((coeffs places) lp count)
      (let ((k 0))
        (map nil (lambda (place) (setf (mem-aref places :int k) place) (incf k)) vars))
      (when weights
        (unless (eql count (length weights)) (error "number of weight values must match number of variables"))
        (let ((j 0))
          (map nil (lambda (w) (setf (mem-aref coeffs :double j) w) (incf j)) weights)
          (setf wptr coeffs)))
      (if (zerop (lps-add-sos (lpp lp) (null-pointer) type priority count places wptr))
          (solver-error `(add-sos-constraint ,type ,vars :priority ,priority :weights ,weights) nil)
          lp))))


(defun add-constraint (lp vars type value)
  (let ((count (length vars))
        (type (constraint-type->code type))
        (value (coerce value 'double-float)))
    (with-copy-buffers ((coeffs places) lp count)
      (let ((k 0))
        (map nil (lambda (pair)
                   (setf (mem-aref coeffs :double k) (coerce (car pair) 'double-float))
                   (setf (mem-aref places :int k) (coerce (cdr pair) 'column-index))
                   (incf k))
             vars))
      (if (zerop (lps-add-constraintex (lpp lp) count coeffs places type value))
          (solver-error `(add-constraint ,vars ,type ,value) nil) 
          lp))))
  
