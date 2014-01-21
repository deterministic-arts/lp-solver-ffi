
(in-package "SYMBOLIC-LP")

(defstruct (constraint (:copier nil)
                       (:constructor %make-constraint (left-side relation right-side))
                       (:predicate constraintp))
  (left-side +zero+ :type linear-sum :read-only t)
  (relation '= :type (member <= = >=) :read-only t)
  (right-side 0d0d0 :type coefficient :read-only t))

(defmethod print-object ((object constraint) stream)
  (print-unreadable-object (object stream :type t)
    (render-sum (constraint-left-side object) :stream stream)
    (format stream " ~A ~F" (constraint-relation object) (constraint-right-side object))))

(defun make-constraint (left relation right)
  (let ((left* ($- left right)))
    (multiple-value-bind (left** right**)
        (let ((next (next-term left*)))
          (cond
            ((null next) (values left* 0.0d0))
            ((constant-term-p left*) (values next (- (term-coefficient left*))))
            (t (values left* 0.0d0))))
      (%make-constraint left** relation right**))))
    
(defun $<= (lhs rhs) (make-constraint lhs '<= rhs))
(defun $= (lhs rhs) (make-constraint lhs '= rhs))
(defun $>= (lhs rhs) (make-constraint lhs '>= rhs))
  

(defmacro with-variable-generators (((&rest bindings) &key key) &body body)
  (let ((generator (gensym))
        (extract (gensym))
        (object (gensym))
        (prefix (gensym))
        (key-getter (gensym)))
    (let ((inner `(labels (,@(when key `((,extract (,object) (funcall ,key-getter ,object))))
                           (,generator (,prefix ,object) (format nil "~A~@[[~A]~]" ,prefix ,(if key `(and ,object (,extract ,object)) object)))
                           ,@(mapcar (lambda (binding)
                                       (cond
                                         ((symbolp binding) `(,binding (&optional ,object) (,generator ,(string-capitalize binding) ,object)))
                                         ((listp binding) `(,(car binding) (&optional ,object) (,generator ,(string (cadr binding)) ,object)))
                                         (t (error "malformed generator binding ~S" binding))))
                               bindings))
                    ,@body)))
      (if (not key) inner
          `(let ((,key-getter ,key))
             ,inner)))))




(defclass variable ()
  ((name
     :type string :initarg :name
     :reader variable-name)
   (program
     :type program :initarg :program
     :reader variable-program)
   (type 
     :type (member :real :integer :binary) :initform :real
     :reader variable-type :accessor vartype-of)
   (exclusion
     :type list :initform nil
     :reader variable-exclusion :accessor exclusion-of)))


(defmethod print-object ((object variable) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A ~A"
            (variable-name object)
            (variable-type object))))


(defclass program ()
  ((varindex
     :type hash-table :initform (make-hash-table :test 'equal)
     :reader program-varindex)
   (variables
     :type list :initform nil
     :reader program-variables :accessor variables-of)
   (constraints
     :type list :initform nil
     :reader program-constraints :accessor constraints-of)
   (objective 
     :type linear-sum :initform +zero+
     :reader program-objective :accessor objective-of)))


(defvar *program*)


(defun intern-variable (name &optional (program *program*) soft)
  (labels ((do-intern (name)
             (let* ((index (program-varindex program))
                    (present (gethash name index)))
               (cond
                 (present present)
                 (soft nil)
                 (t (let ((object (make-instance 'variable :name name :program program)))
                      (setf (gethash name index) object)
                      (push object (variables-of program))
                      object))))))
    (if (typep name 'variable)
        (if (eq program (variable-program name))
            name
            (do-intern (variable-name name)))
        (do-intern name))))


(defun add-constraint (constraint &optional (program *program*))
  (check-type constraint constraint)
  (push constraint (constraints-of program))
  constraint)


(defun declare-variable-type (name type &optional (program *program*))
  (check-type type (member :real :integer :binary))
  (let ((var (intern-variable name program)))
    (unless (or (eq type :real) (null (variable-exclusion var)))
      (error "cannot declare type ~S for variable ~S, since it is member of an exclusion set"
             type (variable-name var)))
    (setf (vartype-of var) type)
    var))


(defun declare-variable-exclusion (name1 name2 &optional (program *program*))
  (let ((var1 (intern-variable name1 program))
        (var2 (intern-variable name2 program)))
    (unless (eq (variable-type var1) :real)
      (error "cannot add ~S to an exclusion set, as its type is ~S, not ~S"
             (variable-name var1) (variable-type var1) :real))
    (unless (eq (variable-type var2) :real)
      (error "cannot add ~S to an exclusion set, as its type is ~S, not ~S"
             (variable-name var2) (variable-type var2) :real))
    (let ((exc1 (variable-exclusion var1))
          (exc2 (variable-exclusion var2))
          (excl (list var1 var2)))
      (cond
        ((and (not exc1) (not exc2))
         (setf (exclusion-of var1) excl)
         (setf (exclusion-of var2) excl)
         var1)
        ((and (equal excl exc1) (equal excl exc2)) var1)
        (exc1 (error "cannot add ~S to more than one exclusion set" (variable-name var1)))
        (t (error "cannot add ~S to more than one exclusion set" (variable-name var2)))))))


(defun minimize-function (objective &optional (program *program*))
  (setf (objective-of program) (negate (term objective))))

(defun maximize-function (objective &optional (program *program*))
  (setf (objective-of program) (term objective)))


(defun solve-program (&optional (program *program*))
  "Solves the linear program `program' (which defaults to the
   value of `*program*'), returning three values:

     1. the value of the objective function
     2. a keyword providing status information
     3. a vector, which represents the solution

   If the program cannot be solved, the function returns 0 as first
   value, and an empty solution vector as third. Inspect the status
   code in order to determine the kind of problem, which caused the
   program to be unsolvable.

   The solution vector contains pairs (name . value), one for each
   variable actually used in the program (i.e., either occuring in
   the objective function or in at least one constraint). This vector
   is always sorted by variable name."
  (let ((slotindex (make-hash-table :test 'equal))
        (slotlist (make-array 16 :adjustable t :fill-pointer 0))
        (constraints (constraints-of program)))
    (labels
        ((slot-of (name)
           (let* ((var (intern-variable name program))
                  (slot (gethash var slotindex)))
             (or slot
                 (let ((exc (variable-exclusion var)))
                   (cond
                     ((null exc)
                      (vector-push-extend (variable-name var) slotlist)
                      (setf (gethash var slotindex) (length slotlist)))
                     ((eq var (car exc))
                      (let ((first (progn 
                                     (vector-push-extend (variable-name var) slotlist)
                                     (setf (gethash var slotindex) (length slotlist)))))
                        (loop
                          :for mv :in (cdr exc)
                          :for cn :upfrom (1+ first)
                          :do (vector-push-extend (variable-name mv) slotlist)
                              (setf (gethash mv slotindex) (length slotlist)))
                        first))
                     (t (slot-of (car exc)) ;; For effect
                        (or (gethash var slotindex)
                            (error "bug in ~S" 'slot-of))))))))
         (collect-sum (sum)
           (let ((result nil))
             (do-terms (cof var) sum
               (when var
                 (push (cons cof (slot-of var)) result)))
             result)))

      ;; Make sure, that we have a slot assigned to all variables actually
      ;; used (and only those)

      (do-terms (cof var) (program-objective program)
        (when (and var (not (zerop cof)))
          (slot-of var)))

      (dolist (cons constraints)
        (do-terms (cof var) (constraint-left-side cons)
          (when (and var (not (zerop cof)))
            (slot-of var))))

      (with-lp (lp :columns (length slotlist) :rows (length constraints))
        (setf (verbosity-level lp) :critical)
        (setf (objective-mode lp) :maximize)
        (setf (objective-parameters lp) (collect-sum (program-objective program)))
        ;; (format t "~&Maximize ~S" (program-objective program))
        (let ((row 1))
          (dolist (cons constraints)
            ;; (format t "~&Adding constraint ~S" cons)
            (setf (constraint-parameters lp row) (collect-sum (constraint-left-side cons)))
            (setf (constraint-type lp row) (constraint-relation cons))
            (setf (right-hand-value lp row) (constraint-right-side cons))
            (incf row)))
        ;; (format t "~&Before declarations: ~S~%" slotlist)
        (maphash (lambda (key value)
                   (let ((exclusion (variable-exclusion key))
                         (type (variable-type key)))
                     (if exclusion
                         (when (eq (car exclusion) key)
                           ;; (format t "~&Declaring exclusive ~S" exclusion)
                           (add-sos-constraint lp 1 (mapcar #'slot-of exclusion) :priority 1))
                         (ecase type
                           ((:real) nil)
                           ((:integer) 
                            ;; (format t "~&Declaring ~S as integer" (variable-name key))
                            (setf (integer-variable-p lp value) t))
                           ((:binary) (error "not yet implemented"))))))
                 slotindex)
        (let ((status (lp-solver-ffi:solve-program lp)))
          (case status
            ((:optimal :suboptimal)
             (values (objective-value lp) status
                     (stable-sort (map 'vector #'cons slotlist (solution-values lp))
                                  #'string< :key #'car)))
            (t (values 0.0d0 status #()))))))))
                     


#-(and)
(let ((*program* (make-instance 'program)))
  (with-variable-generators ((x))
    (let ((x1 (x 1))
          (x2 (x 2)))
      (maximize-function ($+ ($- x1) ($- x2)))
      (add-constraint ($>= x1 1))
      (add-constraint ($>= x2 1))
      (add-constraint ($>= ($+ x1 x2) 2))
      (declare-variable-type x1 :integer)
      (multiple-value-bind (value status variables) (solve-program)
        (format t "~&Value = ~F~%Status = ~A~%Vars = ~S~%"
                value status variables)))))
  
