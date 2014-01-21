
(in-package "SYMBOLIC-LP")



;;;;
;;; Coefficient definition and type cast 

(deftype coefficient ()
  'double-float)

(defun coefficient (value)
  (coerce value 'double-float))



;;;;
;;; Ordering on variable names
;;;
;;; Variables are represented by their names, i.e., as strings.
;;; Here, we define an ordering over these names. It's basically
;;; string<, but with a special case, since we need to be able
;;; to represent constant terms (i.e., without variables), which
;;; we do by using nil as the variable name.

(defun variable< (v1 v2)
  "True, if the variable name V1 is `less-than' the name V2."
  (if (not v1) (and v2 t) (and v2 (string< v1 v2))))

(defun variable= (v1 v2)
  "True, if variable names V1 and V2 are equal."
  (if (not v1) (not v2) (and v2 (string= v1 v2))))

(defun compare-variables (v1 v2)
  "Answers an integer, whose value represents the ordering relationship
   between variable names V1 and V2. The value will be < 0, if V1 is 
   less than V2, = 0, if both names are equal, and > 0, if V1 is greater
   than V2."
  (cond
    ((not v1) (if (not v2) 0 -1))
    ((not v2) 1)
    ((string< v1 v2) -1)
    ((string> v1 v2) 1)
    (t 0)))


;;;;
;;; Linear sum
;;;
;;; A linear sum is basically a list of pairs of coefficient
;;; value and variable name. We keep the links in such a list
;;; ordered by their variable names (and allow at most one link
;;; per given variable name in a list).
;;;
;;; Constant terms (i.e., without variables) are represented 
;;; using nil as variable name.


(defstruct (linear-sum (:copier nil)
                       (:predicate linear-sum-p)
                       (:constructor %make-linear-sum (term-coefficient term-variable next-term))
                       (:conc-name nil))
  (term-coefficient 0.0d0 :type coefficient :read-only t)
  (term-variable nil :type (or null string) :read-only t)
  (next-term nil :type (or null linear-sum) :read-only t))
  
(defparameter +zero+ (%make-linear-sum 0.0d0 nil nil)
  "The canonical constant sum with value 0.")

(defparameter +one+ (%make-linear-sum 1.0d0 nil nil)
  "The canonical constant sum with value 1.")


(defun make-linear-sum (coefficient &optional variable next)
  "Construct a new linear sum, whose term coefficient is COEFFICIENT,
   and whose variable is VARIABLE. The next link of the result will 
   point to NEXT. If the coefficient is zero, this function may decide
   not to return a new link, but directly return NEXT or choose some
   other appropriate link. Note, that this function will never return
   a link, whose variable is non-null but whose coefficient is zero."
  (if (zerop coefficient)
      (if (null next) +zero+ next)
      (%make-linear-sum (coefficient coefficient) variable next)))


(defun final-term-p (value)
  "True, if VALUE has no successor term."
  (null (next-term value)))

(defun constant-term-p (value)
  "True, if VALUE is a constant TERM, i.e., has no variable."
  (null (term-variable value)))

(defun constant-sum-p (value)
  "True, if VALUE is a sum with a constant value."
  (and (null (term-variable value)) (null (next-term value))))

(defun zero-sum-p (value)
  "True, if VALUE is the sum with the constant value of 0."
  (and (zerop (term-coefficient value)) (null (next-term value))))


(defun render-term (coefficient variable 
                    &key (stream *standard-output*) (first nil)
                         (force nil))
  "Generate a textual representation of the term formed by COEFFICIENT
   and VARIABLE, writing it into STREAM. If FIRST is true, this function
   assumes, that the term is the first term of a proper sum, otherwise,
   it assumes, that the term is the successor of some other term. This
   affects the output format. Usually, this function does not generate
   anything, if the coefficient is zero. By passing FORCE with a value
   of true, the caller can force the function to generate output anyway.
   This function returns true, if it did generate output, and false,
   if it ignored the call due to the coefficient being zero."
  (if (and (not force) (zerop coefficient))
      nil
      (let* ((spacing (if first "" " "))
             (sign (if (minusp coefficient) "-" (if first "" "+")))
             (value (abs coefficient))
             (show-coeff (or (not variable) (/= 1.0d0 value))))
        (if show-coeff
            (format stream "~A~A~A~F~@[ ~A~]" spacing sign spacing value variable)
            (format stream "~A~A~A~@[~A~]" spacing sign spacing variable))
        t)))


(defun render-sum (object &key (stream *standard-output*))
  "Renders a textual representation of the entire linear sum passed
   as OBJECT into STREAM. The result is optimized for readability, and
   not meant for automatic processing."
  (loop
    :with first := t 
    :for term := object :then (next-term term) :while term
    :do (let ((var (term-variable term))
              (cof (term-coefficient term)))
          (setf first (and (not (render-term cof var :stream stream :first first)) first)))
    :finally (when first (render-term 0.0d0 nil :stream stream :first first :force t)))
  object)


(defmethod print-object ((object linear-sum) stream)
  (print-unreadable-object (object stream :type t)
    (render-sum object :stream stream)))



;;;;
;;; Term designator
;;;
;;; A term designator is anything, from which we can construct new
;;; terms.


(deftype term-designator ()
  '(or linear-sum string real))


(defun term (value)
  "Coerce VALUE into a term. If it is already a term, the value is
   returned unchanged. The value supplied must be a term-designator."
  (cond
    ((stringp value) (make-linear-sum 1.0d0 value))
    ((realp value) (make-linear-sum value))
    ((linear-sum-p value) value)
    (t (error 'type-error :datum value :expected-type 'term-designator))))


(defun map-over-terms (function sum)
  (loop
    :for term := (term sum) :then (next-term term) :while term
    :do (funcall function (term-coefficient term) (term-variable term))
    :finally (return sum)))


(defmacro do-terms ((cof var) sum &body body)
  `(block nil
     (map-over-terms (lambda (,cof ,var) ,@body) ,sum)))



;;;;
;;; Addition and subtract of terms
;;;

(defun negate (object)
  "Answers the result of (- `object'), i.e., the negation
   of the given sum (or term designator). This function always returns
   a linear sum."
  (cond
    ((stringp object) (%make-linear-sum -1.0d0 object nil))
    ((realp object) (%make-linear-sum (- (coefficient object)) nil nil))
    ((linear-sum-p object)
     (labels
         ((iterate (link)
            (if (null link) nil
                (make-linear-sum (- (term-coefficient link))
                                 (term-variable link)
                                 (iterate (next-term link))))))
       (iterate object)))
    (t (error 'type-error :datum object :expected-type 'term-designator))))


(defun merge-sums (s1 s2 sign)
  "This function is the heart of the addition and subtraction code. It
   merges the linear sums S1 and S2, applying the following formula to 
   each matching term: new-coefficient := coefficient-1 + (sign * coefficient-2)."
  (labels
      ((copy-1 (term next)
         (if (eq next (next-term term)) term
             (make-linear-sum (term-coefficient term) (term-variable term) 
                              next)))
       (copy-2 (term)
         (when term
           (make-linear-sum (* sign (term-coefficient term)) (term-variable term) (copy-2 (next-term term)))))
       (copy-2* (term next)
         (make-linear-sum (* sign (term-coefficient term)) (term-variable term) next))
       (iterate (s1 s2)
         (cond
           ((not s1) (if (= sign 1.0d0) s2 (copy-2 s2)))
           ((not s2) s1)
           (t (let* ((v1 (term-variable s1))
                     (v2 (term-variable s2))
                     (dd (compare-variables v1 v2))
                     (c1 (term-coefficient s1))
                     (c2 (term-coefficient s2))
                     (n1 (next-term s1))
                     (n2 (next-term s2)))
                (cond
                  ((< dd 0) (copy-1 s1 (iterate n1 s2)))
                  ((< 0 dd) (copy-2* s2 (iterate s1 n2)))
                  (t (make-linear-sum (+ c1 (* sign c2)) v1 (iterate n1 n2)))))))))
    (iterate s1 s2)))



(defun $sum (list) 
  (reduce (lambda (s1 s2) (merge-sums (term s1) (term s2) 1.0d0)) list 
          :initial-value +zero+))

(defun $difference (first list) 
  (if (null list) (negate first)
      (reduce (lambda (s1 s2) (merge-sums s1 (term s2) -1.0d0)) list
              :initial-value (term first))))

(defun $+ (&rest forms)
  ($sum forms))

(defun $- (first &rest forms) 
  ($difference first forms))


(defun multiply-2 (first second)
  (labels
      ((scale* (factor term)
         (and term
              (make-linear-sum (* factor (term-coefficient term)) (term-variable term)
                               (scale* factor (next-term term)))))
       (scale (factor term)
         (if (zerop factor) +zero+
             (scale* (coefficient factor) term)))
       (bad-argument (value)
         (error 'type-error :datum value :expected-type 'term-designator))
       (non-linear ()
         (error "multiplication of ~S and ~S would introduce non-linear term" first second)))
    (cond
      ((realp first)
       (cond 
         ((realp second) (make-linear-sum (* (coefficient first) (coefficient second))))
         ((stringp second) (make-linear-sum (coefficient first) second))
         ((linear-sum-p second) (scale first second))
         (t (bad-argument second))))
      ((stringp first)
       (cond
         ((realp second) (make-linear-sum (coefficient second) first))
         ((stringp second) (non-linear))
         ((linear-sum-p second)
          (if (constant-sum-p second)
              (make-linear-sum (term-coefficient second) first)
              (non-linear)))
         (t (bad-argument second))))
      ((linear-sum-p first)
       (cond
         ((realp second) (scale second first))
         ((stringp second) 
          (if (constant-sum-p first)
              (make-linear-sum (term-coefficient first) second)
              (non-linear)))
         ((linear-sum-p second)
          (cond
            ((constant-sum-p first) (scale (term-coefficient first) second))
            ((constant-sum-p second) (scale (term-coefficient second) first))
            (t (non-linear))))))
      (t (bad-argument first)))))


(defun $product (list)
  (reduce #'multiply-2 list 
          :initial-value +one+))

(defun $* (&rest forms)
  ($product forms))

