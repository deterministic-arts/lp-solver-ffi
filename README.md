lp-solver-ffi
=============

This system provides rudimentary bindings to the LP Solve library
for dealing with linear programs and MIPs. It uses CFFI. Note, that
we expose only a tiny subset of the available API right now (i.e.,
the stuff I needed in recent projects). Also note, that the current
implementation is not really efficient (much data is copied over
and over when we set matrices).

Package LP-SOLVER-FFI
---------------------

The actual FFI bindings. These have been written against the LPSOLVE 5.5
API and tested under SBCL. Please note, that we do not expose the 
I/O stuff at all, since it's not thread-safe.

Package SYMBOLIC-LP
-------------------

Convenience stuff, which allows to construct linear programs using
symbolic notation, e.g.,

   ($<= ($+ ($* 3 "x1") ($* -1 "x2")) 17)

to create constraints, etc. This is all currently underdocumented,
and pretty much "optimised" to match my style and requirements in 
recent projects, so this may or may not be useful to anyone but me.

Example:

    ;; Minimize x1 + x2
    ;; Requiring that
    ;;   x1 >= 1
    ;;   x2 >= 1
    ;;   x1 + x2 >= 2
    ;; and x1 being integer

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

Variables are simply strings (and the "with-variable-generators" macro
basically expands into a "labels" construct, which defines function, that
return strings). As of now, all numeric values are coerced to double-float
since that's what the LP Solver library needs anyway.