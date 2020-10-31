(include "indexes.scm")
(include "internals.scm")
(include "externals.scm")

;; register
(let ()
 (include "alist-impl.scm")
 (register-alist!))

(let ()
 (include "plist-impl.scm")
 (register-plist!)) 

(cond-expand
  ((library (srfi 126))
   (let ()
    (include "srfi-126-impl.scm")
    (register-srfi-126!)))
  (else))

(cond-expand
  ((library (srfi 125))
   (let ()
    (include "srfi-125-impl.scm")
    (register-srfi-125!)))
  (else))

(cond-expand
  ((and (library (srfi 69))
        (not (library (srfi 125))))
   (let ()
    (include "srfi-69-impl.scm")
    (register-srfi-69!)))
  (else))
