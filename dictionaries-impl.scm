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
  ((or srfi-69 srfi-125 chibi kawa)
   (begin
     (let ()
      (include "srfi-69-impl.scm")
      (register-srfi-69!))))
  (else))
