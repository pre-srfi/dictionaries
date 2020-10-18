(define (register-plist!)
  
  (define (plist?  l)
    (and (list? l)
         (not (null? l))
         (symbol? (car l))))

  (define (plist-map!  proc plist)
    (let loop ((pl plist))
     (cond
       ((null? pl) plist)
       ((null? (cdr pl)) (error "Malformed plist" plist))
       (else 
         (let ((key (car pl))
               (value (cadr pl))
               (rest (cddr pl)))
           (set-car! (cdr pl)
                     (proc key value))
           (loop rest))))))

  (define (plist-filter!  pred plist)
    (define head (cons #f plist))
    (let loop ((pl plist)
               (parent-cell head))
     (cond
       ((null? pl) (cdr head))
       ((null? (cdr pl)) (error "Malformed plist" plist))
       (else 
         (let ((key (car pl))
               (value (cadr pl))
               (rest (cddr pl)))
           (if (pred key value)
               (loop rest
                     (cdr pl))
               (loop (begin
                       (set-cdr! parent-cell rest)
                       rest)
                     parent-cell)))))))

  ;; head is a pair, whose cdr is the plist
  ;; if found, returns a pair, whose cdr is rest of plist, and cadr is key that was searched for
  ;; if not found, returns #f
  ;;
  ;; the pair indirection is used so that calling set-cdr! on the result allows the plist to be mutated
  (define (find-plist-entry key head)
    (define plist (cdr head))
    (cond
      ((null? plist) #f)
      ((equal? key (car plist)) head)
      (else (find-plist-entry key (cdr plist)))))
  
  (define (plist-search!  plist key failure success)
    (define plist-head (cons #t plist))
    (define (handle-success head)
      (define key-cell (cdr head))
      (define val-cell (cddr head))
      (define (update new-key new-value obj)
        (set-car! key-cell new-key)
        (set-car! val-cell new-value)
        (values plist obj))
      (define (remove obj)
        (set-cdr! head (cddr (cdr head)))
        (values (cdr plist-head) obj))
      (success (car key-cell) (car val-cell) update remove))

    (define (handle-failure)
      (define (insert value obj)
        (values (cons key (cons value plist))
                obj))
      (define (ignore obj)
        (values plist obj))
      (failure insert ignore))
    (cond
      ((find-plist-entry key plist-head) => handle-success)
      (else (handle-failure))))

  (define (plist-size  plist)
    (length plist))

  (define (plist-foreach  proc plist)
    (let loop ((pl plist))
     (if (null? pl) #t
         (begin
           (proc (car pl) (cadr pl))
           (loop (cddr pl))))))
  
  (register-dictionary! 
    'dictionary? plist?
    'dict-map! plist-map!
    'dict-filter! plist-filter!
    'dict-search! plist-search!
    'dict-size plist-size
    'dict-for-each plist-foreach))
