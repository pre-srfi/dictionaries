(define (make-plist-impl)
  
  (define (plist? vec l)
    (and (list? l)
         (not (null? l))
         (symbol? (car l))))

  (define (plist-map! vec proc plist)
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

  (define (plist-filter! vec pred plist)
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
  
  (define (plist-search! vec plist key failure success)
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

  (define (plist-size vec plist)
    (define keys
      (let loop ((pl plist)
                 (keys '()))
        (if (null? pl)
            keys
            (loop (cddr pl)
                  (cons (car pl) keys)))))
    (define (fold-proc el set)
      (lset-adjoin equal? set el))
    (define key-set (fold fold-proc '() keys))
    (length key-set))

  (define (plist-foreach vec proc plist)
    (let loop ((pl plist))
     (if (null? pl) #t
         (begin
           (proc (car pl) (cadr pl))
           (loop (cddr pl))))))

  (define vec (vector-copy model-vec))
  (vector-set! vec d? plist?)
  (vector-set! vec dmap! plist-map!)
  (vector-set! vec dfilter! plist-filter!)
  (vector-set! vec dsearch! plist-search!)
  (vector-set! vec dsize plist-size)
  (vector-set! vec dfor-each plist-foreach)

  vec)