(define (make-alist-impl)
  
  (define (alist? vec l)
    (and (list? l) (every pair? l)))

  (define (alist-map! vec proc alist)
    (map 
      (lambda (e)
        (define key (car e))
        (define value (cdr e))
        (cons key (proc key value))) 
      alist))

  (define (alist-filter! vec pred alist)
    (filter
      (lambda (e)
        (pred (car e) (cdr e))) 
      alist))

  (define (alist-search! vec alist key failure success)
    (define (handle-success pair)
      (define old-key (car pair))
      (define old-value (cdr pair))
      (define (update new-key new-value obj)
        (cond 
          ((and (eq? old-key
                     new-key)
                (eq? old-value
                     new-value))
           (values alist obj))
          (else
            (let ((new-list 
                    (alist-cons
                      new-key new-value
                      (alist-delete old-key alist))))
              (values new-list obj)))))
      (define (remove obj)
        (values (alist-delete old-key alist) obj))
      (success old-key old-value update remove))

    (define (handle-failure)
      (define (insert value obj)
        (values (alist-cons key value alist)
                obj))
      (define (ignore obj)
        (values alist obj))
      (failure insert ignore))
    (cond
      ((assoc key alist) => handle-success)
      (else (handle-failure))))

  (define (alist-size vec alist)
    (define keys (map car alist))
    (define (fold-proc el set)
      (lset-adjoin equal? set el))
    (define key-set (fold fold-proc '() keys))
    (length key-set))

  (define (alist-foreach vec proc alist)
    (define (proc* e)
      (proc (car e) (cdr e)))
    (for-each proc* alist))
  
  (define (alist->alist vec alist)
    alist)


  (define vec (vector-copy model-vec))
  (vector-set! vec d? alist?)
  (vector-set! vec dmap! alist-map!)
  (vector-set! vec dfilter! alist-filter!)
  (vector-set! vec dsearch! alist-search!)
  (vector-set! vec dsize alist-size)
  (vector-set! vec dfor-each alist-foreach)
  (vector-set! vec d->alist alist->alist)

  vec)
