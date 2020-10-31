(define (register-alist!)
  
  (define (alist? l)
    (and (list? l)
         (or (null? l)
             (pair? (car l)))))

  (define (alist-map! proc alist)
    (map 
      (lambda (e)
        (define key (car e))
        (define value (cdr e))
        (cons key (proc key value))) 
      alist))

  (define (alist-filter! pred alist)
    (filter
      (lambda (e)
        (pred (car e) (cdr e))) 
      alist))
  
  (define (alist-delete key alist)
    ;; find the tail of alist that will be kept
    ;; ie rest entries after the last entry with matched key
    (define kept-tail
      (let loop ((tail alist)
                 (lst alist))
      (cond
        ((null? lst) tail)
        (else 
          (if (equal? key (caar lst))
              (loop (cdr lst) (cdr lst))
              (loop tail (cdr lst)))))))
    ;; if tail == alist; just return,
    ;; else filter elements before the tail, and append the tail
    (if (eq? alist kept-tail)
        alist
        (let loop ((lst alist)
                   (result/reversed '()))
          (if (eq? lst kept-tail)
              (append (reverse result/reversed) kept-tail)
              (let* ((entry (car lst))
                     (keep? (not (equal? key (car entry))))
                     (result/reversed* (if keep? 
                                           (cons entry result/reversed)
                                           result/reversed)))
                (loop (cdr lst) result/reversed*))))))

  (define (alist-search! alist key failure success)
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

  (define (alist-size alist)
    (length alist))

  (define (alist-foreach proc alist)
    (define (proc* e)
      (proc (car e) (cdr e)))
    (for-each proc* alist))
  
  (define (alist->alist alist)
    alist)

  (register-dictionary! 
    'dictionary? alist?
    'dict-map! alist-map!
    'dict-filter! alist-filter!
    'dict-search! alist-search!
    'dict-size alist-size
    'dict-for-each alist-foreach
    'dict->alist alist->alist))
