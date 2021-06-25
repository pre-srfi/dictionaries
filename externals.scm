(define registry '())

(define (lookup dictionary fail-on-notfound?)
  (let loop ((r registry))
   (cond
     ((null? r) (if fail-on-notfound?
                    (error "Not a recognized dictionary" dictionary)
                    #f))
     ((dcall d? (car r) dictionary) (car r))
     (else (loop (cdr r))))))

(define (make-internal-wrapper name proc)
  (cond
    ((or (equal? name 'dict-set!)
         (equal? name 'dict-adjoin!)
         (equal? name 'dict-delete!))
     (lambda (vec dict objs)
       (apply proc (cons dict objs))))
    (else
      (lambda (vec . args)
        (apply proc args)))))

(define (register-dictionary! . lst)
  (define vec (vector-copy model-vec))
  (do ((lst lst (cddr lst)))
      ((null? lst))
      (when (null? (cdr lst))
        (error "Uneven amount of arguments" lst))
      (let ((proc-name (car lst))
            (proc (cadr lst)))
        (define index
          (cond 
            ((assoc proc-name dname-map) => cdr)
            (else (error "Unrecognized procedure name" proc-name))))
        (unless (procedure? proc)
          (error "Not a procedure" proc))
        (vector-set! vec index (make-internal-wrapper proc-name proc))))
  (let loop ((reg registry))
   (define new-reg (reverse (cons vec (reverse reg))))
   (if (eq? reg registry)
       (set! registry new-reg)
       (loop registry))))


;;; External (exported) procedure definitions
(define-syntax dispatch
  (syntax-rules ()
    ((dispatch index dictionary args ...)
     (let ((vec (lookup dictionary #t)))  ; error if not found
        ((vector-ref vec index) vec dictionary args ...)))))

(define-syntax proc-dispatch
  (syntax-rules ()
    ((dispatch index dictionary args ...)
      (let ((vec (lookup dictionary #t)))  ; error if not found
        ((vector-ref vec index) vec args ...)))))

(define (dictionary? obj)
  (if (lookup obj #f) #t #f))  ; #f if not found

(define (dict-empty? dictionary)
  (dispatch dempty? dictionary))

(define (dict-contains? dictionary key)
  (dispatch dcontains? dictionary key))

(define dict-ref
  (case-lambda
    ((dictionary key)
     (dict-ref dictionary key error values))
    ((dictionary key failure)
     (dict-ref dictionary key failure values))
    ((dictionary key failure success)
     (dict-ref* dictionary key failure success))))

(define (dict-ref* dictionary key failure success)
  (dispatch dref dictionary key failure success))

(define (dict-ref/default dictionary key default)
  (dispatch dref/default dictionary key default))

(define (dict-set! dictionary . objs)
  (dispatch dset! dictionary objs))

(define (dict-adjoin! dictionary . objs)
  (dispatch dadjoin! dictionary objs))

(define (dict-delete! dictionary . keys)
  (dispatch ddelete! dictionary keys))

(define (dict-delete-all! dictionary keylist)
  (dispatch ddelete-all! dictionary keylist))

(define (dict-replace! dictionary key value)
  (dispatch dreplace! dictionary key value))

(define (dict-intern! dictionary key failure)
  (dispatch dintern! dictionary key failure))

(define dict-update!
  (case-lambda
    ((dictionary key updater)
     (dict-update! dictionary key updater error values))
    ((dictionary key updater failure)
     (dict-update! dictionary key updater failure values))
    ((dictionary key updater failure success)
     (dispatch dupdate! dictionary key updater failure success))))

(define (dict-update/default! dictionary key updater default)
  (dispatch dupdate/default! dictionary key updater default))

(define (dict-pop! dictionary)
  (dispatch dpop! dictionary))

(define (dict-map! proc dictionary)
  (proc-dispatch dmap! dictionary proc dictionary))

(define (dict-filter! pred dictionary)
  (proc-dispatch dfilter! dictionary pred dictionary))

(define (dict-remove! pred dictionary)
  (proc-dispatch dremove! dictionary pred dictionary))

(define (dict-search! dictionary key failure success)
  (dispatch dsearch! dictionary key failure success))

(define (dict-size dictionary)
  (dispatch dsize dictionary))

(define (dict-for-each proc dictionary)
  (proc-dispatch dfor-each dictionary proc dictionary))

(define (dict-count pred dictionary)
  (proc-dispatch dcount dictionary pred dictionary))

(define (dict-any pred dictionary)
  (proc-dispatch dany dictionary pred dictionary))

(define (dict-every pred dictionary)
  (proc-dispatch devery dictionary pred dictionary))

(define (dict-keys dictionary)
  (dispatch dkeys dictionary))

(define (dict-values dictionary)
  (dispatch dvalues dictionary))

(define (dict-entries dictionary)
  (dispatch dentries dictionary))

(define (dict-fold proc knil dictionary)
  (proc-dispatch dfold dictionary proc knil dictionary))

(define (dict-map->list proc dictionary)
  (proc-dispatch dmap->list dictionary proc dictionary))

(define (dict->alist dictionary)
  (dispatch d->alist dictionary))
