;;;; Internal procedure definitions (all take a vec argument first)

;;; Sample call of an internal procedure from another internal procedure:
;;; (dcall dref/default vec dictionary key default)

;;; Notes on definitions:
;;; Vec argument is not used except to pass to dcalls
;;; External procedures with a rest argument use a list argument here
;;; External procedures with optional arguments are not optional here
 
(define-syntax dcall
  (syntax-rules ()
    ((dcall dproc vec dictionary arg ...)
     ((vector-ref vec dproc) vec dictionary arg ...))))

(define (idictionary? vec obj)
  (error "dictionary? method not defined"))

(define (idict-empty? vec dictionary)
  (= 0 (dcall dsize vec dictionary)))

(define (idict-contains? vec dictionary key)
  (dcall dref vec dictionary key
         (lambda () #f) (lambda (x) #t)))

(define (idict-ref vec dictionary key failure success)
  (define-values
    (new-dict result)
    (dcall dsearch! vec dictionary key 
           (lambda (_ ignore)
             (ignore (failure)))
           (lambda (key value update _)
             (update key value (success value)))))
  result)

(define (idict-ref/default vec dictionary key default)
  (dcall dref vec dictionary key
         (lambda () default)
         (lambda (x) x)))

;; private
(define (idict-set!* vec dictionary use-old? objs)
  (let loop ((objs objs)
             (dictionary dictionary))
    (cond
      ((null? objs) 
       dictionary)
      ((null? (cdr objs))
       (error "mismatch of key / values argument list" objs))
      (else (let*-values
              (((key) (car objs))
               ((value) (cadr objs))
               ((new-d _) (dcall dsearch! vec dictionary key
                                 (lambda (insert ignore)
                                   (insert value #f))
                                 (lambda (key old-value update delete)
                                   (update key (if use-old? old-value value) #f)))))
              (loop (cddr objs)
                    new-d))))))

(define (idict-set! vec dictionary objs)
  (idict-set!* vec dictionary #f objs))

(define (idict-adjoin! vec dictionary objs)
  (idict-set!* vec dictionary #t objs))

(define (idict-delete! vec dictionary keys)
  (dcall ddelete-all! vec dictionary keys))

(define (idict-delete-all! vec dictionary keylist)
  (let loop ((keylist keylist)
             (dictionary dictionary))
    (cond
      ((null? keylist) dictionary)
      (else (let*-values 
              (((key) (car keylist))
               ((new-d _) (dcall dsearch! vec dictionary key
                                 (lambda (_ ignore) 
                                   (ignore #f))
                                 (lambda (key old-value _ delete)
                                   (delete #f)))))
              (loop (cdr keylist)
                    new-d))))))

(define (idict-replace! vec dictionary key value)
  (define-values
    (new-dict _)
    (dcall dsearch! vec dictionary key
         (lambda (_ ignore)
           (ignore #f))
         (lambda (key old-value update _)
           (update key value #f))))
  new-dict)

(define (idict-intern! vec dictionary key failure)
  (dcall dsearch! vec dictionary key
         (lambda (insert _)
           (let ((value (failure)))
            (insert value value)))
         (lambda (key value update _)
           (update key value value))))

(define (idict-update! vec dictionary key updater failure success)
  (define-values
    (new-dict _)
    (dcall dsearch! vec dictionary key
           (lambda (insert ignore)
             (insert (updater (failure)) #f))
           (lambda (key value update _)
             (update key (updater (success value)) #f))))
  new-dict)

(define (idict-update/default! vec dictionary key updater default)
  (dcall dupdate! vec dictionary key updater
         (lambda () default)
         (lambda (x) x)))

(define (idict-pop! vec dictionary)
  (define (do-pop)
    (call/cc
      (lambda (cont)
        (dcall dfor-each vec
               (lambda (key value)
                 (define new-dict
                   (dcall ddelete! vec dictionary (list key)))
                 (cont new-dict key value)) 
               dictionary))))
  (define empty? (dcall dempty? vec dictionary))
  (if empty?
      (error "popped empty dictionary")
      (do-pop)))

(define (idict-map! vec proc dictionary)
  (error "dict-map method not defined"))

(define (idict-filter! vec pred dictionary)  
  (error "dict-filter! method not defined"))

(define (idict-remove! vec pred dictionary)
  (dcall dfilter! vec (lambda (key value) (not (pred key value))) dictionary))

(define (idict-search! vec dictionary key failure success)
  (error "dict-search! method not defined"))

(define (idict-size vec dictionary)
  (error "dict-size method not defined"))

(define (idict-for-each vec proc dictionary)
  (error "dict-for-each method not defined"))

(define (idict-count vec pred dictionary)
  (dcall dfold vec
         (lambda (key value acc)
           (if (pred key value)
               (+ 1 acc)
               acc))
         0
         dictionary))

(define (idict-any vec pred dictionary)
  (call/cc
    (lambda (cont)
      (dcall dfor-each vec
             (lambda (key value)
               (define ret (pred key value))
               (when ret
                 (cont ret)))
             dictionary)
      #f)))

(define (idict-every vec pred dictionary)
  (define last #t)
  (call/cc
    (lambda (cont)
      (dcall dfor-each vec
             (lambda (key value)
               (define ret (pred key value))
               (when (not ret)
                 (cont #f))
               (set! last ret))
             dictionary)
      last)))

(define (idict-keys vec dictionary)
  (reverse
    (dcall dfold vec
         (lambda (key value acc)
           (cons key acc))
         '()
         dictionary)))

(define (idict-values vec dictionary)
  (reverse
    (dcall dfold vec
         (lambda (key value acc)
           (cons value acc))
         '()
         dictionary)))

(define (idict-entries vec dictionary)
  (define pair 
    (dcall dfold vec
           (lambda (key value acc)
             (cons (cons key (car acc))
                   (cons value (cdr acc))))
           (cons '() '())
           dictionary))
  (values (reverse (car pair)) 
          (reverse (cdr pair))))

(define (idict-fold vec proc knil dictionary)
  (define acc knil)
  (dcall dfor-each vec
         (lambda (key value)
           (set! acc (proc key value acc)))
         dictionary)
  acc)

(define (idict-map->list vec proc dictionary)
  (define reverse-lst
    (dcall dfold vec
         (lambda (key value lst)
           (cons (proc key value) lst)) 
         '()
         dictionary))
  (reverse reverse-lst))

(define (idict->alist vec dictionary)
  (dcall dmap->list vec
         cons
         dictionary))

(define model-vec 
  (vector
    idictionary?  idict-empty?  idict-contains?  idict-ref
    idict-ref/default idict-set!  idict-adjoin!  idict-delete!
    idict-delete-all!  idict-replace!  idict-intern!
    idict-update! idict-update/default! idict-pop!  idict-map!
    idict-filter!  idict-remove!  idict-search!  idict-size 
    idict-for-each idict-count idict-any idict-every idict-keys
    idict-values idict-entries idict-fold idict-map->list
    idict->alist))
