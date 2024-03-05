(import (yuni scheme)
        (yuni io drypack)
        (ribbon util ribcode)
        (ribbon util interp)
        (ribbon util mergebundle))

(define libpath '())

(define YUNIROOT #f)
(define RUNTIMEROOT #f)
(define source #f)
(define bootstrapfile #f)
(define bootstrapsave #f)
(define *command-line* (vector->list ($$command-line 0)))

(define (consume-arguments!)
  (define (fail a)
    (write (list 'ARGERROR: a)) (newline)
    (exit 1))
  (when (pair? *command-line*)
    (let ((a (car *command-line*))
          (d (cdr *command-line*)))
      (cond
        ((string=? "-bootstrapfile" a)
         (unless (pair? d)
           (fail a))
         (set! bootstrapfile (car d))
         (set! *command-line* (cdr d))
         (consume-arguments!))
        ((string=? "-bootstrapsave" a)
         (unless (pair? d)
           (fail a))
         (set! bootstrapsave (car d))
         (set! *command-line* (cdr d))
         (consume-arguments!))
        ((string=? "-libpath" a)
         (unless (pair? d)
           (fail a))
         (set! libpath (append (list (car d)) libpath))
         (set! *command-line* (cdr d))
         (consume-arguments!))
        ((string=? "-yuniroot" a)
         (unless (pair? d)
           (fail a))
         (set! YUNIROOT (car d))
         (set! *command-line* (cdr d))
         (consume-arguments!))
        ((string=? "-runtimeroot" a)
         (unless (pair? d)
           (fail a))
         (set! RUNTIMEROOT (car d))
         (set! *command-line* (cdr d))
         (consume-arguments!))
        (else
          (set! source a)
          (set! *command-line* d))))))

(define (savedump! obj)
  (define (testentry e)
    (let ((vmmac (vector-ref e 7))
          (vmseq (vector-ref e 5))
          (libsym (vector-ref e 0)))
      (let* ((encmac (map ribcode-encode vmmac))
             (encseq (map ribcode-encode vmseq)))
        (vector-set! e 5 encseq)
        (vector-set! e 7 encmac))))

  (write (list 'RIBCODE...)) (newline)
  (for-each testentry obj)
  (let ((p (open-binary-output-file bootstrapsave)))
   (write (list 'DRYPACK...)) (newline)
   (drypack-put p obj)
   (write (list 'SAVING... source '=> bootstrapsave)) (newline)
   (close-port p)))

;; Initialize VM

($vm-exit 3 raise)

;; Initialize runtime environment

(write (list 'ARGS: *command-line*)) (newline)

(consume-arguments!)

(when YUNIROOT
  (set! libpath
    (append (map (lambda (e) (string-append YUNIROOT "/" e))
                 '("external" "lib" "lib-compat" "lib-r7c"))
            libpath)))

(when RUNTIMEROOT
  (set! libpath
    (append (list (string-append RUNTIMEROOT "/runtime")
                  RUNTIMEROOT)
            libpath)))

;; Initialize interpreter

(write (list 'ARGS: *command-line*)) (newline)

(cond
  (bootstrapsave ;; Bootstrap
    (unless bootstrapfile
      (write (list 'NO-BOOTSTRAP-FILE)) (newline)
      (exit 1))
    (interp-reset!/bootstrap)
    (interp-set-libpath! (reverse libpath))
    (interp-activate!)
    (write (list 'LOADING...: bootstrapfile)) (newline)
    (let ((bundle (interp-gen-bundle #f bootstrapfile)))
     (write (list 'MERGE...)) (newline)
     (let ((mb (merge-bootstrap-bundle! bundle)))
       (write (list 'WRITE...)) (newline)
       (savedump! mb))
     (exit 0)
     'FIXME ;; FIXME: Compiler bug?
     ))
  (else ;; Standard boot

    (unless source
      (write (list 'FILE-REQUIRED)) (newline)
      (exit 1))

    ;; Check source
    (unless (file-exists? source)
      (write (list 'FILE-NOT-FOUND: source)) (newline)
      (exit 1))

    (interp-reset!)
    (interp-set-libpath! (reverse libpath))
    (interp-activate!)

    (write (list 'STARTING...: source)) (newline)
    (let ((bundle (interp-gen-bundle #t source)))
     (write (list 'INTERP...)) (newline)
     (interp-run bundle))

    (write (list 'DONE.)) (newline)))
