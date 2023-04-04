(import (yuni scheme)
        (yuni io drypack)
        (ribbon util ribcode)
        (ribbon util interp))

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
  (define (report sym enc)
    (let ((tbl (vector-ref enc 4))
          (offs-rosym (vector-ref enc 2))
          (offs-rwsym (vector-ref enc 3)))
      (let ((rosyms (vector-copy tbl offs-rosym offs-rwsym))
            (rwsyms (vector-copy tbl offs-rwsym (vector-length tbl))))
        (write (list 'LIB: sym 'ROSYM: rosyms 'RWSYM: rwsyms)) (newline))))
  (define (testentry e)
    (let ((vmmac (vector-ref e 7))
          (vmseq (vector-ref e 5))
          (libsym (vector-ref e 0)))
      (let* ((encmac (ribcode-encode vmmac))
             (encseq (ribcode-encode vmseq)))
        (vector-set! e 5 encseq)
        (vector-set! e 7 encmac)
        (report libsym encmac)
        (report libsym encseq))))

  (for-each testentry obj)
  (let ((p (open-binary-output-file bootstrapsave)))
   (drypack-put p obj)
   (write (list 'SAVING... source '=> bootstrapsave)) (newline)
   (close-port p)))

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
    (write (list 'LOADING...: source)) (newline)
    (let ((bundle (interp-gen-bundle #f bootstrapfile)))
     (write (list 'COMPILE+WRITE...)) (newline)
     (savedump! bundle)
     (exit 0)))
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
