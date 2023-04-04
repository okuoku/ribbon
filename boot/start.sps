(import (yuni scheme)
        (yuni io drypack)
        (ribbon util interp))

(define libpath '())

(define YUNIROOT #f)
(define RUNTIMEROOT #f)
(define source #f)
(define *command-line* (vector->list ($$command-line 0)))

(define (consume-arguments!)
  (define (fail a)
    (write (list 'ARGERROR: a)) (newline)
    (exit 1))
  (when (pair? *command-line*)
    (let ((a (car *command-line*))
          (d (cdr *command-line*)))
      (cond
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

(write (list 'ARGS: *command-line*)) (newline)

(consume-arguments!)

(unless source
  (write (list 'FILE-REQUIRED)) (newline)
  (exit 1))

;; Check source
(unless (file-exists? source)
  (write (list 'FILE-NOT-FOUND: source)) (newline)
  (exit 1))

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

(interp-reset!)
(interp-set-libpath! (reverse libpath))
(interp-activate!)

(write (list 'STARTING...: source)) (newline)

#|
(let ((bundle (interp-gen-expanded source)))
 (write (list 'DUMP...)) (newline)
 (let ((p (open-binary-output-file "dump.bin")))
  (for-each (lambda (v)
              (cond
                ((equal? (vector-ref v 0) '(yunitest mini))
                 (write (list 'DUMP: v)) (newline)
                 (drypack-put p v))
                (else
                  (write (list 'SKIP: v)) (newline))))
            bundle)
  (close-port p)))
|#

(let ((bundle (interp-gen-bundle source)))
 (write (list 'INTERP...)) (newline)
 (interp-run bundle))

(write (list 'DONE.)) (newline)
