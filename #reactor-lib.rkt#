#lang racket
(require reactor)
(require (for-syntax syntax/parse))

(provide (all-from-out reactor)
         (all-defined-out))

(define-syntax-rule (run expr)
  (prime (lambda () expr)))

(define (exec r)
  (if (reactor-done? r)
      (void)
      (begin (react! r)
             (exec r))))

(define-syntax-rule (exec-run expr)
  (let ((r (run expr))) (exec r)))

(define-syntax (let-signal stx)
  (syntax-parse stx
    #:literals (let-signal)
    [(let-signal (s:id ...) body ...+)
     #'(signal (s ...) body ...)]
    [(let-signal ((s:id default:expr
                        (~optional (~seq #:gather f)
                                   #:defaults ([f #'(lambda args
                                                     (error "error!"))])))
                  ...) body ...+)
     #'(signal ((s default #:gather f)
                ...) body ...)]))
