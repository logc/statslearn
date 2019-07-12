#lang racket/base

(module+ test
  (require rackunit))

(require (only-in racket/vector
                  vector-drop
                  vector-map)
         (only-in racket/list
                  empty?))

(struct input-variable (observed-instances))

(define (make-input-variable observed-instances)
  (cond
    [(and (vector? observed-instances)
          (scalars? observed-instances))
     (input-variable observed-instances)]
    [(and (vector? observed-instances)
          (vectors-equal-length? observed-instances))
     (input-variable observed-instances)]
    [else
     (raise-argument-error 'make-input-variable "vector?" observed-instances)]))

(module+ test
  (test-case "Input variables with scalar instances"
    (check-not-exn (lambda () (make-input-variable (vector 2 2 2))))
    (check-exn exn:fail:contract? (lambda () (make-input-variable 'whatever))))
  (test-case "Input variables with vector instances"
    (let ([x_1 (vector 1 2)]
          [x_2 (vector 2 3)])
      (let ([*X* (vector x_1 x_2)])
        (check-not-exn (lambda () (make-input-variable *X*)))))
    (let ([x_1 (vector 1 2 3)]
          [x_2 (vector 2 7)])
      (let ([*X* (vector x_1 x_2)])
        (check-exn exn:fail:contract? (lambda () (make-input-variable *X*)))))))

(define (input-ref X j)
  (vector-ref (input-variable-observed-instances X) j))

(module+ test
  (test-case "Indexing input variables"
    (let ([X (make-input-variable (vector 1 2 3))])
      (check-eq? (input-ref X 1) 2))))

(struct quantitative-output (values))

(define (make-quantitative-output values)
  (cond
    [(and (vector? values) (scalars? values)) (quantitative-output values)]
    [else (raise-argument-error 'make-quantitative-output "vector?" values)]))

(module+ test
  (test-case "Quantitative output"
    (check-not-exn (lambda () (make-quantitative-output (vector 1.1 2.2 3.1))))
    (check-exn exn:fail:contract? (lambda () (make-quantitative-output 'whatever)))))

(struct qualitative-output (values group))

(define (make-qualitative-output values group)
  (cond
    [(all-values-in-group? values group) (qualitative-output values group)]
    [else (raise-argument-error 'make-qualitative-output "all-values-in-group?" values group)]))

(module+ test
  (check-not-exn (lambda () (make-qualitative-output '(a b c) '(a b)))))

(define (vectors-equal-length? vs)
  (let ([lengths (vector-map vector-length vs)])
    (let ([first-length (vector-ref lengths 0)])
      (for/and ([v vs]) (= first-length (vector-length v))))))

(module+ test
  (test-case "Vectors of equal length"
    (let ([vs (vector (vector 1 2) (vector 2))])
      (check-false (vectors-equal-length? vs)))))

(define (scalars? ns)
  (for/and ([n ns]) (number? n)))

(module+ test
  (test-case "Check whether all items in a vector are scalar"
    (check-true (scalars? (vector 1 2 3)))
    (check-false (scalars? (vector (vector 1 1) 1)))))

(define (all-values-in-group? values group)
  (for/and ([v values]) (not (empty? (member v group)))))
