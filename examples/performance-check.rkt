#lang racket/base

(require arena
         ffi/unsafe
         racket/fixnum
         racket/format)

;; Memory usage reporting function using current-memory-use
(define (print-memory-usage label)
  (collect-garbage)
  (collect-garbage) ; Run twice to ensure full GC
  
  ;; Use current-memory-use as suggested
  (define mem-usage (current-memory-use))
  
  (printf "~a: ~a MB\n" 
          label 
          (~r (/ mem-usage (* 1024 1024)) #:precision 2)))

;; Alternative function that uses vector-set-performance-stats!
(define (print-detailed-memory-stats label)
  (collect-garbage)
  (collect-garbage)
  
  (define stats (make-vector 20 0))
  (vector-set-performance-stats! stats)
  
  (printf "\n--- ~a ---\n" label)
  (printf "Memory in use: ~a MB\n" 
          (~r (/ (vector-ref stats 1) (* 1024 1024)) #:precision 2))
  (printf "GC Memory: ~a MB\n" 
          (~r (/ (vector-ref stats 3) (* 1024 1024)) #:precision 2))
  (printf "Total Memory: ~a MB\n" 
          (~r (/ (+ (vector-ref stats 1) (vector-ref stats 3)) (* 1024 1024)) #:precision 2)))

;; Simulate heavy allocation work with normal Racket allocation
(define (racket-allocation-test iterations obj-size)
  ;; Calculate and display total planned allocation size
  (define total-allocation-size (* iterations obj-size))
  (printf "Total planned Racket allocation: ~a MB (~a iterations × ~a bytes)\n"
          (~r (/ total-allocation-size (* 1024 1024)) #:precision 2)
          iterations
          obj-size)
  
  (print-memory-usage "Before Racket allocation")
  (print-detailed-memory-stats "Before Racket allocation (detailed)")
  
  (define objects (make-vector iterations #f))
  (for ([i (in-range iterations)])
    (vector-set! objects i (make-bytes obj-size))
    
    (when (= (modulo i 1000) 0)
      (print-memory-usage (format "Racket allocation ~a/~a" i iterations))))
  
  (print-memory-usage "After Racket allocation")
  (print-detailed-memory-stats "After Racket allocation (detailed)")
  
  ;; Clear references to allow garbage collection
  (for ([i (in-range iterations)])
    (vector-set! objects i #f))
  
  (print-memory-usage "After clearing Racket objects")
  (print-detailed-memory-stats "After clearing Racket objects (detailed)"))

;; Simulate heavy allocation work with arena allocation
(define (arena-allocation-test iterations obj-size)
  ;; Calculate and display total planned allocation size
  (define total-allocation-size (* iterations obj-size))
  (printf "Total planned Arena allocation: ~a MB (~a iterations × ~a bytes)\n"
          (~r (/ total-allocation-size (* 1024 1024)) #:precision 2)
          iterations
          obj-size)
  
  ;; Create a large arena (adjust size as needed)
  (define arena-size (* iterations obj-size 2)) ;; Double to ensure enough space
  (define test-arena (make-arena arena-size))
  
  (print-memory-usage "Before arena allocation")
  (print-detailed-memory-stats "Before arena allocation (detailed)")
  
  ;; Allocate many objects in the arena
  (for ([i (in-range iterations)])
    (define ptr (test-arena obj-size))
    ;; Write some data to make sure memory is actually used
    (for ([j (in-range (min obj-size 100))])
      (ptr-set! (ptr-add ptr j) _byte (fxmodulo j 256)))
    
    (when (= (modulo i 1000) 0)
      (print-memory-usage (format "Arena allocation ~a/~a" i iterations))))
  
  (print-memory-usage "After arena allocation")
  (print-detailed-memory-stats "After arena allocation (detailed)")
  
  ;; Cleanup
  (destroy-arena test-arena)
  
  (print-memory-usage "After destroying arena")
  (print-detailed-memory-stats "After destroying arena (detailed)"))

;; Test creating and destroying arenas repeatedly to check for memory leaks
(define (arena-create-destroy-test iterations arena-size)
  ;; Calculate and display cumulative arena size
  (define total-arena-size (* iterations arena-size))
  (define total-allocation-size (* iterations 100)) ;; We allocate 100 bytes per arena
  
  (printf "Total cumulative arena capacity: ~a MB (~a iterations × ~a bytes)\n"
          (~r (/ total-arena-size (* 1024 1024)) #:precision 2)
          iterations
          arena-size)
  
  (printf "Total actual allocation in arenas: ~a MB (~a iterations × 100 bytes)\n"
          (~r (/ total-allocation-size (* 1024 1024)) #:precision 2)
          iterations)
  
  (print-memory-usage "Before arena create/destroy test")
  (print-detailed-memory-stats "Before arena create/destroy test (detailed)")
  
  ;; Create and immediately destroy many arenas
  (for ([i (in-range iterations)])
    (define test-arena (make-arena arena-size))
    ;; Allocate at least one object to ensure arena is actually used
    (define ptr (test-arena 100))
    ;; Write some data to make sure memory is actually used
    (ptr-set! ptr _byte 42)
    
    ;; Destroy the arena right away
    (destroy-arena test-arena)
    
    (when (= (modulo i 1000) 0)
      (print-memory-usage (format "Arena create/destroy ~a/~a" i iterations))))
  
  (print-memory-usage "After arena create/destroy test")
  (print-detailed-memory-stats "After arena create/destroy test (detailed)")
  
  ;; Force GC to check if all memory is properly released
  (collect-garbage)
  (collect-garbage)
  
  (print-memory-usage "After final GC")
  (print-detailed-memory-stats "After final GC (detailed)"))

;; Run performance tests
(define (run-performance-tests)
  (printf "===== Memory Performance Tests =====\n")
  
  (define iterations 10000)
  (define obj-size 1024) ;; 1KB objects
  (define arena-size (* 1024 1024)) ;; 1MB arenas for create/destroy test
  
  (printf "\n----- Testing Racket Standard Allocation -----\n")
  (racket-allocation-test iterations obj-size)
  
  (printf "\n----- Testing Arena Allocation -----\n")
  (arena-allocation-test iterations obj-size)
  
  (printf "\n----- Testing Arena Creation/Destruction -----\n")
  (arena-create-destroy-test iterations arena-size)
  
  (printf "\n===== Tests Complete =====\n")
  
  ;; Optional: dump full memory stats at the end
  #;(printf "\n----- Final Memory Statistics -----\n")
  #;(dump-memory-stats))

;; Execute tests
(run-performance-tests)
