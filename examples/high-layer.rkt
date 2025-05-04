#lang racket/base

(require arena
         ffi/unsafe)

;; Create a 10MB arena
(define my-arena (make-arena (* 10 1024 1024)))

(printf "=== High-level API example ===\n")

;; ======== Primitive Values ========
;; Individual primitive values
(define age (arena-int my-arena 42))
(printf "Age: ~a\n" (age))
(printf "Age pointer: ~a\n" (age 'ptr))

(define pi-value (arena-double my-arena 3.14159))
(printf "Pi: ~a\n" (pi-value))

;; ======== String Example ========
(define greeting (arena-string my-arena "Hello, Arena Allocator!"))
(printf "Greeting: ~a\n" (greeting))  ;; Normal usage
(printf "Raw pointer: ~a\n" (greeting 'ptr))  ;; Get pointer when needed

;; ======== Array Examples ========
;; Integer array example
(define squares (arena-int-array my-arena '(0 1 4 9 16 25 36 49 64 81)))
(printf "Squares length: ~a\n" (squares 'len))  ;; Get length

(printf "Squares: ")
(for ([i (in-range (squares 'len))])
  (printf "~a " (squares i)))  ;; Access elements by index
(printf "\n")

;; Double array example
(define temperatures (arena-double-array my-arena '(98.6 37.0 22.5 -5.0)))
(printf "Temperatures: ")
(for ([i (in-range (temperatures 'len))])
  (printf "~a " (temperatures i)))
(printf "\n")

;; ======== Vector Example ========
(define fruits (arena-vector my-arena '("apple" "banana" "cherry" "date" "elderberry")))
(printf "Number of fruits: ~a\n" (fruits 'len))

(printf "Fruits: ")
(for ([i (in-range (fruits 'len))])
  (printf "~a " (fruits i)))
(printf "\n")

;; ======== C Struct Examples ========
;; Define a C struct type for a 2D point
(define-cstruct _point ([x _double]
                         [y _double]))

;; Create a point struct in the arena with initial values and explicit field types
(define point1 (arena-cstruct my-arena _point (list _double _double) (list 10.5 20.3)))


;; Access fields
(printf "Point coordinates: (~a, ~a)\n" 
        (point1 0)  ;; x value (index 0)
        (point1 1)) ;; y value (index 1)

;; Create struct array with explicit field types
(define points (arena-cstruct-array my-arena _point (list _double _double) 3
                 (lambda (i) 
                   (list (* i 10.1) (* i 20.2)))))

(printf "Points array:\n")
(for ([i (in-range (points 'len))])
  (define point (points i))
  (printf "  Point ~a: (~a, ~a)\n" 
          i (point 0) (point 1)))

;; Cleanup
(destroy-arena my-arena)

;; ======== Update Value Examples ========
(printf "\n=== Value modification examples ===\n")

;; Create a new arena for our examples
(define update-arena (make-arena (* 1 1024 1024)))

;; Primitive value updates
(define score (arena-int update-arena 75))
(printf "Original score: ~a\n" (score))
(set-arena-int! score 95)
(printf "Updated score: ~a\n" (score))

;; Array element updates
(define data (arena-double-array update-arena '(1.1 2.2 3.3 4.4)))
(printf "Original array: ")
(for ([i (in-range (data 'len))])
  (printf "~a " (data i)))
(printf "\n")

(set-arena-double-array! data 1 2.5)
(set-arena-double-array! data 3 4.9)
(printf "Updated array: ")
(for ([i (in-range (data 'len))])
  (printf "~a " (data i)))
(printf "\n")

;; String updates
(define message (arena-string update-arena "Hello world"))
(printf "Original message: ~a\n" (message))
(set-arena-string! message "Hi there!")  ;; Same length
(printf "Updated message: ~a\n" (message))

;; Struct field updates
(define-cstruct _staff_record ([id _int]
                          [salary _double]))

(define staff1 (arena-cstruct update-arena _staff_record 
                               (list _int _double) 
                               (list 101 50000.0)))

(printf "Original staff record: ID=~a, Salary=~a\n" 
        (staff1 0) (staff1 1))

(set-arena-struct-field! staff1 1 55000.0)  ;; Give a raise
(printf "After raise: ID=~a, Salary=~a\n" 
        (staff1 0) (staff1 1))

;; Struct array updates
(define team (arena-cstruct-array update-arena _staff_record 
                                 (list _int _double) 3
                                 (lambda (i) 
                                   (list (+ 201 i) (* (+ i 1) 40000.0)))))

(printf "Original team:\n")
(for ([i (in-range (team 'len))])
  (define member (team i))
  (printf "  Staff member ~a: ID=~a, Salary=~a\n" 
          i (member 0) (member 1)))

;; Update second staff member's salary
(set-arena-struct-array-field! team 1 1 85000.0)

(printf "After team update:\n")
(for ([i (in-range (team 'len))])
  (define member (team i))
  (printf "  Staff member ~a: ID=~a, Salary=~a\n" 
          i (member 0) (member 1)))

;; Vector element updates
(define cities (arena-vector update-arena 
                           '("Tokyo" "Paris" "London" "New York")))

(printf "Original cities: ")
(for ([i (in-range (cities 'len))])
  (printf "~a " (cities i)))
(printf "\n")

(set-arena-vector-element! cities 2 "Berlin")  ;; Replace London with Berlin
(printf "Updated cities: ")
(for ([i (in-range (cities 'len))])
  (printf "~a " (cities i)))
(printf "\n")

;; Cleanup
(destroy-arena update-arena)

(printf "\n")

;; ======== Deallocation Example ========
(printf "\n=== Deallocation example ===\n")

;; Create a new arena for our deallocation examples
(define dealloc-arena (make-arena (* 1 1024 1024)))

;; Get initial stats to see the arena state
(define initial-stats (arena-get-stats dealloc-arena))
(printf "Initial arena stats: Total=~a, Used=~a, Free=~a, Blocks=~a\n"
        (hash-ref initial-stats 'total)
        (hash-ref initial-stats 'used)
        (hash-ref initial-stats 'free)
        (hash-ref initial-stats 'block-count))

;; Allocate various types of data in the arena
(printf "\nAllocating various data types...\n")
(define d-int (arena-int dealloc-arena 123))
(define d-double (arena-double dealloc-arena 3.14159))
(define d-string (arena-string dealloc-arena "This is a test string for deallocation"))
(define d-array (arena-int-array dealloc-arena '(10 20 30 40 50)))

;; Define a simple struct for the example
(define-cstruct _test_record ([id _int]
                           [value _double]))

(define d-struct (arena-cstruct dealloc-arena _test_record 
                              (list _int _double) 
                              (list 42 99.9)))

;; Check stats after allocation
(define after-alloc-stats (arena-get-stats dealloc-arena))
(printf "After allocation stats: Total=~a, Used=~a, Free=~a, Blocks=~a\n"
        (hash-ref after-alloc-stats 'total)
        (hash-ref after-alloc-stats 'used)
        (hash-ref after-alloc-stats 'free)
        (hash-ref after-alloc-stats 'block-count))

;; Use the allocated data
(printf "\nAllocated values:\n")
(printf "  Integer: ~a\n" (d-int))
(printf "  Double: ~a\n" (d-double))
(printf "  String: ~a\n" (d-string))
(printf "  Array: ~a ~a ~a...\n" (d-array 0) (d-array 1) (d-array 2))
(printf "  Struct: id=~a, value=~a\n" (d-struct 0) (d-struct 1))

;; Deallocate some values individually
(printf "\nDeallocating some values individually...\n")
(deallocate-arena-value d-int)
(printf "Deallocated integer\n")
(deallocate-arena-value d-string)
(printf "Deallocated string\n")

;; Check stats after some deallocation
(define after-dealloc-stats (arena-get-stats dealloc-arena))
(printf "\nAfter partial deallocation stats: Total=~a, Used=~a, Free=~a, Blocks=~a\n"
        (hash-ref after-dealloc-stats 'total)
        (hash-ref after-dealloc-stats 'used)
        (hash-ref after-dealloc-stats 'free)
        (hash-ref after-dealloc-stats 'block-count))

;; Try to allocate new data after deallocating some
(printf "\nAllocating new data after deallocation...\n")
(define d-new-int (arena-int dealloc-arena 456))
(define d-new-string (arena-string dealloc-arena "New string after deallocation"))

;; Use the newly allocated data
(printf "New integer: ~a\n" (d-new-int))
(printf "New string: ~a\n" (d-new-string))

;; Final stats
(define final-stats (arena-get-stats dealloc-arena))
(printf "\nFinal arena stats: Total=~a, Used=~a, Free=~a, Blocks=~a\n"
        (hash-ref final-stats 'total)
        (hash-ref final-stats 'used)
        (hash-ref final-stats 'free)
        (hash-ref final-stats 'block-count))

;; Cleanup
(destroy-arena dealloc-arena)

;; ======== Low-level API ========
(printf "\n=== Low-level API example ===\n")

(define ll-arena (make-arena (* 1 1024 1024)))
(define raw-ptr (ll-arena 20))
;; ** note (ll-arena 20) is short-hand for (allocate-in-arena ll-arena 20)

(for ([i (in-range 10)])
  (ptr-set! (ptr-add raw-ptr i) _byte (+ 48 i)))  ;; ASCII digits 0-9
(ptr-set! (ptr-add raw-ptr 10) _byte 0)  ;; NULL terminator

(define raw-string (cast raw-ptr _pointer _string/utf-8))
(printf "Raw string: ~a\n" raw-string)

;; Cleanup
(destroy-arena ll-arena)

;; ======== With-Arena Example ========
(printf "\n=== With-arena scope example ===\n")
(with-arena temp-arena (* 1 1024 1024)
  (define temp-message (arena-string temp-arena "This message is in a scoped arena"))
  (printf "Scoped message: ~a\n" (temp-message))
  ;; Arena is automatically destroyed when scope exits
)
