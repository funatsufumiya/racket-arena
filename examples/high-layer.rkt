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

;; String with capacity example
(define expandable-text (arena-string my-arena "Short text" 100))  ;; 100 chars capacity
(printf "Initial text: ~a (length: ~a, capacity: ~a)\n" 
        (expandable-text) 
        (expandable-text 'length) 
        (expandable-text 'capacity))

;; Update with a longer string (within capacity)
(set-arena-string! expandable-text "This is a longer text that fits within our capacity")
(printf "Updated text: ~a (new length: ~a, same capacity: ~a)\n" 
        (expandable-text) 
        (expandable-text 'length) 
        (expandable-text 'capacity))

;; ======== Array Examples ========
;; Integer array example
;; Integer array example with capacity
(define squares (arena-int-array my-arena '(0 1 4 9 16 25) 10))
(printf "Squares length: ~a\n" (squares 'size))  ;; Get length
(printf "Squares capacity: ~a\n" (squares 'capacity))  ;; Get capacity

(printf "Squares: ")
(for ([i (in-range (squares 'size))])
  (printf "~a " (squares i)))  ;; Access elements by index
(printf "\n")

;; Using the dynamic functionality
(printf "Adding new elements: ")
(if (arena-array-push! squares 36)
    (printf "Added 36. ")
    (printf "Failed to add 36. "))
(if (arena-array-push! squares 49)
    (printf "Added 49.\n")
    (printf "Failed to add 49.\n"))
(printf "New size: ~a\n" (arena-array-size squares))

(printf "Updated squares: ")
(for ([i (in-range (squares 'size))])
  (printf "~a " (squares i)))
(printf "\n")

;; Popping elements
(define last-elem (arena-array-pop! squares))
(if last-elem
    (printf "Popped: ~a, New size: ~a\n" last-elem (arena-array-size squares))
    (printf "Failed to pop: array is empty\n"))

;; Convert to list
(define squares-list (arena-array->list squares))
(printf "As list: ~a\n" squares-list)

;; Clear the array
(arena-array-clear! squares)
(printf "After clear, size: ~a\n" (arena-array-size squares))

;; ======== String Array Examples ========
;; Create a string array with default capacities
(define names (arena-string-array my-arena '("Alice" "Bob" "Charlie")))
(printf "\nNames array (size: ~a, capacity: ~a, string-capacity: ~a):\n" 
        (names 'size) (names 'capacity) (names 'string-capacity))

;; Access individual strings
(for ([i (in-range (names 'size))])
  (define name-fn (names i))
  (printf "  Name ~a: ~a (length: ~a)\n" 
          i (name-fn) (name-fn 'length)))

;; Create a string array with custom capacity and string-capacity
(define descriptions (arena-string-array 
                      my-arena 
                      '("Short desc" "Another") 
                      10    ;; Array capacity: can hold up to 10 strings
                      100)) ;; String capacity: each string can be up to 100 chars

(printf "\nDescriptions array (size: ~a, capacity: ~a, string-capacity: ~a):\n" 
        (descriptions 'size) (descriptions 'capacity) (descriptions 'string-capacity))

;; Update a string
(set-arena-string-array! descriptions 0 "This is a much longer description")
(printf "Updated description: ~a\n" ((descriptions 0)))

;; Push a new string
(arena-string-array-push! descriptions "Third description added via push")
(printf "\nAfter push (size: ~a):\n" (descriptions 'size))
(for ([i (in-range (descriptions 'size))])
  (printf "  Description ~a: ~a\n" i ((descriptions i))))

;; Pop the last string
(define popped-string (arena-string-array-pop! descriptions))
(printf "\nPopped string: ~a\n" popped-string)
(printf "After pop (size: ~a):\n" (descriptions 'size))
(for ([i (in-range (descriptions 'size))])
  (printf "  Description ~a: ~a\n" i ((descriptions i))))

;; Clear the array
(arena-string-array-clear! descriptions)
(printf "\nAfter clear (size: ~a, capacity still: ~a)\n" 
        (descriptions 'size) (descriptions 'capacity))

;; Add new string after clearing
(arena-string-array-push! descriptions "Fresh start after clearing")
(printf "Added new string after clearing: ~a\n" ((descriptions 0)))

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

;; Create struct array with explicit field types and capacity
;; Starting with 3 points but with room for 10
(define points (arena-cstruct-array my-arena _point (list _double _double) 3
                 (lambda (i) 
                   (list (* i 10.1) (* i 20.2)))
                 10))  ;; Explicit capacity of 10

;; NOTE: The lambda function here is a generator that takes an index (i)
;; and returns the initial values for the struct at that position:
;; - For index 0: (0.0, 0.0)
;; - For index 1: (10.1, 20.2)
;; - For index 2: (20.2, 40.4)

(printf "Points array (size: ~a, capacity: ~a):\n" 
        (points 'size) (points 'capacity))

(for ([i (in-range (points 'size))])
  (define point (points i))
  (printf "  Point ~a: (~a, ~a)\n" 
          i (point 0) (point 1)))

;; Add more points using push operation
(printf "\nAdding more points:\n")

(define new-point1 (list 50.0 75.5))
(if (arena-cstruct-array-push! points new-point1)
    (printf "  Added point: (~a, ~a)\n" (car new-point1) (cadr new-point1))
    (printf "  Failed to add point (array full)\n"))

(define new-point2 (list 42.0 88.8))
(if (arena-cstruct-array-push! points new-point2)
    (printf "  Added point: (~a, ~a)\n" (car new-point2) (cadr new-point2))
    (printf "  Failed to add point (array full)\n"))

;; Show updated array
(printf "\nUpdated points array (size: ~a):\n" (points 'size))
(for ([i (in-range (points 'size))])
  (define point (points i))
  (printf "  Point ~a: (~a, ~a)\n" 
          i (point 0) (point 1)))

;; Remove the last point
(printf "\nRemoving last point:\n")
(if (arena-cstruct-array-pop! points)
    (printf "  Successfully removed the last point\n")
    (printf "  Failed to remove (array empty)\n"))

(printf "After removal (size: ~a):\n" (points 'size))
(for ([i (in-range (points 'size))])
  (define point (points i))
  (printf "  Point ~a: (~a, ~a)\n" 
          i (point 0) (point 1)))

;; Clear all points
(arena-cstruct-array-clear! points)
(printf "\nAfter clearing, size: ~a (capacity still: ~a)\n" 
        (points 'size) (points 'capacity))

;; Add a new point after clearing
(if (arena-cstruct-array-push! points (list 99.9 99.9))
    (printf "Added new point after clearing: (99.9, 99.9)\n")
    (printf "Failed to add after clearing\n"))

;; Show final state
(printf "\nFinal points array (size: ~a):\n" (points 'size))
(for ([i (in-range (points 'size))])
  (define point (points i))
  (printf "  Point ~a: (~a, ~a)\n" 
          i (point 0) (point 1)))

;;
;;
;; Cleanup arena itself (destroy)
(destroy-arena my-arena)
;;
;;

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
(for ([i (in-range (data 'size))])
  (printf "~a " (data i)))
(printf "\n")

(set-arena-double-array! data 1 2.5)
(set-arena-double-array! data 3 4.9)
(printf "Updated array: ")
(for ([i (in-range (data 'size))])
  (printf "~a " (data i)))
(printf "\n")

;; String updates
(define message (arena-string update-arena "Hello world"))
(printf "Original message: ~a\n" (message))
(set-arena-string! message "Hi there!")  ;; Same length
(printf "Updated message: ~a\n" (message))

;; String updates with capacity
(define flexible-text (arena-string update-arena "Initial" 20))
(printf "Original flexible text: ~a (length: ~a, capacity: ~a)\n" 
        (flexible-text) 
        (flexible-text 'length) 
        (flexible-text 'capacity))

;; Update with longer text (within capacity)
(set-arena-string! flexible-text "Longer content now")
(printf "Updated flexible text: ~a (new length: ~a, same capacity: ~a)\n" 
        (flexible-text) 
        (flexible-text 'length) 
        (flexible-text 'capacity))

;; String array updates
(define titles (arena-string-array update-arena 
                                   '("First title" "Second") 
                                   10  ;; capacity 
                                   100)) ;; string-capacity
(printf "\nOriginal titles:\n")
(for ([i (in-range (titles 'size))])
  (printf "  Title ~a: ~a\n" i ((titles i))))

;; Update a title
(set-arena-string-array! titles 1 "Updated second title with more text")
(printf "\nAfter title update:\n")
(for ([i (in-range (titles 'size))])
  (printf "  Title ~a: ~a\n" i ((titles i))))

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

;; NOTE: lambda function here is a generator that takes an index (i)
;; and returns the initial values for the struct at that position:
;; - For index 0: (201, 40000.0)
;; - For index 1: (202, 80000.0)
;; - For index 2: (203, 120000.0)

(printf "Original team:\n")
(for ([i (in-range (team 'size))])
  (define member (team i))
  (printf "  Staff member ~a: ID=~a, Salary=~a\n" 
          i (member 0) (member 1)))

;; Update second staff member's salary
(set-arena-struct-array-field! team 1 1 85000.0)

(printf "After team update:\n")
(for ([i (in-range (team 'size))])
  (define member (team i))
  (printf "  Staff member ~a: ID=~a, Salary=~a\n" 
          i (member 0) (member 1)))

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

(printf "\n\n")