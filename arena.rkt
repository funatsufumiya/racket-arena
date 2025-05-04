#lang racket/base

(require ffi/unsafe
         racket/runtime-path
         racket/match
         racket/list
         (for-syntax racket/base racket/syntax racket/list))

;; Resolve the relative path of the library
(define-runtime-path native-dir "native")

;; Both package and source code use this function
(define (get-library-path)
  (define os (system-type 'os))
  (define so-ext 
    (case os
      [(windows) ".dll"]
      [(macosx) ".dylib"]
      [else ".so"]))
  
  (define lib-name (string-append "libarena" so-ext))
  
  ;; first check the relative position in the runtime path
  (define native-path (build-path native-dir lib-name))
  
  (if (file-exists? native-path)
      native-path
      ;; if not found in runtime path, search in collection path
      (let ([collection-path (collection-path "arena")])
        (build-path collection-path "native" lib-name))))

(define arena-lib 
  (ffi-lib (get-library-path) '("1.0" "")))

(define arena-create
  (get-ffi-obj "arena_create" arena-lib
               (_fun _size -> _pointer)))

(define arena-alloc
  (get-ffi-obj "arena_alloc" arena-lib
               (_fun _pointer _size -> _pointer)))

(define arena-dealloc
  (get-ffi-obj "arena_dealloc" arena-lib
               (_fun _pointer _pointer -> _bool)))

(define arena-stats
  (get-ffi-obj "arena_stats" arena-lib
               (_fun _pointer _pointer _pointer _pointer _pointer -> _bool)))

(define arena-get-allocation-size
  (get-ffi-obj "arena_get_allocation_size" arena-lib
               (_fun _pointer _pointer -> _size)))

(define arena-reset
  (get-ffi-obj "arena_reset" arena-lib
               (_fun _pointer -> _void)))

(define arena-destroy
  (get-ffi-obj "arena_destroy" arena-lib
               (_fun _pointer -> _void)))

(struct arena (ptr)
  #:property prop:procedure
  (lambda (self size) (allocate-in-arena self size)))

(define (make-arena size)
  (define ptr (arena-create size))
  (arena ptr))

(define (allocate-in-arena a size)
  (arena-alloc (arena-ptr a) size))

(define (deallocate-arena-value value-fn)
  (when value-fn
    (let/ec return
      (define ptr 
        (with-handlers ([exn:fail? (lambda (e) (return #f))])
          (value-fn 'ptr)))
      (define a
        (with-handlers ([exn:fail? (lambda (e) (return #f))])
          (value-fn 'arena)))
      (when (and ptr a)
        (deallocate-from-arena a ptr)))))

;; Low-level deallocate API
(define (deallocate-from-arena a ptr)
  (arena-dealloc (arena-ptr a) ptr))

(define (arena-get-stats a)
  (define buffer (make-bytes (* 4 8)))
  
  (define ptr (cast buffer _bytes _pointer))
  
  (define total-ptr ptr)
  (define used-ptr (ptr-add ptr 8))
  (define free-ptr (ptr-add ptr 16))
  (define block-count-ptr (ptr-add ptr 24))
  
  (define success (arena-stats (arena-ptr a) total-ptr used-ptr free-ptr block-count-ptr))
  
  (if success
      (hash 'total (ptr-ref total-ptr _size)
            'used (ptr-ref used-ptr _size)
            'free (ptr-ref free-ptr _size)
            'block-count (ptr-ref block-count-ptr _size))
      #f))

(define (get-allocation-size a ptr)
  (arena-get-allocation-size (arena-ptr a) ptr))

(define (reset-arena a)
  (arena-reset (arena-ptr a)))

(define (destroy-arena a)
  (arena-destroy (arena-ptr a)))

; utility
;; Calculate the offset of a struct field
(define (ctype-offset type field-index)
  (define layout (ctype->layout type))
  (define field-info (vector-ref (second layout) field-index))
  (define offset (list-ref (car field-info) 0))
  offset)

;; Helper function to align offset according to type requirements
(define (align-offset offset alignment)
  ;; Round up to the nearest multiple of alignment
  (* (ceiling (/ offset alignment)) alignment))

;; ======== Primitive Type Definition ========
;; Define list of supported primitive types at compile time
(define-for-syntax primitive-types 
  '((int _int)
    (double _double)
    (float _float)
    (char _byte)
    (bool _bool)
    (short _short)
    (long _long)))

;; ======== Primitive Type Operations ========
;; Macro to generate primitive type allocators (int, double, etc.)
(define-syntax (define-arena-primitive stx)
  (syntax-case stx ()
    [(_ name type-expr)
     (with-syntax ([arena-name (format-id stx "arena-~a" #'name)])
       #'(begin
           (define (arena-name arena value)
             (define type type-expr)
             (define ptr (arena (ctype-sizeof type)))
             ;; Handle numeric conversions if needed
             (define actual-value
               (cond
                [(and (number? value) (eq? type _double)) (exact->inexact value)]
                [(and (number? value) (eq? type _float)) (exact->inexact value)]
                [else value]))
             (ptr-set! ptr type actual-value)
             (case-lambda
               [() (ptr-ref ptr type)]
               [(arg) (cond
                        [(eq? arg 'ptr) ptr]
                        [(eq? arg 'arena) arena]
                        [else (error (format "Invalid argument to ~a function" 'name))])])) 
           (provide arena-name)))]))

;; ======== Array Operations Macro ========
;; Macro to generate array allocators for various types
;; ======== Array Operations with Dynamic Sizing ========
;; Array structure with capacity and size information
;; ======== Array Operations with Dynamic Sizing ========
;; Array structure with capacity and size information
(define-syntax (define-arena-array stx)
  (syntax-case stx ()
    [(_ name type-expr)
     (with-syntax ([arena-name (format-id stx "arena-~a-array" #'name)])
       #'(begin
           (define (arena-name arena values [capacity (length values)])
             (define type type-expr)
             (define len (length values))
             (define type-size (ctype-sizeof type))
             
             ;; Ensure capacity is at least the size of initial values
             (when (< capacity len)
               (error 'arena-name "capacity (~a) must be >= length of values (~a)" capacity len))
             
             ;; Calculate memory layout: [capacity(8 bytes)][size(8 bytes)][data...]
             (define header-size 16)  ;; 8 bytes for capacity + 8 bytes for current size
             (define data-size (* capacity type-size))
             (define total-size (+ header-size data-size))
             
             (define ptr (arena total-size))
             
             ;; Initialize header values - CRITICAL FIX HERE
             (ptr-set! ptr _uint64 capacity)            ;; Store maximum capacity
             (ptr-set! (ptr-add ptr 8) _uint64 len)     ;; Store current size
             
             ;; Copy values with appropriate conversion
             (for ([i (in-range len)]
                   [v (in-list values)])
               (define actual-value
                 (cond
                  [(and (number? v) (eq? type _double)) (exact->inexact v)]
                  [(and (number? v) (eq? type _float)) (exact->inexact v)]
                  [else v]))
               (ptr-set! (ptr-add ptr (+ header-size (* i type-size))) type actual-value))
             
             ;; Return a multi-dispatch function
             (lambda args
               (cond
                [(null? args)
                 (if (= len 1)
                     (ptr-ref (ptr-add ptr (+ header-size 0)) type)
                     (error "Array has multiple elements, index or operation required"))]
                [(= (length args) 1)
                 (let ([arg (car args)])
                   (cond
                     [(number? arg)
                      (define current-size (ptr-ref (ptr-add ptr 8) _uint64))
                      (if (< arg current-size)
                          (ptr-ref (ptr-add ptr (+ header-size (* arg type-size))) type)
                          (error 'arena-name "index out of bounds: ~a (size: ~a)" arg current-size))]
                     [(eq? arg 'ptr) ptr]
                     [(eq? arg 'arena) arena]
                     [(eq? arg 'type) type]
                     [(eq? arg 'element-size) type-size]
                     [(eq? arg 'len) (ptr-ref (ptr-add ptr 8) _uint64)]
                     [(eq? arg 'size) (ptr-ref (ptr-add ptr 8) _uint64)]
                     [(eq? arg 'capacity) (ptr-ref ptr _uint64)]
                     [else (error "Invalid argument to array function")]))]
                [else (error "Too many arguments provided to array function")])))
           (provide arena-name)))]))

;; Array push operation: add element to end of array
;; Returns #t if successful, #f if array is at capacity
(define (arena-array-push! array-fn value)
  (define ptr (array-fn 'ptr))
  (define type (array-fn 'type))
  (define element-size (array-fn 'element-size))
  (define current-size (array-fn 'size))
  (define capacity (array-fn 'capacity))
  
  ;; Check if array is full
  (if (>= current-size capacity)
      #f  ;; Return false if full
      (let ()  ;; Use let instead of begin+define
        ;; Convert value if necessary
        (define actual-value
          (cond
           [(and (number? value) (eq? type _double)) (exact->inexact value)]
           [(and (number? value) (eq? type _float)) (exact->inexact value)]
           [else value]))
        
        ;; Add element at the end
        (define header-size 16)
        (ptr-set! (ptr-add ptr (+ header-size (* current-size element-size))) type actual-value)
        
        ;; Update size
        (ptr-set! (ptr-add ptr 8) _uint64 (add1 current-size))
        
        ;; Return success
        #t)))

;; Array pop operation: remove and return last element
;; Returns the element if successful, #f if array is empty
(define (arena-array-pop! array-fn)
  (define ptr (array-fn 'ptr))
  (define type (array-fn 'type))
  (define element-size (array-fn 'element-size))
  (define current-size (array-fn 'size))
  
  ;; Check if array is empty
  (if (= current-size 0)
      #f  ;; Return false if empty
      (let ()  ;; Use let instead of begin+define
        ;; Get the last element
        (define header-size 16)
        (define last-index (sub1 current-size))
        (define last-element 
          (ptr-ref (ptr-add ptr (+ header-size (* last-index element-size))) type))
        
        ;; Update size
        (ptr-set! (ptr-add ptr 8) _uint64 last-index)
        
        ;; Return the removed element
        last-element)))

;; Clear array (reset to empty)
;; Returns void as clearing should always succeed
(define (arena-array-clear! array-fn)
  (define ptr (array-fn 'ptr))
  
  ;; Reset size to 0 (capacity remains the same)
  (ptr-set! (ptr-add ptr 8) _uint64 0)
  
  ;; Return void
  (void))

;; Get array as list
(define (arena-array->list array-fn)
  (define current-size (array-fn 'size))
  
  ;; Build list from array elements
  (for/list ([i (in-range current-size)])
    (array-fn i)))

;; Size getter - convenience function
(define (arena-array-size array-fn)
  (array-fn 'size))

;; Capacity getter - convenience function
(define (arena-array-capacity array-fn)
  (array-fn 'capacity))

;; Export array functions
(provide arena-array-push!
         arena-array-pop!
         arena-array-clear!
         arena-array->list
         arena-array-size
         arena-array-capacity)

;; Macro to automatically define all primitive type functions
(define-syntax (define-all-primitives stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([(definition ...)
                   (for/list ([p (in-list primitive-types)])
                     (with-syntax ([name (datum->syntax stx (first p))]
                                   [type (datum->syntax stx (second p))])
                       #'(define-arena-primitive name type)))])
       #'(begin definition ...))]))

;; Macro to automatically define all array functions
(define-syntax (define-all-arrays stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([(definition ...)
                   (for/list ([p (in-list primitive-types)])
                     (with-syntax ([name (datum->syntax stx (first p))]
                                   [type (datum->syntax stx (second p))])
                       #'(define-arena-array name type)))])
       #'(begin definition ...))]))

;; Generate all primitive type functions
(define-all-primitives)

;; Generate all array functions
(define-all-arrays)

;; ======== String Operations ========
;; Allocate a string in the arena
(define (arena-string arena str)
  (define len (string-length str))
  (define bytes (+ len 1))  ;; Extra byte for NULL terminator
  (define ptr (arena bytes))
  
  ;; Copy string data & NULL terminator
  (for ([i (in-range len)]
        [c (in-string str)])
    (ptr-set! (ptr-add ptr i) _byte (char->integer c)))
  (ptr-set! (ptr-add ptr len) _byte 0)
  
  ;; Return a multi-dispatch function
  (case-lambda
    [() (cast ptr _pointer _string/utf-8)]
    [(arg) (cond
             [(eq? arg 'ptr) ptr]
             [(eq? arg 'arena) arena]
             [else (error "Invalid argument to string function")])]))

;; Read string from pointer (when needed)
(define (arena-string-ref ptr)
  (cast ptr _pointer _string/utf-8))

;; ======== C Struct Operations ========
;; Allocate a single C struct with field type information
(define (arena-cstruct arena type field-types [init-values #f])
  (define bytes (ctype-sizeof type))
  (define ptr (arena bytes))
  
  ;; Calculate field offsets
  (define field-offsets 
    (let loop ([offset 0] [types field-types] [offsets '()])
      (if (null? types)
          (reverse offsets)
          (let* ([current-type (car types)]
                 [size (ctype-sizeof current-type)]
                 [aligned-offset (align-offset offset size)])
            (loop (+ aligned-offset size) 
                  (cdr types) 
                  (cons aligned-offset offsets))))))
  
  #;(printf "Calculated field offsets: ~a\n" field-offsets)
  
  ;; Set initial values if provided
  (when init-values
    (for ([i (in-range (min (length init-values) (length field-types)))]
          [v (in-list init-values)]
          [ftype (in-list field-types)]
          [offset (in-list field-offsets)])
      #;(printf "Setting field ~a at offset ~a: type=~a, value=~a\n" 
              i offset ftype v)
      ;; Use ptr-add to get to the correct field position, then set value
      (ptr-set! (ptr-add ptr offset) ftype v)))
  
  ;; Return a multi-dispatch function
  (case-lambda
    [(idx) (cond
             [(number? idx) 
              (if (< idx (length field-types))
                  ;; Access field by offset
                  (ptr-ref (ptr-add ptr (list-ref field-offsets idx)) 
                           (list-ref field-types idx))
                  (error 'arena-cstruct "index out of bounds: ~a" idx))]
             [(eq? idx 'ptr) ptr]
             [(eq? idx 'type) type]
             [(eq? idx 'field-types) field-types]
             [(eq? idx 'field-offsets) field-offsets]
             [else (error "Invalid argument to struct function")])]
    [() (ptr-ref (ptr-add ptr (car field-offsets)) (car field-types))]))

;; Allocate an array of C structs with field type information
(define (arena-cstruct-array arena type field-types count [init-func #f] [capacity count])
  ;; Ensure capacity is at least the initial count
  (when (< capacity count)
    (error 'arena-cstruct-array "capacity (~a) must be >= initial count (~a)" capacity count))
  
  ;; Calculate field offsets (same as existing code)
  (define field-offsets 
    (let loop ([offset 0] [types field-types] [offsets '()])
      (if (null? types)
          (reverse offsets)
          (let* ([current-type (car types)]
                 [size (ctype-sizeof current-type)]
                 [aligned-offset (align-offset offset size)])
            (loop (+ aligned-offset size) 
                  (cdr types) 
                  (cons aligned-offset offsets))))))
  
  ;; Calculate struct size (same as existing code)
  (define struct-size 
    (if (null? field-offsets)
        (ctype-sizeof type)
        (let* ([last-offset (last field-offsets)]
               [last-type (last field-types)]
               [last-size (ctype-sizeof last-type)])
          (+ last-offset last-size))))
  
  ;; Add header size for capacity and current size
  (define header-size 16)  ;; 8 bytes for capacity + 8 bytes for size
  
  ;; Calculate total memory size needed
  (define data-size (* capacity struct-size))
  (define total-size (+ header-size data-size))
  
  ;; Allocate memory
  (define ptr (arena total-size))
  
  ;; Set header information
  (ptr-set! ptr _uint64 capacity)         ;; Store maximum capacity
  (ptr-set! (ptr-add ptr 8) _uint64 count)  ;; Store current size
  
  ;; Initialize with values if provided
  (when init-func
    (for ([i (in-range count)])
      (define struct-offset (+ header-size (* i struct-size)))
      (define struct-base-ptr (ptr-add ptr struct-offset))
      (define values (init-func i))
      
      ;; Set field values
      (for ([j (in-range (min (length values) (length field-types)))]
            [v (in-list values)]
            [ftype (in-list field-types)]
            [offset (in-list field-offsets)])
        
        ;; Convert value if necessary
        (define actual-value
          (cond
            [(and (eq? ftype _double) (number? v)) (exact->inexact v)]
            [(and (eq? ftype _float) (number? v)) (exact->inexact v)]
            [else v]))
        
        ;; Set the value
        (ptr-set! (ptr-add struct-base-ptr offset) ftype actual-value))))
  
  ;; Return multi-dispatch function
  (lambda args
    (cond
      [(null? args)
       (error "Index required to access struct in array")]
      [(= (length args) 1)
       (let ([arg (car args)])
         (cond
           [(number? arg)  ;; Access by index
            (define current-size (ptr-ref (ptr-add ptr 8) _uint64))
            (if (< arg current-size)
                (let ([struct-offset (+ header-size (* arg struct-size))]
                      [struct-ptr (ptr-add ptr (+ header-size (* arg struct-size)))])
                  (lambda args
                    (cond
                      [(null? args)
                       (error "Field index required")]
                      [(= (length args) 1)
                       (let ([field-idx (car args)])
                         (cond
                           [(number? field-idx)
                            (if (< field-idx (length field-types))
                                (ptr-ref (ptr-add struct-ptr
                                                  (list-ref field-offsets field-idx))
                                         (list-ref field-types field-idx))
                                (error 'arena-cstruct-array
                                       "field index out of bounds: ~a" field-idx))]
                           [(eq? field-idx 'ptr) struct-ptr]
                           [(eq? field-idx 'field-types) field-types]
                           [(eq? field-idx 'field-offsets) field-offsets]
                           [else (error "Invalid field index")]))]
                      [else (error "Too many arguments")])))
                (error 'arena-cstruct-array "index out of bounds: ~a (size: ~a)"
                       arg current-size))]
           [(eq? arg 'ptr) ptr]
           [(eq? arg 'type) type]
           [(eq? arg 'field-types) field-types]
           [(eq? arg 'field-offsets) field-offsets]
           [(eq? arg 'size) (ptr-ref (ptr-add ptr 8) _uint64)]
           [(eq? arg 'capacity) (ptr-ref ptr _uint64)]
           [else (error "Invalid argument to struct array function")]))]
      [else (error "Too many arguments provided to array function")])))

;; Add a new struct to the end of the array
;; Returns #t if successful, #f if array is at capacity
(define (arena-cstruct-array-push! array-fn init-values)
  (define ptr (array-fn 'ptr))
  (define type (array-fn 'type))
  (define field-types (array-fn 'field-types))
  (define field-offsets (array-fn 'field-offsets))
  (define current-size (array-fn 'size))
  (define capacity (array-fn 'capacity))
  
  ;; Check if array is full
  (if (>= current-size capacity)
      #f  ;; Return false if full
      (let ()
        ;; Calculate struct size
        (define last-offset (last field-offsets))
        (define last-type (last field-types))
        (define last-size (ctype-sizeof last-type))
        (define struct-size (+ last-offset last-size))
        
        ;; Calculate position for new struct
        (define header-size 16)
        (define struct-offset (+ header-size (* current-size struct-size)))
        (define struct-ptr (ptr-add ptr struct-offset))
        
        ;; Set field values
        (for ([i (in-range (min (length init-values) (length field-types)))]
              [v (in-list init-values)]
              [ftype (in-list field-types)]
              [offset (in-list field-offsets)])
          
          ;; Convert value if necessary
          (define actual-value
            (cond
              [(and (eq? ftype _double) (number? v)) (exact->inexact v)]
              [(and (eq? ftype _float) (number? v)) (exact->inexact v)]
              [else v]))
          
          ;; Set the value
          (ptr-set! (ptr-add struct-ptr offset) ftype actual-value))
        
        ;; Update size
        (ptr-set! (ptr-add ptr 8) _uint64 (add1 current-size))
        
        ;; Return success
        #t)))

;; Remove the last struct from the array
;; Returns #t if successful, #f if array is empty
(define (arena-cstruct-array-pop! array-fn)
  (define current-size (array-fn 'size))
  
  ;; Check if array is empty
  (if (= current-size 0)
      #f  ;; Return false if empty
      (let ()
        ;; Get pointer to array
        (define ptr (array-fn 'ptr))
        
        ;; Update size
        (ptr-set! (ptr-add ptr 8) _uint64 (sub1 current-size))
        
        ;; Return success
        #t)))

;; Clear struct array (reset to empty)
(define (arena-cstruct-array-clear! array-fn)
  (define ptr (array-fn 'ptr))
  
  ;; Reset size to 0 (capacity remains the same)
  (ptr-set! (ptr-add ptr 8) _uint64 0)
  
  ;; Return void
  (void))

;; Export struct array functions
(provide arena-cstruct-array-push!
         arena-cstruct-array-pop!
         arena-cstruct-array-clear!)

;; ======== Setter Functions for Primitive Types ========
;; Macro to generate primitive type setters
(define-syntax (define-primitive-setter stx)
  (syntax-case stx ()
    [(_ name type-expr)
     (with-syntax ([setter-name (format-id stx "set-arena-~a!" #'name)])
       #'(begin
           (define (setter-name value-fn new-value)
             (define type type-expr)
             ;; Handle numeric conversions if needed
             (define actual-value
               (cond
                [(and (number? new-value) (eq? type _double)) (exact->inexact new-value)]
                [(and (number? new-value) (eq? type _float)) (exact->inexact new-value)]
                [else new-value]))
             (ptr-set! (value-fn 'ptr) type actual-value))
           (provide setter-name)))]))

;; Macro to automatically define all primitive setters
(define-syntax (define-all-primitive-setters stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([(definition ...)
                   (for/list ([p (in-list primitive-types)])
                     (with-syntax ([name (datum->syntax stx (first p))]
                                   [type (datum->syntax stx (second p))])
                       #'(define-primitive-setter name type)))])
       #'(begin definition ...))]))

;; Generate all primitive setter functions
(define-all-primitive-setters)

;; ======== Setter Functions for Arrays ========
;; Macro to generate array setters
(define-syntax (define-array-setter stx)
  (syntax-case stx ()
    [(_ name type-expr)
     (with-syntax ([setter-name (format-id stx "set-arena-~a-array!" #'name)])
       #'(begin
           (define (setter-name array-fn index new-value)
             (define type type-expr)
             (define current-size (array-fn 'size))
             (define type-size (array-fn 'element-size))
             (define ptr (array-fn 'ptr))
             
             ;; Check bounds
             (unless (< index current-size)
               (error 'setter-name "index out of bounds: ~a (size: ~a)" index current-size))
             
             ;; Convert value if necessary
             (define actual-value
               (cond
                [(and (number? new-value) (eq? type _double)) (exact->inexact new-value)]
                [(and (number? new-value) (eq? type _float)) (exact->inexact new-value)]
                [else new-value]))
             
             ;; Set the value
             (define header-size 16)
             (ptr-set! (ptr-add ptr (+ header-size (* index type-size))) type actual-value))
           (provide setter-name)))]))

;; Macro to automatically define all array setters
(define-syntax (define-all-array-setters stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([(definition ...)
                   (for/list ([p (in-list primitive-types)])
                     (with-syntax ([name (datum->syntax stx (first p))]
                                   [type (datum->syntax stx (second p))])
                       #'(define-array-setter name type)))])
       #'(begin definition ...))]))

;; Generate all array setter functions
(define-all-array-setters)

;; ======== String Setter Function ========
;; Set a new string value
(define (set-arena-string! str-fn new-string)
  (define ptr (str-fn 'ptr))
  (define old-string (str-fn))
  (define old-len (string-length old-string))
  (define new-len (string-length new-string))
  
  ;; Check if new string fits in the allocated memory
  ;; (allowing for NULL terminator)
  (when (> (+ new-len 1) (+ old-len 1))
    (error 'set-arena-string! 
           "new string (~a chars) is longer than allocated space (~a chars)" 
           new-len old-len))
  
  ;; Copy new string data
  (for ([i (in-range new-len)]
        [c (in-string new-string)])
    (ptr-set! (ptr-add ptr i) _byte (char->integer c)))
  
  ;; Add NULL terminator
  (ptr-set! (ptr-add ptr new-len) _byte 0)
  
  ;; Clear any remaining bytes (optional but good practice)
  (when (< new-len old-len)
    (for ([i (in-range (add1 new-len) (add1 old-len))])
      (ptr-set! (ptr-add ptr i) _byte 0))))

;; ======== C Struct Field Setter Function ========
;; Set a field value in a C struct
(define (set-arena-struct-field! struct-fn field-index new-value)
  (define ptr (struct-fn 'ptr))
  (define field-types (struct-fn 'field-types))
  (define field-offsets (struct-fn 'field-offsets))
  
  ;; Check bounds
  (unless (< field-index (length field-types))
    (error 'set-arena-struct-field! "field index out of bounds: ~a" field-index))
  
  (define field-type (list-ref field-types field-index))
  (define offset (list-ref field-offsets field-index))
  
  ;; Convert value if necessary
  (define actual-value
    (cond
      [(and (number? new-value) (eq? field-type _double)) (exact->inexact new-value)]
      [(and (number? new-value) (eq? field-type _float)) (exact->inexact new-value)]
      [else new-value]))
  
  ;; Set the field value
  (ptr-set! (ptr-add ptr offset) field-type actual-value))

;; Set a field value in a struct within an array
(define (set-arena-struct-array-field! struct-array-fn struct-index field-index new-value)
  (define struct-fn (struct-array-fn struct-index))
  (set-arena-struct-field! struct-fn field-index new-value))

;; Export the newly added setter functions
(provide set-arena-string!
         set-arena-struct-field!
         set-arena-struct-array-field!)

;; ======== Scope Management ========
;; Scoped arena operations
(define-syntax-rule (with-arena arena-name size body ...)
  (let ([arena-name (make-arena size)])
    (dynamic-wind
      (lambda () #f)
      (lambda () body ...)
      (lambda () (destroy-arena arena-name)))))

;; Export remaining functions not covered by the automatic export macros
(provide make-arena
         allocate-in-arena
         deallocate-from-arena
         deallocate-arena-value
         arena-get-stats
         get-allocation-size
         reset-arena
         destroy-arena
         arena?
         arena
         arena-string
         arena-string-ref
         arena-cstruct
         arena-cstruct-array
         with-arena)
