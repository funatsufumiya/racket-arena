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
               [(arg) (if (eq? arg 'ptr) ptr
                         (error (format "Invalid argument to ~a function" 'name)))]))
           (provide arena-name)))]))

;; ======== Array Operations Macro ========
;; Macro to generate array allocators for various types
(define-syntax (define-arena-array stx)
  (syntax-case stx ()
    [(_ name type-expr)
     (with-syntax ([arena-name (format-id stx "arena-~a-array" #'name)])
       #'(begin
           (define (arena-name arena values)
             (define type type-expr)
             (define len (length values))
             (define type-size (ctype-sizeof type))
             (define bytes (* len type-size))
             (define ptr (arena bytes))
             
             ;; Copy values with appropriate conversion
             (for ([i (in-range len)]
                   [v (in-list values)])
               (define actual-value
                 (cond
                  [(and (number? v) (eq? type _double)) (exact->inexact v)]
                  [(and (number? v) (eq? type _float)) (exact->inexact v)]
                  [else v]))
               (ptr-set! (ptr-add ptr (* i type-size)) type actual-value))
             
             ;; Return a multi-dispatch function
             (case-lambda
               [(idx) (cond
                        [(number? idx)                           
                         (if (< idx len)
                             (ptr-ref (ptr-add ptr (* idx type-size)) type)
                             (error 'arena-name "index out of bounds: ~a" idx))]
                        [(eq? idx 'ptr) ptr]                    
                        [(eq? idx 'len) len]                    
                        [else (error "Invalid argument to array function")])]
               [() (if (= len 1)                               
                      (ptr-ref ptr type 0)
                      (error "Array has multiple elements, index required"))]))
           (provide arena-name)))]))

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
  
  ;; Copy string data
  (for ([i (in-range len)]
        [c (in-string str)])
    (ptr-set! (ptr-add ptr i) _byte (char->integer c)))
  
  ;; Add NULL terminator
  (ptr-set! (ptr-add ptr len) _byte 0)
  
  ;; Return a multi-dispatch function
  (case-lambda
    [() (cast ptr _pointer _string/utf-8)]             ;; Normal usage - get string
    [(arg) (if (eq? arg 'ptr) ptr                      ;; Get pointer with 'ptr symbol
               (error "Invalid argument to string function"))]))

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
(define (arena-cstruct-array arena type field-types count [init-func #f])
  ;; Calculate field offsets once
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
  
  #;(printf "Array struct field offsets: ~a\n" field-offsets)
  
  ;; Calculate struct size with proper alignment
  (define struct-size 
    (if (null? field-offsets)
        (ctype-sizeof type)  ;; Fallback to type's size
        (let* ([last-offset (last field-offsets)]
               [last-type (last field-types)]
               [last-size (ctype-sizeof last-type)])
          (+ last-offset last-size))))
  
  (define total-size (* count struct-size))
  (define ptr (arena total-size))
  
  ;; Initialize each struct if init function is provided
  (when init-func
    (for ([i (in-range count)])
      (define struct-base-ptr (ptr-add ptr (* i struct-size)))
      (define values (init-func i))
      
      ;; Set field values using the appropriate types at correct offsets
      (for ([j (in-range (min (length values) (length field-types)))]
            [v (in-list values)]
            [ftype (in-list field-types)]
            [offset (in-list field-offsets)])
        #;(printf "Setting array[~a] field ~a at offset ~a: type=~a, value=~a\n" 
                i j offset ftype v)
        
        ;; Ensure numeric values are properly converted to the target type
        (define actual-value
          (cond
            [(and (eq? ftype _double) (number? v)) (exact->inexact v)]
            [(and (eq? ftype _float) (number? v)) (exact->inexact v)]
            [else v]))
        
        ;; Add field offset to struct base pointer
        (ptr-set! (ptr-add struct-base-ptr offset) ftype actual-value))))
  
  ;; Return a multi-dispatch function
  (case-lambda
    [(idx) (cond
             [(number? idx)                            ;; Get struct at index
              (if (< idx count)
                  (let ([struct-ptr (ptr-add ptr (* idx struct-size))])
                    (case-lambda
                      [(field-idx) (cond
                                    [(number? field-idx) 
                                     (if (< field-idx (length field-types))
                                         ;; Access field by its offset
                                         (ptr-ref (ptr-add struct-ptr 
                                                           (list-ref field-offsets field-idx))
                                                 (list-ref field-types field-idx))
                                         (error 'arena-cstruct-array 
                                                "field index out of bounds: ~a" field-idx))]
                                    [(eq? field-idx 'ptr) struct-ptr]
                                    [(eq? field-idx 'field-types) field-types]
                                    [(eq? field-idx 'field-offsets) field-offsets]
                                    [else (error "Invalid field index")])]
                      [() (ptr-ref (ptr-add struct-ptr (car field-offsets)) 
                                   (car field-types))]))
                  (error 'arena-cstruct-array "index out of bounds: ~a" idx))]
             [(eq? idx 'ptr) ptr]                     ;; Get base pointer
             [(eq? idx 'len) count]                   ;; Get array length
             [(eq? idx 'type) type]                   ;; Get struct type
             [(eq? idx 'field-types) field-types]     ;; Get field types
             [(eq? idx 'field-offsets) field-offsets] ;; Get field offsets
             [else (error "Invalid argument to struct array function")])]
    [() (error "Index required to access struct in array")]))

;; ======== Collection Operations ========
;; Implement arena-based vector (for racket-style vectors)
(define (arena-vector arena elems)
  (define len (length elems))
  (define ptr-size (ctype-sizeof _pointer))
  (define bytes (* (+ len 1) ptr-size))  ;; Space for length + data
  (define ptr (arena bytes))
  
  ;; Set length
  (ptr-set! ptr _int len 0)
  
  ;; Copy elements (simplified to support strings only)
  (for ([i (in-range len)]
        [elem (in-list elems)])
    (define str-fn (arena-string arena elem))
    (ptr-set! (ptr-add ptr (+ ptr-size (* i ptr-size))) _pointer 
              (str-fn 'ptr)))
  
  ;; Return a multi-dispatch function
  (case-lambda
    [(idx) (cond
             [(number? idx)                             ;; Normal usage - get element at index
              (if (< idx len)
                  (arena-string-ref 
                   (ptr-ref (ptr-add ptr (+ ptr-size (* idx ptr-size))) _pointer))
                  (error 'arena-vector "index out of bounds: ~a" idx))]
             [(eq? idx 'ptr) ptr]                       ;; Get pointer with 'ptr symbol
             [(eq? idx 'len) len]                       ;; Get length with 'len symbol
             [else (error "Invalid argument to vector function")])]
    [() (if (= len 1)                                  ;; No arguments - get first element if singleton
            (arena-string-ref (ptr-ref (ptr-add ptr ptr-size) _pointer))
            (error "Vector has multiple elements, index required"))]))

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
         reset-arena
         destroy-arena
         arena?
         arena
         arena-string
         arena-string-ref
         arena-cstruct
         arena-cstruct-array
         arena-vector
         with-arena)
