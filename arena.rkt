#lang racket/base

(require ffi/unsafe
         racket/runtime-path
         racket/match)

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

;; ======== Numeric Array Operations ========
;; Allocate integer array
(define (arena-int-array arena values)
  (define len (length values))
  (define int-size (ctype-sizeof _int))
  (define bytes (* len int-size))
  (define ptr (arena bytes))
  
  ;; Copy values
  (for ([i (in-range len)]
        [v (in-list values)])
    (ptr-set! (ptr-add ptr (* i int-size)) _int v))
  
  ;; Return a multi-dispatch function
  (case-lambda
    [(idx) (cond
             [(number? idx)                             ;; Normal usage - get element at index
              (if (< idx len)
                  (ptr-ref (ptr-add ptr (* idx int-size)) _int)
                  (error 'arena-int-array "index out of bounds: ~a" idx))]
             [(eq? idx 'ptr) ptr]                       ;; Get pointer with 'ptr symbol
             [(eq? idx 'len) len]                       ;; Get length with 'len symbol
             [else (error "Invalid argument to int array function")])]
    [() (if (= len 1)                                  ;; No arguments - get first element if singleton
            (ptr-ref ptr _int 0)
            (error "Array has multiple elements, index required"))]))

;; Allocate floating-point array
(define (arena-double-array arena values)
  (define len (length values))
  (define double-size (ctype-sizeof _double))
  (define bytes (* len double-size))
  (define ptr (arena bytes))
  
  ;; Copy values
  (for ([i (in-range len)]
        [v (in-list values)])
    ;; Make sure v is explicitly converted to a double
    (ptr-set! (ptr-add ptr (* i double-size)) _double (exact->inexact v)))
  
  ;; Return a multi-dispatch function
  (case-lambda
    [(idx) (cond
             [(number? idx)                             ;; Normal usage - get element at index
              (if (< idx len)
                  (ptr-ref (ptr-add ptr (* idx double-size)) _double)
                  (error 'arena-double-array "index out of bounds: ~a" idx))]
             [(eq? idx 'ptr) ptr]                       ;; Get pointer with 'ptr symbol
             [(eq? idx 'len) len]                       ;; Get length with 'len symbol
             [else (error "Invalid argument to double array function")])]
    [() (if (= len 1)                                  ;; No arguments - get first element if singleton
            (ptr-ref ptr _double 0)
            (error "Array has multiple elements, index required"))]))

;; ======== Struct Operations ========
;; Allocate a struct
(define (arena-struct arena type init-values)
  (define bytes (ctype-sizeof type))
  (define ptr (arena bytes))
  
  ;; Set initial values if provided
  (when init-values
    (for ([i (in-range (length init-values))]
          [v (in-list init-values)])
      (ptr-set! ptr type v i)))
  
  ;; Return a multi-dispatch function
  (case-lambda
    [(idx) (cond
             [(number? idx) (ptr-ref ptr type idx)]     ;; Normal usage - get field at index
             [(eq? idx 'ptr) ptr]                       ;; Get pointer with 'ptr symbol
             [else (error "Invalid argument to struct function")])]
    [() (ptr-ref ptr type 0)]))                        ;; No arguments - get first field

;; ======== Collection Operations ========
;; Implement simple arena-based vector
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
(define-syntax-rule (with-arena size body ...)
  (let ([a (make-arena size)])
    (dynamic-wind
      (lambda () #f)
      (lambda () 
        (let () ; This creates a context that allows definitions
          body ...))
      (lambda () (destroy-arena a)))))

(provide make-arena
         allocate-in-arena
         reset-arena
         destroy-arena
         arena?
         arena
         arena-string
         arena-string-ref
         arena-int-array
         arena-double-array
         arena-struct
         arena-vector
         with-arena)
