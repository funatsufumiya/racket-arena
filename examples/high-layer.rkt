#lang racket/base

(require arena
         ffi/unsafe)

;; Create a 10MB arena
(define my-arena (make-arena (* 10 1024 1024)))

;; String example
(define greeting (arena-string my-arena "Hello, Arena Allocator!"))
(printf "Greeting: ~a\n" (greeting))  ;; Normal usage
(printf "Raw pointer: ~a\n" (greeting 'ptr))  ;; Get pointer when needed

;; Integer array example
(define squares (arena-int-array my-arena '(0 1 4 9 16 25 36 49 64 81)))
(printf "Squares length: ~a\n" (squares 'len))  ;; Get length

(printf "Squares: ")
(for ([i (in-range (squares 'len))])
  (printf "~a " (squares i)))  ;; Access elements by index
(printf "\n")

;; Vector example
(define fruits (arena-vector my-arena '("apple" "banana" "cherry" "date" "elderberry")))
(printf "Number of fruits: ~a\n" (fruits 'len))

(printf "Fruits: ")
(for ([i (in-range (fruits 'len))])
  (printf "~a " (fruits i)))
(printf "\n")

;; Low-level API still available when needed
(define raw-ptr (my-arena 20))
(for ([i (in-range 10)])
  (ptr-set! (ptr-add raw-ptr i) _byte (+ 48 i)))  ;; ASCII digits 0-9
(ptr-set! (ptr-add raw-ptr 10) _byte 0)  ;; NULL terminator

(define raw-string (cast raw-ptr _pointer _string/utf-8))
; (printf "Debug - raw-string type: ~a\n" (object-name (object-name raw-string)))
(printf "Raw string: ~a\n" raw-string)

;; Cleanup
(destroy-arena my-arena)
