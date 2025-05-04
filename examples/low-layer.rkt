#lang racket/base

;; NOTE: recommend to check high-layer.rkt first for High-level API usage

(require arena
         ffi/unsafe)

;; create arena with size 1MB
(define my-arena (make-arena (* 1 1024 1024)))

;; allocate string with length 10
(define str-len 10)
(define str-ptr (my-arena str-len))
;; ** note (my-arena str-len) is short-hand for (allocate-in-arena my-arena str-len)

;; write string data
(for ([i (in-range str-len)])
  (ptr-set! (ptr-add str-ptr i) _byte (+ (char->integer #\A) i)))

(printf "String data bytes: ")
(for ([i (in-range str-len)])
  (printf "~a " (ptr-ref str-ptr _byte i)))
(printf "\n")

;; read string data
(define result
  (list->string
   (for/list ([i (in-range str-len)])
     (integer->char (ptr-ref str-ptr _byte i)))))

(printf "Allocated string: ~a\n" result)

;; reset arena
(reset-arena my-arena)

;; allocate integer array with size 5
(define int-array-size 5)
(define int-array-ptr (my-arena (* int-array-size (ctype-sizeof _int))))

;; write integer array
(define int-size (ctype-sizeof _int))
(for ([i (in-range int-array-size)])
  (ptr-set! (ptr-add int-array-ptr (* i int-size)) _int (* i i)))

(printf "Integer data bytes: ")
(for ([i (in-range (* int-array-size (ctype-sizeof _int)))])
  (printf "~a " (ptr-ref int-array-ptr _byte i)))
(printf "\n")

;; read integer array
(define int-results
  (for/list ([i (in-range int-array-size)])
    (ptr-ref int-array-ptr _int i)))

(printf "Allocated integers: ~a\n" int-results)

;; destroy arena
(destroy-arena my-arena)