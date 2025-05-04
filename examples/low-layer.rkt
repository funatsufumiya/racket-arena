#lang racket/base

(require arena
         ffi/unsafe)

(printf "\n=== Low-layer extended examples ===\n")

;; Create arena with size 1MB
(define my-arena (make-arena (* 1 1024 1024)))

;; === Basic allocation example (from existing file) ===
(define str-ptr (my-arena 10))
(for ([i (in-range 5)])
  (ptr-set! (ptr-add str-ptr i) _byte (+ (char->integer #\A) i)))
(ptr-set! (ptr-add str-ptr 5) _byte 0) ;; null terminator

(printf "Initial string: ~a\n" (cast str-ptr _pointer _string/utf-8))

;; === Deallocation example ===
(printf "\n=== Low-level deallocation example ===\n")

;; Get stats before deallocation
(define before-stats (arena-get-stats my-arena))
(printf "Before deallocation: Used=~a bytes\n"
        (hash-ref before-stats 'used))

;; Show allocation size
(define alloc-size (get-allocation-size my-arena str-ptr))
(printf "Allocation size of string: ~a bytes\n" alloc-size)

;; Deallocate the string explicitly
(printf "Deallocating string...\n")
(deallocate-from-arena my-arena str-ptr)

;; Get stats after deallocation
(define after-stats (arena-get-stats my-arena))
(printf "After deallocation: Used=~a bytes\n"
        (hash-ref after-stats 'used))

;; === Low-level modification example ===
(printf "\n=== Low-level modification example ===\n")

;; Allocate an int
(define int-ptr (my-arena (ctype-sizeof _int)))
(ptr-set! int-ptr _int 42)
(printf "Initial int value: ~a\n" (ptr-ref int-ptr _int))

;; Modify directly
(ptr-set! int-ptr _int 99)
(printf "Modified int value: ~a\n" (ptr-ref int-ptr _int))

;; Allocate a string and modify it
(define str2-len 15)
(define str2-ptr (my-arena str2-len))

;; Write initial string
(define initial-str "Hello")
(for ([i (in-range (string-length initial-str))])
  (ptr-set! (ptr-add str2-ptr i) _byte (char->integer (string-ref initial-str i))))
(ptr-set! (ptr-add str2-ptr (string-length initial-str)) _byte 0)

(printf "Initial string: ~a\n" (cast str2-ptr _pointer _string/utf-8))

;; Modify the string directly
(define new-str "World!")
(for ([i (in-range (string-length new-str))])
  (ptr-set! (ptr-add str2-ptr i) _byte (char->integer (string-ref new-str i))))
(ptr-set! (ptr-add str2-ptr (string-length new-str)) _byte 0)

(printf "Modified string: ~a\n" (cast str2-ptr _pointer _string/utf-8))

;; Cleanup
(destroy-arena my-arena)
