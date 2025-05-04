# racket-arena

Arena allocation (memory segment) for Racket.

## Example

see [examples/high-layer.rkt](https://github.com/funatsufumiya/racket-arena/blob/main/examples/high-layer.rkt)

## Install and test

```bash
# === optional: Build package using cargo (rust)
# $ make build 

# === Install package using raco
$ make install
# or: raco pkg install --deps search-auto --name arena --link .

# === Run examples for check
$ make check
# or: racket examples/high-layer.rkt

# === optional: Performance-check (memory bench)
$ make performance-check
# or: racket examples/performance-check.rkt
```

## Acknowledgment

This project was developed with the assistance of coding-assistant AI (Cody).

## API Documentation

### Arena Management

```racket
;; Create a new arena with specified size in bytes
(make-arena size) → arena?

;; Destroy an arena and free all memory
(destroy-arena arena) → void?

;; Reset arena (free all allocations but keep arena)
(reset-arena arena) → void?

;; Get arena statistics (total, used, free space, block count)
(arena-get-stats arena) → hash?

;; Create a scoped arena that auto-destroys when leaving scope
(with-arena arena-name size
  body ...)
```

### Primitive Types

```racket
;; Allocate primitive values in arena
(arena-int arena value) → procedure?
(arena-double arena value) → procedure?
(arena-float arena value) → procedure?
(arena-char arena value) → procedure?
(arena-bool arena value) → procedure?
(arena-short arena value) → procedure?
(arena-long arena value) → procedure?

;; Update primitive values
(set-arena-int! value-fn new-value) → void?
(set-arena-double! value-fn new-value) → void?
;; ... and so on for other primitive types
```

### Strings

```racket
;; Allocate a string with optional capacity
(arena-string arena str [capacity]) → procedure?

;; Update a string value
(set-arena-string! str-fn new-string) → void?
```

### Arrays

```racket
;; Allocate arrays of primitive types with optional capacity
(arena-int-array arena values [capacity]) → procedure?
(arena-double-array arena values [capacity]) → procedure?
;; ... and so on for other primitive types

;; Array operations
(arena-array-push! array-fn value) → boolean?
(arena-array-pop! array-fn) → any/c
(arena-array-clear! array-fn) → void?
(arena-array->list array-fn) → list?
(arena-array-size array-fn) → exact-nonnegative-integer?
(arena-array-capacity array-fn) → exact-nonnegative-integer?

;; Update array elements
(set-arena-int-array! array-fn index new-value) → void?
(set-arena-double-array! array-fn index new-value) → void?
;; ... and so on for other primitive types
```

### String Arrays

```racket
;; Allocate an array of strings with optional capacities
(arena-string-array arena strings [capacity string-capacity]) → procedure?

;; String array operations
(set-arena-string-array! array-fn index new-string) → void?
(arena-string-array-push! array-fn new-string) → boolean?
(arena-string-array-pop! array-fn) → string?
(arena-string-array-clear! array-fn) → void?
```

### C Structs

```racket
;; Define a C struct type
(define-cstruct _struct_name ([field-name field-type] ...))

;; Allocate a C struct with field types and initial values
(arena-cstruct arena type field-types [init-values]) → procedure?

;; Allocate an array of C structs
(arena-cstruct-array arena type field-types count 
                    [init-func capacity]) → procedure?

;; C struct operations
(set-arena-struct-field! struct-fn field-index new-value) → void?
(set-arena-struct-array-field! struct-array-fn struct-index 
                             field-index new-value) → void?
(arena-cstruct-array-push! array-fn init-values) → boolean?
(arena-cstruct-array-pop! array-fn) → boolean?
(arena-cstruct-array-clear! array-fn) → void?
```

### Low-level API

```racket
;; Direct allocation in arena (returns pointer)
(allocate-in-arena arena size) → cpointer?
;; Shorthand: (arena size)

;; Deallocate specific memory from arena
(deallocate-from-arena arena ptr) → boolean?

;; Deallocate arena value (works with high-level API objects)
(deallocate-arena-value value-fn) → void?

;; Get size of an allocation
(get-allocation-size arena ptr) → exact-nonnegative-integer?
```

### Value Access Pattern

All high-level values follow a consistent access pattern:

```racket
;; For primitives (int, double, etc.)
(define my-int (arena-int my-arena 42))
(my-int)                 ; Get value: 42
(my-int 'ptr)            ; Get pointer to value
(my-int 'arena)          ; Get parent arena

;; For arrays
(define my-array (arena-int-array my-arena '(1 2 3)))
(my-array i)             ; Get value at index i
(my-array 'size)         ; Get current size
(my-array 'capacity)     ; Get capacity
(my-array 'ptr)          ; Get pointer to array data

;; For strings
(define my-string (arena-string my-arena "Hello"))
(my-string)              ; Get string value
(my-string 'length)      ; Get string length
(my-string 'capacity)    ; Get string capacity
(my-string 'ptr)         ; Get pointer to string data
```

See [examples/high-layer.rkt](https://github.com/funatsufumiya/racket-arena/blob/main/examples/high-layer.rkt) and [examples/low-layer.rkt](https://github.com/funatsufumiya/racket-arena/blob/main/examples/low-layer.rkt) for more detailed usage examples.