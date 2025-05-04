# racket-arena

Arena allocation (memory segment) for Racket.

This maybe useful to avoid specific kind of GC (memory usage) problem.

## Example

see [examples/high-layer.rkt](examples/high-layer.rkt)

(or [examples/low-layer.rkt](examples/low-layer.rkt) for low-layer API)

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

## API Documentation

see [docs/api.md](docs/api.md)

## Known issues

- You probably cannot use string with capacity (dynamic length string) in complexed data structure using cstruct ([Issue #4](https://github.com/funatsufumiya/racket-arena/issues/4)), although you can do simple array or string (see [examples/high-layer.rkt](examples/high-layer.rkt) for detail).
  - We probabaly do not provide string-with-capacity data type (for this complexed usage). You should choose or create it as you like, or initialize with max-length string.
  - This is same for array (complexed array).
- We don't have hash table API yet ([Issue #5](https://github.com/funatsufumiya/racket-arena/issues/5)).
  - We may provide implementation for the most simple case of this, but in the future.

## Acknowledgment

This project was developed with the assistance of coding-assistant AI (Cody).
