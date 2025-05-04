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
# or: raco pkg install --deps search-auto --name arena --link $(REPOSITORY_PATH)

# === Run examples for check
$ make check
# or: racket examples/high-layer.rkt

# === optional: Performance-check (memory bench)
$ make performance-check
# or: racket examples/performance-check.rkt
```
