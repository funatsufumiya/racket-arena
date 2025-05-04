# racket-arena

Arena allocation (memory segment) for Racket.

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

## Acknowledgment

This project was developed with the assistance of coding-assistant AI (Cody).

## API Documentation

see [docs/api.md](docs/api.md)