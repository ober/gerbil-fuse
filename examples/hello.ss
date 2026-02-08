#!/usr/bin/env gxi
;;; hello.ss — Minimal read-only FUSE filesystem
;;;
;;; Exposes a single file /hello.txt with a greeting.
;;;
;;; Usage:
;;;   mkdir -p /tmp/fuse-hello
;;;   gxi examples/hello.ss /tmp/fuse-hello
;;;   cat /tmp/fuse-hello/hello.txt
;;;   ls -la /tmp/fuse-hello/
;;;   umount /tmp/fuse-hello

(import :gerbil-fuse/fuse/fuse
        :gerbil-fuse/fuse/fuse-types
        :gerbil-fuse/fuse/fuse-ffi)

(def hello-content (string->bytes "Hello, FUSE from Gerbil Scheme!\n"))

(def hello-ops
  (make-fuse-operations
    getattr: (lambda (path)
      (cond
        ((string=? path "/")
         (make-dir-stat))
        ((string=? path "/hello.txt")
         (make-file-stat/reg size: (u8vector-length hello-content)))
        (else (- ENOENT))))

    readdir: (lambda (path offset)
      (if (string=? path "/")
        '("." ".." "hello.txt")
        (- ENOENT)))

    open: (lambda (path flags)
      (if (string=? path "/hello.txt") 0 (- ENOENT)))

    read: (lambda (path size offset fh)
      (if (string=? path "/hello.txt")
        (let* ((len (u8vector-length hello-content))
               (end (min (+ offset size) len)))
          (if (>= offset len)
            #u8()
            (subu8vector hello-content offset end)))
        (- ENOENT)))))

(def (main . args)
  (when (null? args)
    (displayln "Usage: hello <mountpoint>")
    (exit 1))
  (displayln "Mounting hello filesystem on " (car args))
  (displayln "Press Ctrl+C or run 'umount " (car args) "' to unmount")
  (fuse-run hello-ops (car args)))
