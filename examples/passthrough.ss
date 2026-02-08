#!/usr/bin/env gxi
;;; passthrough.ss — Passthrough/mirror FUSE filesystem
;;;
;;; Forwards all operations to an underlying source directory.
;;; Useful as a template for overlay/filter filesystems.
;;;
;;; Usage:
;;;   mkdir -p /tmp/fuse-pass
;;;   gxi examples/passthrough.ss /some/source/dir /tmp/fuse-pass
;;;   ls /tmp/fuse-pass/       # mirrors /some/source/dir/
;;;   cat /tmp/fuse-pass/foo   # reads /some/source/dir/foo
;;;   umount /tmp/fuse-pass

(import :gerbil-fuse/fuse/fuse
        :gerbil-fuse/fuse/fuse-types
        :gerbil-fuse/fuse/fuse-ffi)

;; The source directory to mirror
(def *source* #f)

(def (real-path path)
  "Translate a FUSE path to the real underlying path."
  (if (string=? path "/")
    *source*
    (string-append *source* path)))

;; ============================================================
;; Helpers: call OS and catch errors
;; ============================================================

(def (with-errno thunk)
  "Call thunk, return its value. On Gambit file error, return negative errno."
  (with-catch
    (lambda (e)
      (cond
        ((no-such-file-or-directory-exception? e) (- ENOENT))
        ((permission-denied-exception? e) (- EACCES))
        ((file-exists-exception? e) (- EEXIST))
        (else (- EIO))))
    thunk))

(def (stat->file-stat info)
  "Convert Gambit file-info to our file-stat struct."
  (let* ((type (file-info-type info))
         (mode (file-info-mode info))
         (nlink (file-info-number-of-links info))
         (size (file-info-size info))
         (uid (file-info-owner info))
         (gid (file-info-group info))
         (atime (inexact->exact (floor (time->seconds (file-info-last-access-time info)))))
         (mtime (inexact->exact (floor (time->seconds (file-info-last-modification-time info)))))
         (ctime (inexact->exact (floor (time->seconds (file-info-last-change-time info)))))
         (type-bits (cond
                     ((eq? type 'regular) S_IFREG)
                     ((eq? type 'directory) S_IFDIR)
                     ((eq? type 'symbolic-link) S_IFLNK)
                     ((eq? type 'character-special) S_IFCHR)
                     ((eq? type 'block-special) S_IFBLK)
                     ((eq? type 'fifo) S_IFIFO)
                     ((eq? type 'socket) S_IFSOCK)
                     (else 0))))
    (make-file-stat
      (bitwise-ior type-bits mode) nlink size uid gid
      atime 0 mtime 0 ctime 0 #f #f)))

;; ============================================================
;; FUSE operations
;; ============================================================

(def (pass-getattr path)
  (with-errno
    (lambda ()
      (stat->file-stat (file-info (real-path path) #f)))))

(def (pass-readdir path offset)
  (with-errno
    (lambda ()
      (let ((rpath (real-path path)))
        (cons* "." ".."
          (directory-files rpath))))))

(def (pass-open path flags)
  (let ((rpath (real-path path)))
    (if (file-exists? rpath) 0 (- ENOENT))))

(def (pass-read path size offset fh)
  (with-errno
    (lambda ()
      (let* ((rpath (real-path path))
             (port (open-input-file [path: rpath buffering: #f]))
             (data (make-u8vector size 0)))
        (when (> offset 0)
          (input-port-byte-position port offset))
        (let ((n (read-subu8vector data 0 size port)))
          (close-input-port port)
          (if (and (fixnum? n) (> n 0))
            (if (< n size) (subu8vector data 0 n) data)
            #u8()))))))

(def (pass-write path data offset fh)
  (with-errno
    (lambda ()
      (let* ((rpath (real-path path))
             (port (open-output-file [path: rpath
                                     append: #f
                                     truncate: #f
                                     buffering: #f]))
             (len (u8vector-length data)))
        (output-port-byte-position port offset)
        (write-subu8vector data 0 len port)
        (close-output-port port)
        len))))

(def (pass-create path mode)
  (with-errno
    (lambda ()
      (let ((rpath (real-path path)))
        (close-output-port
          (open-output-file [path: rpath create: #t truncate: #f]))
        0))))

(def (pass-unlink path)
  (with-errno
    (lambda ()
      (delete-file (real-path path))
      0)))

(def (pass-mkdir path mode)
  (with-errno
    (lambda ()
      (create-directory (real-path path))
      0)))

(def (pass-rmdir path)
  (with-errno
    (lambda ()
      (delete-directory (real-path path))
      0)))

(def (pass-rename oldpath newpath flags)
  (with-errno
    (lambda ()
      (rename-file (real-path oldpath) (real-path newpath))
      0)))

(def (pass-truncate path size)
  (with-errno
    (lambda ()
      (let* ((rpath (real-path path))
             (info (file-info rpath))
             (old-size (file-info-size info)))
        (cond
          ;; Shrink: read prefix, rewrite
          ((<= size old-size)
           (let ((data (make-u8vector size 0)))
             (when (> size 0)
               (let ((in (open-input-file [path: rpath buffering: #f])))
                 (read-subu8vector data 0 size in)
                 (close-input-port in)))
             (let ((out (open-output-file [path: rpath truncate: #t buffering: #f])))
               (when (> size 0)
                 (write-subu8vector data 0 size out))
               (close-output-port out))))
          ;; Extend: append zeros
          (else
           (let ((out (open-output-file [path: rpath append: #t buffering: #f]))
                 (zeros (make-u8vector (- size old-size) 0)))
             (write-subu8vector zeros 0 (u8vector-length zeros) out)
             (close-output-port out))))
        0))))

(def (pass-access path mask)
  (if (file-exists? (real-path path)) 0 (- ENOENT)))

(def (pass-release path fh) 0)

(def (pass-statfs path)
  (make-default-statvfs))

;; ============================================================
;; Build ops and run
;; ============================================================

(def pass-ops
  (make-fuse-operations
    getattr:  pass-getattr
    readdir:  pass-readdir
    open:     pass-open
    read:     pass-read
    write:    pass-write
    create:   pass-create
    unlink:   pass-unlink
    mkdir:    pass-mkdir
    rmdir:    pass-rmdir
    rename:   pass-rename
    truncate: pass-truncate
    access:   pass-access
    release:  pass-release
    statfs:   pass-statfs))

(def (main . args)
  (when (< (length args) 2)
    (displayln "Usage: passthrough <source-dir> <mountpoint>")
    (exit 1))
  (let ((source (car args))
        (mountpoint (cadr args)))
    (unless (file-exists? source)
      (displayln "Error: source directory does not exist: " source)
      (exit 1))
    (set! *source* source)
    (displayln "Mounting passthrough of " source " on " mountpoint)
    (displayln "Press Ctrl+C or run 'umount " mountpoint "' to unmount")
    (fuse-run pass-ops mountpoint)))
