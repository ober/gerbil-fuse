(export #t)
(import :gerbil-fuse/fuse/fuse-ffi
        :gerbil-fuse/fuse/fuse-types
        :std/sugar)

;; ============================================================
;; fuse-operations: the filesystem protocol
;; ============================================================

(defclass fuse-operations
  (getattr readdir open read write release
   create unlink mkdir rmdir rename truncate
   chmod chown utimens flush fsync
   opendir releasedir fsyncdir statfs access
   readlink symlink link mknod
   setxattr getxattr listxattr removexattr
   lock flock fallocate lseek
   init destroy)
  transparent: #t
  constructor: :init!)

(defmethod {:init! fuse-operations}
  (lambda (self
      getattr: (getattr #f) readdir: (readdir #f)
      open: (open #f) read: (read #f) write: (write #f)
      release: (release #f) create: (create #f)
      unlink: (unlink #f) mkdir: (mkdir #f) rmdir: (rmdir #f)
      rename: (rename #f) truncate: (truncate #f)
      chmod: (chmod #f) chown: (chown #f) utimens: (utimens #f)
      flush: (flush #f) fsync: (fsync #f)
      opendir: (opendir #f) releasedir: (releasedir #f)
      fsyncdir: (fsyncdir #f) statfs: (statfs #f)
      access: (access #f) readlink: (readlink #f)
      symlink: (symlink #f) link: (link #f) mknod: (mknod #f)
      setxattr: (setxattr #f) getxattr: (getxattr #f)
      listxattr: (listxattr #f) removexattr: (removexattr #f)
      lock: (lock #f) flock: (flock #f)
      fallocate: (fallocate #f) lseek: (lseek #f)
      init: (init #f) destroy: (destroy #f))
    (set! (fuse-operations-getattr self) getattr)
    (set! (fuse-operations-readdir self) readdir)
    (set! (fuse-operations-open self) open)
    (set! (fuse-operations-read self) read)
    (set! (fuse-operations-write self) write)
    (set! (fuse-operations-release self) release)
    (set! (fuse-operations-create self) create)
    (set! (fuse-operations-unlink self) unlink)
    (set! (fuse-operations-mkdir self) mkdir)
    (set! (fuse-operations-rmdir self) rmdir)
    (set! (fuse-operations-rename self) rename)
    (set! (fuse-operations-truncate self) truncate)
    (set! (fuse-operations-chmod self) chmod)
    (set! (fuse-operations-chown self) chown)
    (set! (fuse-operations-utimens self) utimens)
    (set! (fuse-operations-flush self) flush)
    (set! (fuse-operations-fsync self) fsync)
    (set! (fuse-operations-opendir self) opendir)
    (set! (fuse-operations-releasedir self) releasedir)
    (set! (fuse-operations-fsyncdir self) fsyncdir)
    (set! (fuse-operations-statfs self) statfs)
    (set! (fuse-operations-access self) access)
    (set! (fuse-operations-readlink self) readlink)
    (set! (fuse-operations-symlink self) symlink)
    (set! (fuse-operations-link self) link)
    (set! (fuse-operations-mknod self) mknod)
    (set! (fuse-operations-setxattr self) setxattr)
    (set! (fuse-operations-getxattr self) getxattr)
    (set! (fuse-operations-listxattr self) listxattr)
    (set! (fuse-operations-removexattr self) removexattr)
    (set! (fuse-operations-lock self) lock)
    (set! (fuse-operations-flock self) flock)
    (set! (fuse-operations-fallocate self) fallocate)
    (set! (fuse-operations-lseek self) lseek)
    (set! (fuse-operations-init self) init)
    (set! (fuse-operations-destroy self) destroy)))

;; ============================================================
;; Callback wrappers
;;
;; Each wrapper translates between the Scheme API conventions
;; (returning Scheme values) and the raw FFI (filling C buffers,
;; returning errno codes). All wrappers catch exceptions and
;; return -EIO as a safe default.
;; ============================================================

(def (wrap-getattr user-fn)
  (lambda (path)
    (try
      (let (result (user-fn path))
        (cond
          ((file-stat? result)
           (file-stat->c! result) 0)
          ((fixnum? result) result)
          (else (- ENOSYS))))
      (catch (e) (- EIO)))))

(def (wrap-readdir user-fn)
  (lambda (path buf filler offset)
    (try
      (let (entries (user-fn path offset))
        (cond
          ((list? entries)
           (for-each (lambda (name) (ffi-fill-dir filler buf name 0)) entries)
           0)
          ((fixnum? entries) entries)
          (else (- ENOSYS))))
      (catch (e) (- EIO)))))

(def (wrap-open user-fn)
  (lambda (path flags)
    (try
      (let (result (user-fn path flags))
        (if (fixnum? result) result (- ENOSYS)))
      (catch (e) (- EIO)))))

(def (wrap-read user-fn)
  (lambda (path buf size offset fh)
    (try
      (let (result (user-fn path size offset fh))
        (cond
          ((u8vector? result)
           (let (len (min (u8vector-length result) size))
             (when (> len 0)
               (ffi-copy-to-buffer! buf result 0 len))
             len))
          ((fixnum? result) result)
          (else (- EIO))))
      (catch (e) (- EIO)))))

(def (wrap-write user-fn)
  (lambda (path buf size offset fh)
    (try
      (let (data (ffi-buffer-to-u8vector buf size))
        (let (result (user-fn path data offset fh))
          (if (fixnum? result) result (- EIO))))
      (catch (e) (- EIO)))))

(def (wrap-release user-fn)
  (lambda (path fh)
    (try
      (let (result (user-fn path fh))
        (if (fixnum? result) result 0))
      (catch (e) (- EIO)))))

(def (wrap-create user-fn)
  (lambda (path mode)
    (try
      (let (result (user-fn path mode))
        (if (fixnum? result) result (- ENOSYS)))
      (catch (e) (- EIO)))))

(def (wrap-simple user-fn)
  "Wrap a simple callback that returns 0 or negative errno."
  (lambda args
    (try
      (let (result (apply user-fn args))
        (if (fixnum? result) result 0))
      (catch (e) (- EIO)))))

(def (wrap-statfs user-fn)
  (lambda (path)
    (try
      (let (result (user-fn path))
        (cond
          ((fs-statvfs? result)
           (fs-statvfs->c! result) 0)
          ((fixnum? result) result)
          (else (- ENOSYS))))
      (catch (e) (- EIO)))))

(def (wrap-readlink user-fn)
  "Wrap readlink: user returns target string or negative errno."
  (lambda (path)
    (try
      (let (result (user-fn path))
        (cond
          ((string? result)
           (let (bytes (string->bytes result))
             (ffi-readlink-set! result (string-length result)))
           0)
          ((fixnum? result) result)
          (else (- ENOSYS))))
      (catch (e) (- EIO)))))

(def (wrap-getxattr user-fn)
  "Wrap getxattr: user returns u8vector or negative errno.
   If size=0, return the data length. Otherwise copy data."
  (lambda (path name value size)
    (try
      (let (result (user-fn path name))
        (cond
          ((u8vector? result)
           (let (len (u8vector-length result))
             (if (= size 0)
               len
               (if (> len size)
                 (- ERANGE)
                 (begin
                   (ffi-xattr-set-result! result len)
                   len)))))
          ((fixnum? result) result)
          (else (- ENOSYS))))
      (catch (e) (- EIO)))))

(def (wrap-listxattr user-fn)
  "Wrap listxattr: user returns list of name strings or negative errno.
   Names are packed as null-terminated strings."
  (lambda (path list size)
    (try
      (let (result (user-fn path))
        (cond
          ((list? result)
           (let* ((packed (apply string-append
                    (map (lambda (s) (string-append s "\0")) result)))
                  (bytes (string->bytes packed))
                  (len (u8vector-length bytes)))
             (if (= size 0)
               len
               (if (> len size)
                 (- ERANGE)
                 (begin
                   (ffi-xattr-set-result! bytes len)
                   len)))))
          ((fixnum? result) result)
          (else (- ENOSYS))))
      (catch (e) (- EIO)))))

(def (wrap-setxattr user-fn)
  "Wrap setxattr: converts C buffer to u8vector before calling user."
  (lambda (path name value size flags)
    (try
      (let (data (ffi-buffer-to-u8vector value size))
        (let (result (user-fn path name data flags))
          (if (fixnum? result) result 0)))
      (catch (e) (- EIO)))))

(def (wrap-init user-fn)
  (lambda () (try (user-fn) (catch (e) (void)))))

(def (wrap-destroy user-fn)
  (lambda () (try (user-fn) (catch (e) (void)))))

;; ============================================================
;; fuse-run: main entry point
;; ============================================================

(def (fuse-run ops mountpoint
     foreground: (foreground #t)
     debug: (debug #f)
     allow-other: (allow-other #f)
     options: (options []))
  "Mount and run a FUSE filesystem. Blocks until unmounted.
   ops: a fuse-operations instance
   mountpoint: path to mount directory
   foreground: run in foreground (default #t)
   debug: enable FUSE debug output (default #f)
   allow-other: allow other users to access (default #f)
   options: extra FUSE mount options as list of strings"

  ;; 1. Reset ops and build dispatch table
  (ffi-ops-reset!)
  (let ((dispatch (make-hash-table)))

    ;; Register each operation that has a handler
    (def (register! name accessor wrapper setter)
      (let (fn (accessor ops))
        (when fn
          (hash-put! dispatch name (wrapper fn))
          (setter))))

    ;; Core ops
    (register! 'getattr fuse-operations-getattr wrap-getattr ffi-ops-set-getattr!)
    (register! 'readdir fuse-operations-readdir wrap-readdir ffi-ops-set-readdir!)
    (register! 'open    fuse-operations-open    wrap-open    ffi-ops-set-open!)
    (register! 'read    fuse-operations-read    wrap-read    ffi-ops-set-read!)
    (register! 'release fuse-operations-release wrap-release ffi-ops-set-release!)
    (register! 'init    fuse-operations-init    wrap-init    ffi-ops-set-init!)
    (register! 'destroy fuse-operations-destroy wrap-destroy ffi-ops-set-destroy!)

    ;; Read-write ops
    (register! 'write    fuse-operations-write    wrap-write  ffi-ops-set-write!)
    (register! 'create   fuse-operations-create   wrap-create ffi-ops-set-create!)
    (register! 'unlink   fuse-operations-unlink   wrap-simple ffi-ops-set-unlink!)
    (register! 'mkdir    fuse-operations-mkdir    wrap-simple ffi-ops-set-mkdir!)
    (register! 'rmdir    fuse-operations-rmdir    wrap-simple ffi-ops-set-rmdir!)
    (register! 'rename   fuse-operations-rename   wrap-simple ffi-ops-set-rename!)
    (register! 'truncate fuse-operations-truncate wrap-simple ffi-ops-set-truncate!)
    (register! 'chmod    fuse-operations-chmod    wrap-simple ffi-ops-set-chmod!)
    (register! 'chown    fuse-operations-chown    wrap-simple ffi-ops-set-chown!)
    (register! 'utimens  fuse-operations-utimens  wrap-simple ffi-ops-set-utimens!)
    (register! 'flush    fuse-operations-flush    wrap-simple ffi-ops-set-flush!)
    (register! 'fsync    fuse-operations-fsync    wrap-simple ffi-ops-set-fsync!)

    ;; Extended ops
    (register! 'opendir    fuse-operations-opendir    wrap-simple   ffi-ops-set-opendir!)
    (register! 'releasedir fuse-operations-releasedir wrap-simple   ffi-ops-set-releasedir!)
    (register! 'fsyncdir   fuse-operations-fsyncdir   wrap-simple   ffi-ops-set-fsyncdir!)
    (register! 'statfs     fuse-operations-statfs     wrap-statfs   ffi-ops-set-statfs!)
    (register! 'access     fuse-operations-access     wrap-simple   ffi-ops-set-access!)
    (register! 'readlink   fuse-operations-readlink   wrap-readlink ffi-ops-set-readlink!)
    (register! 'symlink    fuse-operations-symlink    wrap-simple   ffi-ops-set-symlink!)
    (register! 'link       fuse-operations-link       wrap-simple   ffi-ops-set-link!)
    (register! 'mknod      fuse-operations-mknod      wrap-simple   ffi-ops-set-mknod!)

    ;; Advanced ops
    (register! 'setxattr    fuse-operations-setxattr    wrap-setxattr   ffi-ops-set-setxattr!)
    (register! 'getxattr    fuse-operations-getxattr    wrap-getxattr   ffi-ops-set-getxattr!)
    (register! 'listxattr   fuse-operations-listxattr   wrap-listxattr  ffi-ops-set-listxattr!)
    (register! 'removexattr fuse-operations-removexattr wrap-simple     ffi-ops-set-removexattr!)
    (register! 'lock        fuse-operations-lock        wrap-simple     ffi-ops-set-lock!)
    (register! 'flock       fuse-operations-flock       wrap-simple     ffi-ops-set-flock!)
    (register! 'fallocate   fuse-operations-fallocate   wrap-simple     ffi-ops-set-fallocate!)
    (register! 'lseek       fuse-operations-lseek       wrap-simple     ffi-ops-set-lseek!)

    ;; Install the dispatch table
    (set-fuse-dispatch! dispatch))

  ;; 2. Build argv for fuse_main
  (let ((argv-list (append
                    ["gerbil-fuse" "-s"]  ;; -s = single-threaded (required)
                    (if foreground ["-f"] [])
                    (if debug ["-d"] [])
                    (if allow-other ["-o" "allow_other"] [])
                    (foldr (lambda (o acc) (cons* "-o" o acc)) [] options)
                    [mountpoint])))
    ;; 3. Call fuse_main (blocks until unmount)
    (ffi-fuse-main argv-list)))

;; ============================================================
;; fuse-unmount: unmount a FUSE filesystem
;; ============================================================

(def (fuse-unmount mountpoint)
  "Unmount a FUSE filesystem."
  (let ((cmd (if (equal? (cadr (system-type)) 'apple)
               (string-append "umount " mountpoint)
               (string-append "fusermount3 -u " mountpoint))))
    (##shell-command cmd)))
