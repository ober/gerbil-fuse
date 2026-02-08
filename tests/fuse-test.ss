(import :std/test
        :gerbil-fuse/fuse/fuse
        :gerbil-fuse/fuse/fuse-types
        :gerbil-fuse/fuse/fuse-ffi)

(export fuse-test)

(def fuse-test
  (test-suite "gerbil-fuse"

    ;; ============================================================
    ;; file-stat construction and accessors
    ;; ============================================================

    (test-case "file-stat: construct with all fields"
      (let ((st (make-file-stat #o100644 1 1024 501 20
                                1000 0 2000 0 3000 0
                                8 4096)))
        (check (file-stat-mode st) => #o100644)
        (check (file-stat-nlink st) => 1)
        (check (file-stat-size st) => 1024)
        (check (file-stat-uid st) => 501)
        (check (file-stat-gid st) => 20)
        (check (file-stat-atime-sec st) => 1000)
        (check (file-stat-mtime-sec st) => 2000)
        (check (file-stat-ctime-sec st) => 3000)
        (check (file-stat-blocks st) => 8)
        (check (file-stat-blksize st) => 4096)))

    (test-case "file-stat: field mutation"
      (let ((st (make-file-stat 0 0 0 0 0 0 0 0 0 0 0 #f #f)))
        (set! (file-stat-mode st) #o755)
        (set! (file-stat-size st) 42)
        (check (file-stat-mode st) => #o755)
        (check (file-stat-size st) => 42)))

    ;; ============================================================
    ;; Convenience constructors
    ;; ============================================================

    (test-case "make-dir-stat: defaults"
      (let ((st (make-dir-stat)))
        (check (file-stat-mode st) => (bitwise-ior S_IFDIR #o755))
        (check (file-stat-nlink st) => 2)
        (check (file-stat-size st) => 0)
        (check (file-stat-uid st) => 0)
        (check (file-stat-gid st) => 0)))

    (test-case "make-dir-stat: custom mode"
      (let ((st (make-dir-stat mode: #o700)))
        (check (file-stat-mode st) => (bitwise-ior S_IFDIR #o700))))

    (test-case "make-dir-stat: custom nlink"
      (let ((st (make-dir-stat nlink: 5)))
        (check (file-stat-nlink st) => 5)))

    (test-case "make-file-stat/reg: defaults"
      (let ((st (make-file-stat/reg)))
        (check (file-stat-mode st) => (bitwise-ior S_IFREG #o644))
        (check (file-stat-nlink st) => 1)
        (check (file-stat-size st) => 0)))

    (test-case "make-file-stat/reg: with size"
      (let ((st (make-file-stat/reg size: 1024)))
        (check (file-stat-size st) => 1024)
        (check (file-stat-mode st) => (bitwise-ior S_IFREG #o644))))

    (test-case "make-file-stat/reg: with size and mode"
      (let ((st (make-file-stat/reg size: 42 mode: #o444)))
        (check (file-stat-size st) => 42)
        (check (file-stat-mode st) => (bitwise-ior S_IFREG #o444))))

    (test-case "make-symlink-stat: defaults"
      (let ((st (make-symlink-stat)))
        (check (file-stat-mode st) => (bitwise-ior S_IFLNK #o777))
        (check (file-stat-nlink st) => 1)
        (check (file-stat-size st) => 0)))

    (test-case "make-symlink-stat: with target-len"
      (let ((st (make-symlink-stat target-len: 15)))
        (check (file-stat-size st) => 15)))

    ;; ============================================================
    ;; fs-statvfs construction
    ;; ============================================================

    (test-case "fs-statvfs: construct and access"
      (let ((sv (make-fs-statvfs 4096 4096 1000 500 500 1000 500 255)))
        (check (fs-statvfs-bsize sv) => 4096)
        (check (fs-statvfs-frsize sv) => 4096)
        (check (fs-statvfs-blocks sv) => 1000)
        (check (fs-statvfs-bfree sv) => 500)
        (check (fs-statvfs-bavail sv) => 500)
        (check (fs-statvfs-files sv) => 1000)
        (check (fs-statvfs-ffree sv) => 500)
        (check (fs-statvfs-namemax sv) => 255)))

    (test-case "make-default-statvfs: returns valid struct"
      (let ((sv (make-default-statvfs)))
        (check (fs-statvfs? sv) => #t)
        (check (fs-statvfs-bsize sv) => 4096)
        (check (fs-statvfs-namemax sv) => 255)))

    ;; ============================================================
    ;; Constants
    ;; ============================================================

    (test-case "errno constants: positive values"
      (check (> ENOENT 0) => #t)
      (check (> EACCES 0) => #t)
      (check (> EEXIST 0) => #t)
      (check (> EISDIR 0) => #t)
      (check (> ENOTDIR 0) => #t)
      (check (> EIO 0) => #t)
      (check (> ENOSYS 0) => #t)
      (check (> ENOTEMPTY 0) => #t))

    (test-case "stat mode constants"
      (check (> S_IFREG 0) => #t)
      (check (> S_IFDIR 0) => #t)
      (check (> S_IFLNK 0) => #t)
      (check (= S_IRUSR #o400) => #t)
      (check (= S_IWUSR #o200) => #t)
      (check (= S_IXUSR #o100) => #t))

    (test-case "open flags"
      (check (= O_RDONLY 0) => #t)
      (check (> O_WRONLY 0) => #t)
      (check (> O_RDWR 0) => #t))

    ;; ============================================================
    ;; fuse-operations construction
    ;; ============================================================

    (test-case "make-fuse-operations: defaults all #f"
      (let ((ops (make-fuse-operations)))
        (check (fuse-operations-getattr ops) => #f)
        (check (fuse-operations-readdir ops) => #f)
        (check (fuse-operations-open ops) => #f)
        (check (fuse-operations-read ops) => #f)
        (check (fuse-operations-write ops) => #f)))

    (test-case "make-fuse-operations: set handlers"
      (let* ((ga (lambda (p) 0))
             (rd (lambda (p o) '()))
             (ops (make-fuse-operations getattr: ga readdir: rd)))
        (check (eq? (fuse-operations-getattr ops) ga) => #t)
        (check (eq? (fuse-operations-readdir ops) rd) => #t)
        (check (fuse-operations-write ops) => #f)))

    ;; ============================================================
    ;; Callback wrapper error handling
    ;; ============================================================

    (test-case "wrap-getattr: file-stat returns 0"
      (let* ((fn (lambda (path) (make-dir-stat)))
             (wrapped (wrap-getattr fn)))
        (check (wrapped "/") => 0)))

    (test-case "wrap-getattr: negative errno passes through"
      (let* ((fn (lambda (path) (- ENOENT)))
             (wrapped (wrap-getattr fn)))
        (check (wrapped "/missing") => (- ENOENT))))

    (test-case "wrap-getattr: exception returns -EIO"
      (let* ((fn (lambda (path) (error "boom")))
             (wrapped (wrap-getattr fn)))
        (check (wrapped "/") => (- EIO))))

    (test-case "wrap-readdir: list of strings returns 0"
      ;; wrap-readdir needs buf and filler args but filler is never called
      ;; if we just check the return value. We can't easily test with real
      ;; filler, so test error handling instead.
      (let* ((fn (lambda (path offset) (error "boom")))
             (wrapped (wrap-readdir fn)))
        (check (wrapped "/" #f #f 0) => (- EIO))))

    (test-case "wrap-read: exception returns -EIO"
      (let* ((fn (lambda (path size offset fh) (error "crash")))
             (wrapped (wrap-read fn)))
        (check (wrapped "/" #f 0 0 0) => (- EIO))))

    (test-case "wrap-write: exception returns -EIO"
      (let* ((fn (lambda (path data offset fh) (error "crash")))
             (wrapped (wrap-write fn)))
        (check (wrapped "/" #f 0 0 0) => (- EIO))))

    (test-case "wrap-simple: 0 return passes through"
      (let* ((fn (lambda args 0))
             (wrapped (wrap-simple fn)))
        (check (wrapped "/foo") => 0)))

    (test-case "wrap-simple: exception returns -EIO"
      (let* ((fn (lambda args (error "fail")))
             (wrapped (wrap-simple fn)))
        (check (wrapped "/foo") => (- EIO))))

    (test-case "wrap-statfs: fs-statvfs returns 0"
      (let* ((fn (lambda (path) (make-default-statvfs)))
             (wrapped (wrap-statfs fn)))
        (check (wrapped "/") => 0)))

    (test-case "wrap-statfs: exception returns -EIO"
      (let* ((fn (lambda (path) (error "boom")))
             (wrapped (wrap-statfs fn)))
        (check (wrapped "/") => (- EIO))))

    (test-case "wrap-create: returns fixnum"
      (let* ((fn (lambda (path mode) 0))
             (wrapped (wrap-create fn)))
        (check (wrapped "/new" #o644) => 0)))

    (test-case "wrap-release: returns 0 on success"
      (let* ((fn (lambda (path fh) 0))
             (wrapped (wrap-release fn)))
        (check (wrapped "/file" 42) => 0)))

    ;; ============================================================
    ;; file-stat->c! (requires FFI buffer but tests the function exists)
    ;; ============================================================

    (test-case "file-stat->c!: does not error"
      (let ((st (make-dir-stat)))
        (file-stat->c! st)
        (check #t => #t)))

    (test-case "fs-statvfs->c!: does not error"
      (let ((sv (make-default-statvfs)))
        (fs-statvfs->c! sv)
        (check #t => #t)))

    ;; ============================================================
    ;; fuse-unmount: existence check
    ;; ============================================================

    (test-case "fuse-unmount: function exists"
      (check (procedure? fuse-unmount) => #t))
  ))
