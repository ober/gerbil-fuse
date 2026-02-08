(export #t)
(import :std/foreign)

(begin-ffi (
  ;; Errno constants
  ENOENT EACCES EEXIST EISDIR ENOTDIR EINVAL ENOSYS ENOTEMPTY
  ENOSPC EIO EPERM ENOMEM EBADF EROFS ENOTSUP ERANGE ENODATA EXDEV

  ;; Stat mode constants
  S_IFMT S_IFREG S_IFDIR S_IFLNK S_IFCHR S_IFBLK S_IFIFO S_IFSOCK
  S_IRUSR S_IWUSR S_IXUSR S_IRGRP S_IWGRP S_IXGRP S_IROTH S_IWOTH S_IXOTH
  S_ISUID S_ISGID S_ISVTX

  ;; Open flags
  O_RDONLY O_WRONLY O_RDWR O_CREAT O_EXCL O_TRUNC O_APPEND

  ;; Stat buffer setters
  ffi-stat-reset! ffi-stat-set-mode! ffi-stat-set-nlink! ffi-stat-set-size!
  ffi-stat-set-uid! ffi-stat-set-gid!
  ffi-stat-set-atime! ffi-stat-set-mtime! ffi-stat-set-ctime!
  ffi-stat-set-blocks! ffi-stat-set-blksize!

  ;; Statvfs setters
  ffi-statvfs-reset! ffi-statvfs-set-bsize! ffi-statvfs-set-frsize!
  ffi-statvfs-set-blocks!
  ffi-statvfs-set-bfree! ffi-statvfs-set-bavail!
  ffi-statvfs-set-files! ffi-statvfs-set-ffree! ffi-statvfs-set-namemax!

  ;; Buffer helpers
  ffi-copy-to-buffer! ffi-buffer-to-u8vector
  ffi-readlink-set!
  ffi-xattr-set-result! ffi-xattr-get-len
  ffi-lseek-set-result!

  ;; fuse_fill_dir wrapper
  ffi-fill-dir

  ;; fuse_context accessors
  ffi-context-uid ffi-context-gid ffi-context-pid

  ;; Ops registration
  ffi-ops-reset!
  ffi-ops-set-getattr! ffi-ops-set-readdir! ffi-ops-set-open!
  ffi-ops-set-read! ffi-ops-set-write! ffi-ops-set-release!
  ffi-ops-set-create! ffi-ops-set-unlink! ffi-ops-set-mkdir!
  ffi-ops-set-rmdir! ffi-ops-set-rename! ffi-ops-set-truncate!
  ffi-ops-set-chmod! ffi-ops-set-chown! ffi-ops-set-utimens!
  ffi-ops-set-flush! ffi-ops-set-fsync! ffi-ops-set-opendir!
  ffi-ops-set-releasedir! ffi-ops-set-statfs! ffi-ops-set-access!
  ffi-ops-set-readlink! ffi-ops-set-symlink! ffi-ops-set-link!
  ffi-ops-set-mknod! ffi-ops-set-init! ffi-ops-set-destroy!
  ffi-ops-set-setxattr! ffi-ops-set-getxattr!
  ffi-ops-set-listxattr! ffi-ops-set-removexattr!
  ffi-ops-set-lock! ffi-ops-set-flock! ffi-ops-set-fallocate!
  ffi-ops-set-lseek! ffi-ops-set-fsyncdir!

  ;; Main entry
  ffi-fuse-main

  ;; Dispatch table
  *fuse-dispatch* set-fuse-dispatch!
  )

  ;; ============================================================
  ;; Include the C shim
  ;; ============================================================
  (c-declare #<<END-C
#ifndef ___HAVE_FFI_U8VECTOR
#define ___HAVE_FFI_U8VECTOR
#define U8_DATA(obj) ___CAST (___U8*, ___BODY_AS (obj, ___tSUBTYPED))
#define U8_LEN(obj) ___HD_BYTES (___HEADER (obj))
#endif

#include "shim.c"

/* No-op free for borrowed pointers */
___SCMOBJ ffi_nofree(void *p) { return ___FIX(___NO_ERR); }
END-C
  )

  ;; ============================================================
  ;; Constants
  ;; ============================================================

  ;; Errno values
  (define-const ENOENT)
  (define-const EACCES)
  (define-const EEXIST)
  (define-const EISDIR)
  (define-const ENOTDIR)
  (define-const EINVAL)
  (define-const ENOSYS)
  (define-const ENOTEMPTY)
  (define-const ENOSPC)
  (define-const EIO)
  (define-const EPERM)
  (define-const ENOMEM)
  (define-const EBADF)
  (define-const EROFS)
  (define-const ENOTSUP)
  (define-const ERANGE)
  (define-const ENODATA)
  (define-const EXDEV)

  ;; Stat mode type bits
  (define-const S_IFMT)
  (define-const S_IFREG)
  (define-const S_IFDIR)
  (define-const S_IFLNK)
  (define-const S_IFCHR)
  (define-const S_IFBLK)
  (define-const S_IFIFO)
  (define-const S_IFSOCK)

  ;; Permission bits
  (define-const S_IRUSR)
  (define-const S_IWUSR)
  (define-const S_IXUSR)
  (define-const S_IRGRP)
  (define-const S_IWGRP)
  (define-const S_IXGRP)
  (define-const S_IROTH)
  (define-const S_IWOTH)
  (define-const S_IXOTH)
  (define-const S_ISUID)
  (define-const S_ISGID)
  (define-const S_ISVTX)

  ;; Open flags
  (define-const O_RDONLY)
  (define-const O_WRONLY)
  (define-const O_RDWR)
  (define-const O_CREAT)
  (define-const O_EXCL)
  (define-const O_TRUNC)
  (define-const O_APPEND)

  ;; ============================================================
  ;; Stat buffer setters
  ;; ============================================================

  (define-c-lambda ffi-stat-reset! () void "ffi_stat_reset")
  (define-c-lambda ffi-stat-set-mode! (int) void "ffi_stat_set_mode")
  (define-c-lambda ffi-stat-set-nlink! (int) void "ffi_stat_set_nlink")
  (define-c-lambda ffi-stat-set-size! (int64) void "ffi_stat_set_size")
  (define-c-lambda ffi-stat-set-uid! (int) void "ffi_stat_set_uid")
  (define-c-lambda ffi-stat-set-gid! (int) void "ffi_stat_set_gid")
  (define-c-lambda ffi-stat-set-atime! (long long) void "ffi_stat_set_atime")
  (define-c-lambda ffi-stat-set-mtime! (long long) void "ffi_stat_set_mtime")
  (define-c-lambda ffi-stat-set-ctime! (long long) void "ffi_stat_set_ctime")
  (define-c-lambda ffi-stat-set-blocks! (long) void "ffi_stat_set_blocks")
  (define-c-lambda ffi-stat-set-blksize! (long) void "ffi_stat_set_blksize")

  ;; ============================================================
  ;; Statvfs buffer setters
  ;; ============================================================

  (define-c-lambda ffi-statvfs-reset! () void "ffi_statvfs_reset")
  (define-c-lambda ffi-statvfs-set-bsize! (unsigned-long) void "ffi_statvfs_set_bsize")
  (define-c-lambda ffi-statvfs-set-frsize! (unsigned-long) void "ffi_statvfs_set_frsize")
  (define-c-lambda ffi-statvfs-set-blocks! (unsigned-long) void "ffi_statvfs_set_blocks")
  (define-c-lambda ffi-statvfs-set-bfree! (unsigned-long) void "ffi_statvfs_set_bfree")
  (define-c-lambda ffi-statvfs-set-bavail! (unsigned-long) void "ffi_statvfs_set_bavail")
  (define-c-lambda ffi-statvfs-set-files! (unsigned-long) void "ffi_statvfs_set_files")
  (define-c-lambda ffi-statvfs-set-ffree! (unsigned-long) void "ffi_statvfs_set_ffree")
  (define-c-lambda ffi-statvfs-set-namemax! (unsigned-long) void "ffi_statvfs_set_namemax")

  ;; ============================================================
  ;; Buffer helpers
  ;; ============================================================

  ;; Copy from Scheme u8vector into a C buffer
  (define-c-lambda ffi-copy-to-buffer!
    ((pointer void) scheme-object size_t size_t) void
    "memcpy(___arg1, U8_DATA(___arg2) + ___arg3, ___arg4);")

  ;; Copy from C buffer into a new Scheme u8vector
  (define-c-lambda ffi-buffer-to-u8vector
    ((pointer void) size_t) scheme-object
    #<<END-C
    ___SCMOBJ result;
    ___U8 *src = (___U8 *)___arg1;
    size_t len = ___arg2;
    result = ___EXT(___alloc_scmobj)(___PSTATE, ___sU8VECTOR, len);
    if (!___FIXNUMP(result)) {
        memcpy(U8_DATA(result), src, len);
    }
    ___return(result);
END-C
  )

  ;; Readlink result setter
  (define-c-lambda ffi-readlink-set! (UTF-8-string size_t) void "ffi_readlink_set")

  ;; Xattr result helpers
  (define-c-lambda ffi-xattr-set-result! ((pointer void) size_t) void "ffi_xattr_set_result")
  (define-c-lambda ffi-xattr-get-len () size_t "ffi_xattr_get_len")

  ;; Lseek result setter
  (define-c-lambda ffi-lseek-set-result! (int64) void "ffi_lseek_set_result")

  ;; ============================================================
  ;; fuse_fill_dir wrapper
  ;; ============================================================

  ;; Calls the fuse_fill_dir_t function pointer.
  ;; filler is passed as (pointer void), buf as (pointer void).
  ;; FUSE 2: filler(buf, name, NULL, 0) - 4 args
  ;; FUSE 3: filler(buf, name, NULL, 0, 0) - 5 args
  (define-c-lambda ffi-fill-dir
    ((pointer void) (pointer void) char-string int)
    int
    #<<END-C
#if FUSE_MAJOR_VERSION >= 3
    ___return(((fuse_fill_dir_t)___arg1)(___arg2, ___arg3, NULL, 0, 0));
#else
    ___return(((fuse_fill_dir_t)___arg1)(___arg2, ___arg3, NULL, 0));
#endif
END-C
  )

  ;; ============================================================
  ;; fuse_context accessors
  ;; ============================================================

  (define-c-lambda ffi-context-uid () int
    "___return(fuse_get_context()->uid);")
  (define-c-lambda ffi-context-gid () int
    "___return(fuse_get_context()->gid);")
  (define-c-lambda ffi-context-pid () int
    "___return(fuse_get_context()->pid);")

  ;; ============================================================
  ;; Dispatch table
  ;; ============================================================

  (define *fuse-dispatch* (make-hash-table))
  (define (set-fuse-dispatch! ops) (set! *fuse-dispatch* ops))

  ;; ============================================================
  ;; c-define callbacks — Scheme functions callable from C
  ;; ============================================================

  ;; --- Core operations ---

  (c-define (scheme_getattr path)
            (char-string) int "scheme_getattr" ""
    (let ((handler (hash-ref *fuse-dispatch* 'getattr #f)))
      (if handler (handler path) (- ENOSYS))))

  (c-define (scheme_readdir path buf filler offset)
            (char-string (pointer void) (pointer void) long) int
            "scheme_readdir" ""
    (let ((handler (hash-ref *fuse-dispatch* 'readdir #f)))
      (if handler (handler path buf filler offset) (- ENOSYS))))

  (c-define (scheme_open path flags)
            (char-string int) int "scheme_open" ""
    (let ((handler (hash-ref *fuse-dispatch* 'open #f)))
      (if handler (handler path flags) (- ENOSYS))))

  (c-define (scheme_read path buf size offset fh)
            (char-string (pointer void) size_t long unsigned-int64) int
            "scheme_read" ""
    (let ((handler (hash-ref *fuse-dispatch* 'read #f)))
      (if handler (handler path buf size offset fh) (- ENOSYS))))

  (c-define (scheme_release path fh)
            (char-string unsigned-int64) int "scheme_release" ""
    (let ((handler (hash-ref *fuse-dispatch* 'release #f)))
      (if handler (handler path fh) (- ENOSYS))))

  (c-define (scheme_init)
            () void "scheme_init" ""
    (let ((handler (hash-ref *fuse-dispatch* 'init #f)))
      (when handler (handler))))

  (c-define (scheme_destroy)
            () void "scheme_destroy" ""
    (let ((handler (hash-ref *fuse-dispatch* 'destroy #f)))
      (when handler (handler))))

  ;; --- Read-write operations ---

  (c-define (scheme_write path buf size offset fh)
            (char-string (pointer void) size_t long unsigned-int64) int
            "scheme_write" ""
    (let ((handler (hash-ref *fuse-dispatch* 'write #f)))
      (if handler (handler path buf size offset fh) (- ENOSYS))))

  (c-define (scheme_create path mode)
            (char-string int) int "scheme_create" ""
    (let ((handler (hash-ref *fuse-dispatch* 'create #f)))
      (if handler (handler path mode) (- ENOSYS))))

  (c-define (scheme_unlink path)
            (char-string) int "scheme_unlink" ""
    (let ((handler (hash-ref *fuse-dispatch* 'unlink #f)))
      (if handler (handler path) (- ENOSYS))))

  (c-define (scheme_mkdir path mode)
            (char-string int) int "scheme_mkdir" ""
    (let ((handler (hash-ref *fuse-dispatch* 'mkdir #f)))
      (if handler (handler path mode) (- ENOSYS))))

  (c-define (scheme_rmdir path)
            (char-string) int "scheme_rmdir" ""
    (let ((handler (hash-ref *fuse-dispatch* 'rmdir #f)))
      (if handler (handler path) (- ENOSYS))))

  (c-define (scheme_rename oldpath newpath flags)
            (char-string char-string unsigned-int) int "scheme_rename" ""
    (let ((handler (hash-ref *fuse-dispatch* 'rename #f)))
      (if handler (handler oldpath newpath flags) (- ENOSYS))))

  (c-define (scheme_truncate path size)
            (char-string int64) int "scheme_truncate" ""
    (let ((handler (hash-ref *fuse-dispatch* 'truncate #f)))
      (if handler (handler path size) (- ENOSYS))))

  (c-define (scheme_chmod path mode)
            (char-string int) int "scheme_chmod" ""
    (let ((handler (hash-ref *fuse-dispatch* 'chmod #f)))
      (if handler (handler path mode) (- ENOSYS))))

  (c-define (scheme_chown path uid gid)
            (char-string int int) int "scheme_chown" ""
    (let ((handler (hash-ref *fuse-dispatch* 'chown #f)))
      (if handler (handler path uid gid) (- ENOSYS))))

  (c-define (scheme_utimens path atime_sec atime_nsec mtime_sec mtime_nsec)
            (char-string long long long long) int "scheme_utimens" ""
    (let ((handler (hash-ref *fuse-dispatch* 'utimens #f)))
      (if handler (handler path atime_sec atime_nsec mtime_sec mtime_nsec) (- ENOSYS))))

  (c-define (scheme_flush path fh)
            (char-string unsigned-int64) int "scheme_flush" ""
    (let ((handler (hash-ref *fuse-dispatch* 'flush #f)))
      (if handler (handler path fh) (- ENOSYS))))

  (c-define (scheme_fsync path datasync fh)
            (char-string int unsigned-int64) int "scheme_fsync" ""
    (let ((handler (hash-ref *fuse-dispatch* 'fsync #f)))
      (if handler (handler path datasync fh) (- ENOSYS))))

  ;; --- Extended operations ---

  (c-define (scheme_opendir path)
            (char-string) int "scheme_opendir" ""
    (let ((handler (hash-ref *fuse-dispatch* 'opendir #f)))
      (if handler (handler path) (- ENOSYS))))

  (c-define (scheme_releasedir path)
            (char-string) int "scheme_releasedir" ""
    (let ((handler (hash-ref *fuse-dispatch* 'releasedir #f)))
      (if handler (handler path) (- ENOSYS))))

  (c-define (scheme_fsyncdir path datasync)
            (char-string int) int "scheme_fsyncdir" ""
    (let ((handler (hash-ref *fuse-dispatch* 'fsyncdir #f)))
      (if handler (handler path datasync) (- ENOSYS))))

  (c-define (scheme_statfs path)
            (char-string) int "scheme_statfs" ""
    (let ((handler (hash-ref *fuse-dispatch* 'statfs #f)))
      (if handler (handler path) (- ENOSYS))))

  (c-define (scheme_access path mask)
            (char-string int) int "scheme_access" ""
    (let ((handler (hash-ref *fuse-dispatch* 'access #f)))
      (if handler (handler path mask) (- ENOSYS))))

  (c-define (scheme_readlink path buf size)
            (char-string (pointer void) size_t) int "scheme_readlink" ""
    (let ((handler (hash-ref *fuse-dispatch* 'readlink #f)))
      (if handler (handler path) (- ENOSYS))))

  (c-define (scheme_symlink target linkpath)
            (char-string char-string) int "scheme_symlink" ""
    (let ((handler (hash-ref *fuse-dispatch* 'symlink #f)))
      (if handler (handler target linkpath) (- ENOSYS))))

  (c-define (scheme_link oldpath newpath)
            (char-string char-string) int "scheme_link" ""
    (let ((handler (hash-ref *fuse-dispatch* 'link #f)))
      (if handler (handler oldpath newpath) (- ENOSYS))))

  (c-define (scheme_mknod path mode dev)
            (char-string int unsigned-long) int "scheme_mknod" ""
    (let ((handler (hash-ref *fuse-dispatch* 'mknod #f)))
      (if handler (handler path mode dev) (- ENOSYS))))

  ;; --- Advanced operations ---

  (c-define (scheme_setxattr path name value size flags)
            (char-string char-string (pointer void) size_t int) int
            "scheme_setxattr" ""
    (let ((handler (hash-ref *fuse-dispatch* 'setxattr #f)))
      (if handler (handler path name value size flags) (- ENOSYS))))

  (c-define (scheme_getxattr path name value size)
            (char-string char-string (pointer void) size_t) int
            "scheme_getxattr" ""
    (let ((handler (hash-ref *fuse-dispatch* 'getxattr #f)))
      (if handler (handler path name value size) (- ENOSYS))))

  (c-define (scheme_listxattr path list size)
            (char-string (pointer void) size_t) int
            "scheme_listxattr" ""
    (let ((handler (hash-ref *fuse-dispatch* 'listxattr #f)))
      (if handler (handler path list size) (- ENOSYS))))

  (c-define (scheme_removexattr path name)
            (char-string char-string) int "scheme_removexattr" ""
    (let ((handler (hash-ref *fuse-dispatch* 'removexattr #f)))
      (if handler (handler path name) (- ENOSYS))))

  (c-define (scheme_lock path fh cmd type start len pid)
            (char-string unsigned-int64 int int int64 int64 int) int
            "scheme_lock" ""
    (let ((handler (hash-ref *fuse-dispatch* 'lock #f)))
      (if handler (handler path fh cmd type start len pid) (- ENOSYS))))

  (c-define (scheme_flock path fh op)
            (char-string unsigned-int64 int) int "scheme_flock" ""
    (let ((handler (hash-ref *fuse-dispatch* 'flock #f)))
      (if handler (handler path fh op) (- ENOSYS))))

  (c-define (scheme_fallocate path mode offset length fh)
            (char-string int int64 int64 unsigned-int64) int
            "scheme_fallocate" ""
    (let ((handler (hash-ref *fuse-dispatch* 'fallocate #f)))
      (if handler (handler path mode offset length fh) (- ENOSYS))))

  (c-define (scheme_lseek path offset whence fh)
            (char-string int64 int unsigned-int64) int
            "scheme_lseek" ""
    (let ((handler (hash-ref *fuse-dispatch* 'lseek #f)))
      (if handler (handler path offset whence fh) (- ENOSYS))))

  ;; ============================================================
  ;; Include trampolines AFTER c-define callbacks
  ;; (so Gambit's generated forward declarations are in scope)
  ;; ============================================================
  (c-declare "#include \"shim-trampolines.c\"")

  ;; ============================================================
  ;; Operations registration
  ;; ============================================================

  (define-c-lambda ffi-ops-reset! () void "ffi_ops_reset")

  ;; Core ops
  (define-c-lambda ffi-ops-set-getattr! () void "ffi_ops_set_getattr")
  (define-c-lambda ffi-ops-set-readdir! () void "ffi_ops_set_readdir")
  (define-c-lambda ffi-ops-set-open! () void "ffi_ops_set_open")
  (define-c-lambda ffi-ops-set-read! () void "ffi_ops_set_read")
  (define-c-lambda ffi-ops-set-release! () void "ffi_ops_set_release")
  (define-c-lambda ffi-ops-set-init! () void "ffi_ops_set_init")
  (define-c-lambda ffi-ops-set-destroy! () void "ffi_ops_set_destroy")

  ;; Read-write ops
  (define-c-lambda ffi-ops-set-write! () void "ffi_ops_set_write")
  (define-c-lambda ffi-ops-set-create! () void "ffi_ops_set_create")
  (define-c-lambda ffi-ops-set-unlink! () void "ffi_ops_set_unlink")
  (define-c-lambda ffi-ops-set-mkdir! () void "ffi_ops_set_mkdir")
  (define-c-lambda ffi-ops-set-rmdir! () void "ffi_ops_set_rmdir")
  (define-c-lambda ffi-ops-set-rename! () void "ffi_ops_set_rename")
  (define-c-lambda ffi-ops-set-truncate! () void "ffi_ops_set_truncate")
  (define-c-lambda ffi-ops-set-chmod! () void "ffi_ops_set_chmod")
  (define-c-lambda ffi-ops-set-chown! () void "ffi_ops_set_chown")
  (define-c-lambda ffi-ops-set-utimens! () void "ffi_ops_set_utimens")
  (define-c-lambda ffi-ops-set-flush! () void "ffi_ops_set_flush")
  (define-c-lambda ffi-ops-set-fsync! () void "ffi_ops_set_fsync")

  ;; Extended ops
  (define-c-lambda ffi-ops-set-opendir! () void "ffi_ops_set_opendir")
  (define-c-lambda ffi-ops-set-releasedir! () void "ffi_ops_set_releasedir")
  (define-c-lambda ffi-ops-set-fsyncdir! () void "ffi_ops_set_fsyncdir")
  (define-c-lambda ffi-ops-set-statfs! () void "ffi_ops_set_statfs")
  (define-c-lambda ffi-ops-set-access! () void "ffi_ops_set_access")
  (define-c-lambda ffi-ops-set-readlink! () void "ffi_ops_set_readlink")
  (define-c-lambda ffi-ops-set-symlink! () void "ffi_ops_set_symlink")
  (define-c-lambda ffi-ops-set-link! () void "ffi_ops_set_link")
  (define-c-lambda ffi-ops-set-mknod! () void "ffi_ops_set_mknod")

  ;; Advanced ops
  (define-c-lambda ffi-ops-set-setxattr! () void "ffi_ops_set_setxattr")
  (define-c-lambda ffi-ops-set-getxattr! () void "ffi_ops_set_getxattr")
  (define-c-lambda ffi-ops-set-listxattr! () void "ffi_ops_set_listxattr")
  (define-c-lambda ffi-ops-set-removexattr! () void "ffi_ops_set_removexattr")
  (define-c-lambda ffi-ops-set-lock! () void "ffi_ops_set_lock")
  (define-c-lambda ffi-ops-set-flock! () void "ffi_ops_set_flock")
  (define-c-lambda ffi-ops-set-fallocate! () void "ffi_ops_set_fallocate")
  (define-c-lambda ffi-ops-set-lseek! () void "ffi_ops_set_lseek")

  ;; ============================================================
  ;; Main entry point
  ;; ============================================================

  (define-c-lambda ffi-fuse-main (int (pointer char-string)) int "ffi_fuse_main")
)
