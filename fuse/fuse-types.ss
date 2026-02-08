(export #t)
(import :gerbil-fuse/fuse/fuse-ffi)

;; ============================================================
;; file-stat: Scheme-side representation of struct stat
;; ============================================================

(defstruct file-stat
  (mode nlink size uid gid
   atime-sec atime-nsec
   mtime-sec mtime-nsec
   ctime-sec ctime-nsec
   blocks blksize)
  transparent: #t)

(def (file-stat->c! st)
  "Flush a file-stat struct into the C stat buffer."
  (ffi-stat-reset!)
  (ffi-stat-set-mode!   (file-stat-mode st))
  (ffi-stat-set-nlink!  (file-stat-nlink st))
  (ffi-stat-set-size!   (file-stat-size st))
  (ffi-stat-set-uid!    (file-stat-uid st))
  (ffi-stat-set-gid!    (file-stat-gid st))
  (ffi-stat-set-atime!  (file-stat-atime-sec st) (file-stat-atime-nsec st))
  (ffi-stat-set-mtime!  (file-stat-mtime-sec st) (file-stat-mtime-nsec st))
  (ffi-stat-set-ctime!  (file-stat-ctime-sec st) (file-stat-ctime-nsec st))
  (when (file-stat-blocks st)
    (ffi-stat-set-blocks! (file-stat-blocks st)))
  (when (file-stat-blksize st)
    (ffi-stat-set-blksize! (file-stat-blksize st))))

;; ============================================================
;; Convenience constructors
;; ============================================================

(def (make-dir-stat mode: (mode #o755) nlink: (nlink 2) uid: (uid 0) gid: (gid 0))
  "Create a file-stat for a directory."
  (make-file-stat (bitwise-ior S_IFDIR mode) nlink 0 uid gid
                  0 0 0 0 0 0 #f #f))

(def (make-file-stat/reg size: (size 0) mode: (mode #o644) uid: (uid 0) gid: (gid 0))
  "Create a file-stat for a regular file."
  (make-file-stat (bitwise-ior S_IFREG mode) 1 size uid gid
                  0 0 0 0 0 0 #f #f))

(def (make-symlink-stat mode: (mode #o777) uid: (uid 0) gid: (gid 0) target-len: (target-len 0))
  "Create a file-stat for a symbolic link."
  (make-file-stat (bitwise-ior S_IFLNK mode) 1 target-len uid gid
                  0 0 0 0 0 0 #f #f))

;; ============================================================
;; fs-statvfs: Scheme-side representation of struct statvfs
;; ============================================================

(defstruct fs-statvfs
  (bsize frsize blocks bfree bavail files ffree namemax)
  transparent: #t)

(def (fs-statvfs->c! sv)
  "Flush a fs-statvfs struct into the C statvfs buffer."
  (ffi-statvfs-reset!)
  (ffi-statvfs-set-bsize!   (fs-statvfs-bsize sv))
  (ffi-statvfs-set-frsize!  (fs-statvfs-frsize sv))
  (ffi-statvfs-set-blocks!  (fs-statvfs-blocks sv))
  (ffi-statvfs-set-bfree!   (fs-statvfs-bfree sv))
  (ffi-statvfs-set-bavail!  (fs-statvfs-bavail sv))
  (ffi-statvfs-set-files!   (fs-statvfs-files sv))
  (ffi-statvfs-set-ffree!   (fs-statvfs-ffree sv))
  (ffi-statvfs-set-namemax! (fs-statvfs-namemax sv)))

;; ============================================================
;; Default statvfs
;; ============================================================

(def (make-default-statvfs)
  "Create a reasonable default statvfs for virtual filesystems."
  (make-fs-statvfs
    4096     ;; bsize
    4096     ;; frsize
    1000000  ;; blocks
    500000   ;; bfree
    500000   ;; bavail
    1000000  ;; files
    500000   ;; ffree
    255))    ;; namemax
