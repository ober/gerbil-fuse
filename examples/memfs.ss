#!/usr/bin/env gxi
;;; memfs.ss — In-memory read-write FUSE filesystem
;;;
;;; Demonstrates all CRUD operations: create, read, write, delete files
;;; and directories, rename, chmod, chown, truncate, utimens.
;;;
;;; Usage:
;;;   mkdir -p /tmp/fuse-mem
;;;   gxi examples/memfs.ss /tmp/fuse-mem
;;;   echo "hello" > /tmp/fuse-mem/test.txt
;;;   cat /tmp/fuse-mem/test.txt
;;;   mkdir /tmp/fuse-mem/subdir
;;;   ls -la /tmp/fuse-mem/
;;;   umount /tmp/fuse-mem

(import :gerbil-fuse/fuse/fuse
        :gerbil-fuse/fuse/fuse-types
        :gerbil-fuse/fuse/fuse-ffi)

;; ============================================================
;; Node types: files and directories stored in a hash table
;; ============================================================

(defstruct inode
  (type     ;; 'file or 'dir
   mode     ;; permission bits (e.g. #o755)
   uid gid
   size     ;; file size in bytes (0 for dirs)
   data     ;; u8vector for files, #f for dirs
   children ;; list of child names for dirs, #f for files
   nlink    ;; hard link count
   atime mtime ctime)  ;; timestamps (seconds)
  transparent: #t)

;; The filesystem: path -> inode
(def *fs* (make-hash-table))

;; Current time helper
(def (now) (inexact->exact (floor (time->seconds (current-time)))))

(def (make-file-inode mode: (mode #o644) uid: (uid 0) gid: (gid 0))
  (let ((t (now)))
    (make-inode 'file mode uid gid 0 (make-u8vector 0) #f 1 t t t)))

(def (make-dir-inode mode: (mode #o755) uid: (uid 0) gid: (gid 0))
  (let ((t (now)))
    (make-inode 'dir mode uid gid 0 #f [] 2 t t t)))

;; Initialize root directory
(hash-put! *fs* "/" (make-dir-inode))

;; ============================================================
;; Path helpers
;; ============================================================

(def (parent-path path)
  "Return parent directory of path. / returns /."
  (if (string=? path "/") "/"
    (let loop ((i (- (string-length path) 1)))
      (cond
        ((<= i 0) "/")
        ((char=? (string-ref path i) #\/)
         (if (= i 0) "/" (substring path 0 i)))
        (else (loop (- i 1)))))))

(def (path-basename path)
  "Return the last component of path."
  (if (string=? path "/") ""
    (let loop ((i (- (string-length path) 1)))
      (cond
        ((< i 0) path)
        ((char=? (string-ref path i) #\/)
         (substring path (+ i 1) (string-length path)))
        (else (loop (- i 1)))))))

(def (dir-add-child! dir-path name)
  "Add a child name to a directory's children list."
  (let ((dir (hash-get *fs* dir-path)))
    (when (and dir (eq? (inode-type dir) 'dir))
      (set! (inode-children dir) (cons name (inode-children dir))))))

(def (dir-remove-child! dir-path name)
  "Remove a child name from a directory's children list."
  (let ((dir (hash-get *fs* dir-path)))
    (when (and dir (eq? (inode-type dir) 'dir))
      (let ((filtered (let loop ((lst (inode-children dir)) (acc []))
                        (cond ((null? lst) (reverse acc))
                              ((string=? (car lst) name) (loop (cdr lst) acc))
                              (else (loop (cdr lst) (cons (car lst) acc)))))))
        (set! (inode-children dir) filtered)))))

;; ============================================================
;; FUSE operations
;; ============================================================

(def (memfs-getattr path)
  (let ((node (hash-get *fs* path)))
    (if node
      (if (eq? (inode-type node) 'dir)
        (make-dir-stat mode: (inode-mode node)
                       nlink: (inode-nlink node)
                       uid: (inode-uid node)
                       gid: (inode-gid node))
        (make-file-stat/reg size: (inode-size node)
                            mode: (inode-mode node)
                            uid: (inode-uid node)
                            gid: (inode-gid node)))
      (- ENOENT))))

(def (memfs-readdir path offset)
  (let ((node (hash-get *fs* path)))
    (if (and node (eq? (inode-type node) 'dir))
      (cons* "." ".." (inode-children node))
      (- ENOENT))))

(def (memfs-open path flags)
  (if (hash-get *fs* path) 0 (- ENOENT)))

(def (memfs-read path size offset fh)
  (let ((node (hash-get *fs* path)))
    (if (and node (eq? (inode-type node) 'file))
      (let* ((data (inode-data node))
             (len (u8vector-length data))
             (end (min (+ offset size) len)))
        (if (>= offset len) #u8()
          (subu8vector data offset end)))
      (- ENOENT))))

(def (memfs-write path data offset fh)
  (let ((node (hash-get *fs* path)))
    (if (and node (eq? (inode-type node) 'file))
      (let* ((old-data (inode-data node))
             (old-len (u8vector-length old-data))
             (write-len (u8vector-length data))
             (new-len (max old-len (+ offset write-len)))
             (new-data (make-u8vector new-len 0)))
        ;; Copy old data
        (when (> old-len 0)
          (u8vector-copy! new-data 0 old-data 0 old-len))
        ;; Write new data at offset
        (u8vector-copy! new-data offset data 0 write-len)
        (set! (inode-data node) new-data)
        (set! (inode-size node) new-len)
        (set! (inode-mtime node) (now))
        write-len)
      (- ENOENT))))

(def (memfs-create path mode)
  (let ((par (parent-path path)))
    (if (hash-get *fs* par)
      (if (hash-get *fs* path)
        (- EEXIST)
        (begin
          (hash-put! *fs* path (make-file-inode mode: (bitwise-and mode #o7777)))
          (dir-add-child! par (path-basename path))
          0))
      (- ENOENT))))

(def (memfs-unlink path)
  (let ((node (hash-get *fs* path)))
    (if (and node (eq? (inode-type node) 'file))
      (begin
        (dir-remove-child! (parent-path path) (path-basename path))
        (hash-remove! *fs* path)
        0)
      (if node (- EISDIR) (- ENOENT)))))

(def (memfs-mkdir path mode)
  (let ((par (parent-path path)))
    (if (hash-get *fs* par)
      (if (hash-get *fs* path)
        (- EEXIST)
        (begin
          (hash-put! *fs* path (make-dir-inode mode: (bitwise-and mode #o7777)))
          (dir-add-child! par (path-basename path))
          ;; Increment parent nlink
          (let ((pnode (hash-get *fs* par)))
            (set! (inode-nlink pnode) (+ (inode-nlink pnode) 1)))
          0))
      (- ENOENT))))

(def (memfs-rmdir path)
  (let ((node (hash-get *fs* path)))
    (cond
      ((not node) (- ENOENT))
      ((not (eq? (inode-type node) 'dir)) (- ENOTDIR))
      ((not (null? (inode-children node))) (- ENOTEMPTY))
      (else
       (let ((par (parent-path path)))
         (dir-remove-child! par (path-basename path))
         (hash-remove! *fs* path)
         (let ((pnode (hash-get *fs* par)))
           (set! (inode-nlink pnode) (- (inode-nlink pnode) 1)))
         0)))))

(def (memfs-rename oldpath newpath flags)
  (let ((node (hash-get *fs* oldpath)))
    (if node
      (begin
        ;; Remove from old parent
        (dir-remove-child! (parent-path oldpath) (path-basename oldpath))
        ;; Remove any existing target
        (when (hash-get *fs* newpath)
          (dir-remove-child! (parent-path newpath) (path-basename newpath))
          (hash-remove! *fs* newpath))
        ;; Move node
        (hash-remove! *fs* oldpath)
        (hash-put! *fs* newpath node)
        (dir-add-child! (parent-path newpath) (path-basename newpath))
        ;; If directory, update nlink counts
        (when (eq? (inode-type node) 'dir)
          (let ((old-par (hash-get *fs* (parent-path oldpath)))
                (new-par (hash-get *fs* (parent-path newpath))))
            (when (and old-par (not (string=? (parent-path oldpath) (parent-path newpath))))
              (set! (inode-nlink old-par) (- (inode-nlink old-par) 1))
              (set! (inode-nlink new-par) (+ (inode-nlink new-par) 1)))))
        0)
      (- ENOENT))))

(def (memfs-truncate path size)
  (let ((node (hash-get *fs* path)))
    (if (and node (eq? (inode-type node) 'file))
      (let* ((old-data (inode-data node))
             (old-len (u8vector-length old-data))
             (new-data (make-u8vector size 0)))
        (when (> old-len 0)
          (u8vector-copy! new-data 0 old-data 0 (min old-len size)))
        (set! (inode-data node) new-data)
        (set! (inode-size node) size)
        (set! (inode-mtime node) (now))
        0)
      (- ENOENT))))

(def (memfs-chmod path mode)
  (let ((node (hash-get *fs* path)))
    (if node
      (begin
        (set! (inode-mode node) (bitwise-and mode #o7777))
        (set! (inode-ctime node) (now))
        0)
      (- ENOENT))))

(def (memfs-chown path uid gid)
  (let ((node (hash-get *fs* path)))
    (if node
      (begin
        (when (>= uid 0) (set! (inode-uid node) uid))
        (when (>= gid 0) (set! (inode-gid node) gid))
        (set! (inode-ctime node) (now))
        0)
      (- ENOENT))))

(def (memfs-utimens path atime-sec atime-nsec mtime-sec mtime-nsec)
  (let ((node (hash-get *fs* path)))
    (if node
      (begin
        (set! (inode-atime node) atime-sec)
        (set! (inode-mtime node) mtime-sec)
        0)
      (- ENOENT))))

(def (memfs-release path fh) 0)
(def (memfs-flush path fh) 0)
(def (memfs-fsync path datasync fh) 0)

(def (memfs-access path mask)
  (if (hash-get *fs* path) 0 (- ENOENT)))

(def (memfs-statfs path)
  (make-default-statvfs))

;; ============================================================
;; Build the ops and run
;; ============================================================

(def memfs-ops
  (make-fuse-operations
    getattr:  memfs-getattr
    readdir:  memfs-readdir
    open:     memfs-open
    read:     memfs-read
    write:    memfs-write
    create:   memfs-create
    unlink:   memfs-unlink
    mkdir:    memfs-mkdir
    rmdir:    memfs-rmdir
    rename:   memfs-rename
    truncate: memfs-truncate
    chmod:    memfs-chmod
    chown:    memfs-chown
    utimens:  memfs-utimens
    release:  memfs-release
    flush:    memfs-flush
    fsync:    memfs-fsync
    access:   memfs-access
    statfs:   memfs-statfs))

(def (main . args)
  (when (null? args)
    (displayln "Usage: memfs <mountpoint>")
    (exit 1))
  (displayln "Mounting in-memory filesystem on " (car args))
  (displayln "Press Ctrl+C or run 'umount " (car args) "' to unmount")
  (fuse-run memfs-ops (car args)))
