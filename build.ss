#!/usr/bin/env gxi
;; build.ss — gerbil-fuse build script
;; Detects FUSE library (FUSE-T, macFUSE, or libfuse3) and compiles FFI module.

(import :std/build-script
        :std/make)

(def here (path-directory (this-source-file)))
(def fuse-dir (path-expand "fuse" here))

;; Platform detection for FUSE include/library flags
(def (detect-fuse-flags)
  (cond
   ;; macOS: try FUSE-T via pkg-config first
   ((equal? (software-type) "darwin")
    (let ((cc-flags (cppflags "fuse-t"
                     ;; fallback: try macFUSE framework
                     (string-append
                      "-I/Library/Frameworks/fuse_t.framework/Headers"
                      " -D_FILE_OFFSET_BITS=64")))
          (ld-flags (ldflags "fuse-t"
                     ;; fallback: framework link
                     "-L/usr/local/lib -lfuse-t")))
      (values (string-append cc-flags " -D_FILE_OFFSET_BITS=64")
              ld-flags)))
   ;; Linux: use pkg-config for libfuse3
   (else
    (values (string-append (cppflags "fuse3" "-I/usr/include/fuse3")
                           " -D_FILE_OFFSET_BITS=64 -DFUSE_USE_VERSION=31")
            (ldflags "fuse3" "-lfuse3")))))

(defvalues (fuse-cc-flags fuse-ld-flags) (detect-fuse-flags))

(defbuild-script
  `(;; FFI module — needs FUSE C compiler and linker flags
    (gxc: "fuse/fuse-ffi"
          "-cc-options" ,(string-append "-I" fuse-dir " " fuse-cc-flags)
          "-ld-options" ,fuse-ld-flags)
    ;; Pure Scheme modules
    "fuse/fuse-types"
    "fuse/fuse"))
