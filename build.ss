#!/usr/bin/env gxi
;; build.ss — gerbil-fuse build script
;; Detects FUSE library (FUSE-T, macFUSE, or libfuse3) and compiles FFI module.

(import :std/build-script
        :std/make)

(def here (path-directory (this-source-file)))
(def fuse-dir (path-expand "fuse" here))

;; Find FUSE include directory on macOS
(def (find-macos-fuse-include)
  (cond
   ((file-exists? "/Library/Frameworks/fuse_t.framework/Headers/fuse.h")
    "-I/Library/Frameworks/fuse_t.framework/Headers")
   ((file-exists? "/usr/local/include/fuse/fuse.h")
    "-I/usr/local/include/fuse")
   ((file-exists? "/opt/homebrew/include/fuse/fuse.h")
    "-I/opt/homebrew/include/fuse")
   ((file-exists? "/usr/local/include/osxfuse/fuse.h")
    "-I/usr/local/include/osxfuse")
   (else
    (error "Cannot find FUSE headers. Install macFUSE or FUSE-T."))))

;; Find FUSE library flags on macOS
(def (find-macos-fuse-ldflags)
  (cond
   ((file-exists? "/usr/local/lib/libfuse-t.dylib")
    "-L/usr/local/lib -lfuse-t")
   ((file-exists? "/usr/local/lib/libosxfuse.dylib")
    "-L/usr/local/lib -losxfuse")
   ((file-exists? "/opt/homebrew/lib/libosxfuse.dylib")
    "-L/opt/homebrew/lib -losxfuse")
   (else
    (error "Cannot find FUSE library. Install macFUSE or FUSE-T."))))

;; Platform detection for FUSE include/library flags
(def (detect-fuse-flags)
  (cond
   ;; macOS: probe for FUSE-T or macFUSE directly
   ((equal? (cadr (system-type)) 'apple)
    (values (string-append (find-macos-fuse-include) " -D_FILE_OFFSET_BITS=64")
            (find-macos-fuse-ldflags)))
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
