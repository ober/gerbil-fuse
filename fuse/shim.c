/*
 * shim.c — C shim for gerbil-fuse (part 1: headers, helpers, buffers)
 *
 * Included by fuse-ffi.ss via c-declare BEFORE c-define callbacks.
 * Part 2 (trampolines) is in shim-trampolines.c.
 *
 * Supports both libfuse2 (macOS/FUSE-T/macFUSE) and libfuse3 (Linux).
 */

#ifdef __APPLE__
  #ifndef FUSE_USE_VERSION
    #define FUSE_USE_VERSION 29
  #endif
  #include <fuse.h>
#else
  #ifndef FUSE_USE_VERSION
    #define FUSE_USE_VERSION 31
  #endif
  #include <fuse3/fuse.h>
#endif

#include <sys/stat.h>
#include <sys/statvfs.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

/* ============================================================
 * Platform compatibility
 * ============================================================ */

#ifndef ENODATA
  #ifdef ENOATTR
    #define ENODATA ENOATTR
  #else
    #define ENODATA ENOENT
  #endif
#endif

#ifndef ENOTSUP
  #define ENOTSUP EOPNOTSUPP
#endif

/* ============================================================
 * Stat buffer management
 * ============================================================ */

static struct stat _current_stbuf;

void ffi_stat_reset(void) {
    memset(&_current_stbuf, 0, sizeof(struct stat));
}

void ffi_stat_set_mode(int mode) {
    _current_stbuf.st_mode = mode;
}

void ffi_stat_set_nlink(int n) {
    _current_stbuf.st_nlink = n;
}

void ffi_stat_set_size(long long size) {
    _current_stbuf.st_size = size;
}

void ffi_stat_set_uid(int uid) {
    _current_stbuf.st_uid = uid;
}

void ffi_stat_set_gid(int gid) {
    _current_stbuf.st_gid = gid;
}

void ffi_stat_set_atime(long sec, long nsec) {
#ifdef __APPLE__
    _current_stbuf.st_atimespec.tv_sec = sec;
    _current_stbuf.st_atimespec.tv_nsec = nsec;
#else
    _current_stbuf.st_atim.tv_sec = sec;
    _current_stbuf.st_atim.tv_nsec = nsec;
#endif
}

void ffi_stat_set_mtime(long sec, long nsec) {
#ifdef __APPLE__
    _current_stbuf.st_mtimespec.tv_sec = sec;
    _current_stbuf.st_mtimespec.tv_nsec = nsec;
#else
    _current_stbuf.st_mtim.tv_sec = sec;
    _current_stbuf.st_mtim.tv_nsec = nsec;
#endif
}

void ffi_stat_set_ctime(long sec, long nsec) {
#ifdef __APPLE__
    _current_stbuf.st_ctimespec.tv_sec = sec;
    _current_stbuf.st_ctimespec.tv_nsec = nsec;
#else
    _current_stbuf.st_ctim.tv_sec = sec;
    _current_stbuf.st_ctim.tv_nsec = nsec;
#endif
}

void ffi_stat_set_blocks(long blocks) {
    _current_stbuf.st_blocks = blocks;
}

void ffi_stat_set_blksize(long blksize) {
    _current_stbuf.st_blksize = blksize;
}

/* ============================================================
 * Statvfs buffer management
 * ============================================================ */

static struct statvfs _current_stvfs;

void ffi_statvfs_reset(void) {
    memset(&_current_stvfs, 0, sizeof(struct statvfs));
}

void ffi_statvfs_set_bsize(unsigned long v) { _current_stvfs.f_bsize = v; }
void ffi_statvfs_set_frsize(unsigned long v) { _current_stvfs.f_frsize = v; }
void ffi_statvfs_set_blocks(unsigned long v) { _current_stvfs.f_blocks = v; }
void ffi_statvfs_set_bfree(unsigned long v) { _current_stvfs.f_bfree = v; }
void ffi_statvfs_set_bavail(unsigned long v) { _current_stvfs.f_bavail = v; }
void ffi_statvfs_set_files(unsigned long v) { _current_stvfs.f_files = v; }
void ffi_statvfs_set_ffree(unsigned long v) { _current_stvfs.f_ffree = v; }
void ffi_statvfs_set_namemax(unsigned long v) { _current_stvfs.f_namemax = v; }

/* ============================================================
 * Readlink result buffer
 * ============================================================ */

static char _readlink_buf[4096];
static size_t _readlink_len;

void ffi_readlink_set(const char *target, size_t len) {
    if (len >= sizeof(_readlink_buf))
        len = sizeof(_readlink_buf) - 1;
    memcpy(_readlink_buf, target, len);
    _readlink_buf[len] = '\0';
    _readlink_len = len;
}

/* ============================================================
 * Xattr result buffers
 * ============================================================ */

static char _xattr_buf[65536];
static size_t _xattr_len;

void ffi_xattr_set_result(const char *data, size_t len) {
    if (len > sizeof(_xattr_buf))
        len = sizeof(_xattr_buf);
    memcpy(_xattr_buf, data, len);
    _xattr_len = len;
}

size_t ffi_xattr_get_len(void) { return _xattr_len; }

/* ============================================================
 * Lseek result
 * ============================================================ */

static off_t _lseek_result;

void ffi_lseek_set_result(long long offset) { _lseek_result = offset; }

/* ============================================================
 * Operations struct (declared here, populated by trampolines)
 * ============================================================ */

static struct fuse_operations fuse_ops;

void ffi_ops_reset(void) {
    memset(&fuse_ops, 0, sizeof(fuse_ops));
}

int ffi_fuse_main(int argc, char **argv) {
    return fuse_main(argc, argv, &fuse_ops, NULL);
}
