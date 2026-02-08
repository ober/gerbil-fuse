/*
 * shim-trampolines.c — FUSE callback trampolines & ops registration
 *
 * Included by fuse-ffi.ss AFTER all c-define callbacks, so the
 * scheme_* functions are already declared by Gambit's generated code.
 *
 * Each trampoline receives raw C arguments from FUSE, calls the
 * c-define'd Scheme function, and copies results as needed.
 */

/* ============================================================
 * Core operation trampolines
 * ============================================================ */

#if FUSE_MAJOR_VERSION >= 3
static int fuse_getattr_trampoline(const char *path, struct stat *stbuf,
                                   struct fuse_file_info *fi) {
#else
static int fuse_getattr_trampoline(const char *path, struct stat *stbuf) {
#endif
    ffi_stat_reset();
    int ret = scheme_getattr((char *)path);
    if (ret == 0) {
        memcpy(stbuf, &_current_stbuf, sizeof(struct stat));
    }
    return ret;
}

#if FUSE_MAJOR_VERSION >= 3
static int fuse_readdir_trampoline(const char *path, void *buf,
                                   fuse_fill_dir_t filler, off_t offset,
                                   struct fuse_file_info *fi,
                                   enum fuse_readdir_flags flags) {
#else
static int fuse_readdir_trampoline(const char *path, void *buf,
                                   fuse_fill_dir_t filler, off_t offset,
                                   struct fuse_file_info *fi) {
#endif
    return scheme_readdir((char *)path, buf, (void *)filler, (long)offset);
}

static int fuse_open_trampoline(const char *path, struct fuse_file_info *fi) {
    int ret = scheme_open((char *)path, fi->flags);
    if (ret > 0) {
        fi->fh = ret;
        return 0;
    }
    return ret;
}

static int fuse_read_trampoline(const char *path, char *buf, size_t size,
                                off_t offset, struct fuse_file_info *fi) {
    return scheme_read((char *)path, buf, size, (long)offset, fi->fh);
}

static int fuse_release_trampoline(const char *path, struct fuse_file_info *fi) {
    return scheme_release((char *)path, fi->fh);
}

#if FUSE_MAJOR_VERSION >= 3
static void *fuse_init_trampoline(struct fuse_conn_info *conn,
                                  struct fuse_config *cfg) {
#else
static void *fuse_init_trampoline(struct fuse_conn_info *conn) {
#endif
    scheme_init();
    return NULL;
}

static void fuse_destroy_trampoline(void *private_data) {
    scheme_destroy();
}

/* ============================================================
 * Read-write operation trampolines
 * ============================================================ */

static int fuse_write_trampoline(const char *path, const char *buf, size_t size,
                                 off_t offset, struct fuse_file_info *fi) {
    return scheme_write((char *)path, (void *)buf, size, (long)offset, fi->fh);
}

static int fuse_create_trampoline(const char *path, mode_t mode,
                                  struct fuse_file_info *fi) {
    int ret = scheme_create((char *)path, (int)mode);
    if (ret >= 0) {
        fi->fh = ret;
        return 0;
    }
    return ret;
}

static int fuse_unlink_trampoline(const char *path) {
    return scheme_unlink((char *)path);
}

static int fuse_mkdir_trampoline(const char *path, mode_t mode) {
    return scheme_mkdir((char *)path, (int)mode);
}

static int fuse_rmdir_trampoline(const char *path) {
    return scheme_rmdir((char *)path);
}

#if FUSE_MAJOR_VERSION >= 3
static int fuse_rename_trampoline(const char *oldpath, const char *newpath,
                                  unsigned int flags) {
    return scheme_rename((char *)oldpath, (char *)newpath, flags);
}
#else
static int fuse_rename_trampoline(const char *oldpath, const char *newpath) {
    return scheme_rename((char *)oldpath, (char *)newpath, 0);
}
#endif

#if FUSE_MAJOR_VERSION >= 3
static int fuse_truncate_trampoline(const char *path, off_t size,
                                    struct fuse_file_info *fi) {
#else
static int fuse_truncate_trampoline(const char *path, off_t size) {
#endif
    return scheme_truncate((char *)path, (long long)size);
}

#if FUSE_MAJOR_VERSION >= 3
static int fuse_chmod_trampoline(const char *path, mode_t mode,
                                 struct fuse_file_info *fi) {
#else
static int fuse_chmod_trampoline(const char *path, mode_t mode) {
#endif
    return scheme_chmod((char *)path, (int)mode);
}

#if FUSE_MAJOR_VERSION >= 3
static int fuse_chown_trampoline(const char *path, uid_t uid, gid_t gid,
                                 struct fuse_file_info *fi) {
#else
static int fuse_chown_trampoline(const char *path, uid_t uid, gid_t gid) {
#endif
    return scheme_chown((char *)path, (int)uid, (int)gid);
}

#if FUSE_MAJOR_VERSION >= 3
static int fuse_utimens_trampoline(const char *path, const struct timespec tv[2],
                                   struct fuse_file_info *fi) {
#else
static int fuse_utimens_trampoline(const char *path, const struct timespec tv[2]) {
#endif
    return scheme_utimens((char *)path,
                          (long)tv[0].tv_sec, (long)tv[0].tv_nsec,
                          (long)tv[1].tv_sec, (long)tv[1].tv_nsec);
}

static int fuse_flush_trampoline(const char *path, struct fuse_file_info *fi) {
    return scheme_flush((char *)path, fi->fh);
}

static int fuse_fsync_trampoline(const char *path, int datasync,
                                 struct fuse_file_info *fi) {
    return scheme_fsync((char *)path, datasync, fi->fh);
}

/* ============================================================
 * Extended operation trampolines
 * ============================================================ */

static int fuse_opendir_trampoline(const char *path, struct fuse_file_info *fi) {
    return scheme_opendir((char *)path);
}

static int fuse_releasedir_trampoline(const char *path, struct fuse_file_info *fi) {
    return scheme_releasedir((char *)path);
}

static int fuse_fsyncdir_trampoline(const char *path, int datasync,
                                    struct fuse_file_info *fi) {
    return scheme_fsyncdir((char *)path, datasync);
}

static int fuse_statfs_trampoline(const char *path, struct statvfs *stbuf) {
    ffi_statvfs_reset();
    int ret = scheme_statfs((char *)path);
    if (ret == 0) {
        memcpy(stbuf, &_current_stvfs, sizeof(struct statvfs));
    }
    return ret;
}

static int fuse_access_trampoline(const char *path, int mask) {
    return scheme_access((char *)path, mask);
}

static int fuse_readlink_trampoline(const char *path, char *buf, size_t size) {
    int ret = scheme_readlink((char *)path, _readlink_buf, sizeof(_readlink_buf));
    if (ret == 0 && size > 0) {
        size_t len = _readlink_len;
        if (len >= size) len = size - 1;
        memcpy(buf, _readlink_buf, len);
        buf[len] = '\0';
    }
    return ret;
}

static int fuse_symlink_trampoline(const char *target, const char *linkpath) {
    return scheme_symlink((char *)target, (char *)linkpath);
}

static int fuse_link_trampoline(const char *oldpath, const char *newpath) {
    return scheme_link((char *)oldpath, (char *)newpath);
}

static int fuse_mknod_trampoline(const char *path, mode_t mode, dev_t dev) {
    return scheme_mknod((char *)path, (int)mode, (unsigned long)dev);
}

/* ============================================================
 * Advanced operation trampolines
 * ============================================================ */

#ifdef __APPLE__
static int fuse_setxattr_trampoline(const char *path, const char *name,
                                    const char *value, size_t size, int flags,
                                    uint32_t position) {
    return scheme_setxattr((char *)path, (char *)name, (void *)value, size, flags);
}
#else
static int fuse_setxattr_trampoline(const char *path, const char *name,
                                    const char *value, size_t size, int flags) {
    return scheme_setxattr((char *)path, (char *)name, (void *)value, size, flags);
}
#endif

#ifdef __APPLE__
static int fuse_getxattr_trampoline(const char *path, const char *name,
                                    char *value, size_t size, uint32_t position) {
#else
static int fuse_getxattr_trampoline(const char *path, const char *name,
                                    char *value, size_t size) {
#endif
    memset(_xattr_buf, 0, sizeof(_xattr_buf));
    _xattr_len = 0;
    int ret = scheme_getxattr((char *)path, (char *)name, _xattr_buf, size);
    if (ret >= 0 && value != NULL && size > 0) {
        size_t copy_len = (size_t)ret;
        if (copy_len > size) copy_len = size;
        memcpy(value, _xattr_buf, copy_len);
    }
    return ret;
}

static int fuse_listxattr_trampoline(const char *path, char *list, size_t size) {
    memset(_xattr_buf, 0, sizeof(_xattr_buf));
    _xattr_len = 0;
    int ret = scheme_listxattr((char *)path, _xattr_buf, size);
    if (ret >= 0 && list != NULL && size > 0) {
        size_t copy_len = (size_t)ret;
        if (copy_len > size) copy_len = size;
        memcpy(list, _xattr_buf, copy_len);
    }
    return ret;
}

static int fuse_removexattr_trampoline(const char *path, const char *name) {
    return scheme_removexattr((char *)path, (char *)name);
}

static int fuse_lock_trampoline(const char *path, struct fuse_file_info *fi,
                                int cmd, struct flock *lock) {
    return scheme_lock((char *)path, fi->fh, cmd,
                       lock->l_type, (long long)lock->l_start,
                       (long long)lock->l_len, lock->l_pid);
}

static int fuse_flock_trampoline(const char *path, struct fuse_file_info *fi,
                                 int op) {
    return scheme_flock((char *)path, fi->fh, op);
}

static int fuse_fallocate_trampoline(const char *path, int mode,
                                     off_t offset, off_t length,
                                     struct fuse_file_info *fi) {
    return scheme_fallocate((char *)path, mode,
                            (long long)offset, (long long)length, fi->fh);
}

#if FUSE_MAJOR_VERSION >= 3
static off_t fuse_lseek_trampoline(const char *path, off_t off, int whence,
                                   struct fuse_file_info *fi) {
    _lseek_result = -1;
    int ret = scheme_lseek((char *)path, (long long)off, whence, fi->fh);
    if (ret >= 0) return _lseek_result;
    return ret;
}
#endif

/* ============================================================
 * Operations struct registration
 * ============================================================ */

/* Core ops */
void ffi_ops_set_getattr(void)  { fuse_ops.getattr  = fuse_getattr_trampoline; }
void ffi_ops_set_readdir(void)  { fuse_ops.readdir  = fuse_readdir_trampoline; }
void ffi_ops_set_open(void)     { fuse_ops.open     = fuse_open_trampoline; }
void ffi_ops_set_read(void)     { fuse_ops.read     = fuse_read_trampoline; }
void ffi_ops_set_release(void)  { fuse_ops.release  = fuse_release_trampoline; }
void ffi_ops_set_init(void)     { fuse_ops.init     = fuse_init_trampoline; }
void ffi_ops_set_destroy(void)  { fuse_ops.destroy  = fuse_destroy_trampoline; }

/* Read-write ops */
void ffi_ops_set_write(void)    { fuse_ops.write    = fuse_write_trampoline; }
void ffi_ops_set_create(void)   { fuse_ops.create   = fuse_create_trampoline; }
void ffi_ops_set_unlink(void)   { fuse_ops.unlink   = fuse_unlink_trampoline; }
void ffi_ops_set_mkdir(void)    { fuse_ops.mkdir    = fuse_mkdir_trampoline; }
void ffi_ops_set_rmdir(void)    { fuse_ops.rmdir    = fuse_rmdir_trampoline; }
void ffi_ops_set_rename(void)   { fuse_ops.rename   = fuse_rename_trampoline; }
void ffi_ops_set_truncate(void) { fuse_ops.truncate = fuse_truncate_trampoline; }
void ffi_ops_set_chmod(void)    { fuse_ops.chmod    = fuse_chmod_trampoline; }
void ffi_ops_set_chown(void)    { fuse_ops.chown    = fuse_chown_trampoline; }
void ffi_ops_set_utimens(void)  { fuse_ops.utimens  = fuse_utimens_trampoline; }
void ffi_ops_set_flush(void)    { fuse_ops.flush    = fuse_flush_trampoline; }
void ffi_ops_set_fsync(void)    { fuse_ops.fsync    = fuse_fsync_trampoline; }

/* Extended ops */
void ffi_ops_set_opendir(void)    { fuse_ops.opendir    = fuse_opendir_trampoline; }
void ffi_ops_set_releasedir(void) { fuse_ops.releasedir = fuse_releasedir_trampoline; }
void ffi_ops_set_fsyncdir(void)   { fuse_ops.fsyncdir   = fuse_fsyncdir_trampoline; }
void ffi_ops_set_statfs(void)     { fuse_ops.statfs     = fuse_statfs_trampoline; }
void ffi_ops_set_access(void)     { fuse_ops.access     = fuse_access_trampoline; }
void ffi_ops_set_readlink(void)   { fuse_ops.readlink   = fuse_readlink_trampoline; }
void ffi_ops_set_symlink(void)    { fuse_ops.symlink    = fuse_symlink_trampoline; }
void ffi_ops_set_link(void)       { fuse_ops.link       = fuse_link_trampoline; }
void ffi_ops_set_mknod(void)      { fuse_ops.mknod      = fuse_mknod_trampoline; }

/* Advanced ops */
void ffi_ops_set_setxattr(void)    { fuse_ops.setxattr    = fuse_setxattr_trampoline; }
void ffi_ops_set_getxattr(void)    { fuse_ops.getxattr    = fuse_getxattr_trampoline; }
void ffi_ops_set_listxattr(void)   { fuse_ops.listxattr   = fuse_listxattr_trampoline; }
void ffi_ops_set_removexattr(void) { fuse_ops.removexattr = fuse_removexattr_trampoline; }
void ffi_ops_set_lock(void)        { fuse_ops.lock        = fuse_lock_trampoline; }
void ffi_ops_set_flock(void)       { fuse_ops.flock       = fuse_flock_trampoline; }
void ffi_ops_set_fallocate(void)   { fuse_ops.fallocate   = fuse_fallocate_trampoline; }

#if FUSE_MAJOR_VERSION >= 3
void ffi_ops_set_lseek(void)       { fuse_ops.lseek       = fuse_lseek_trampoline; }
#else
void ffi_ops_set_lseek(void)       { /* lseek not available in FUSE 2 */ }
#endif
