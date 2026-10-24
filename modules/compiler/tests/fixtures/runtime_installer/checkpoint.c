// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// Interpose only in the test process, after a real filesystem operation succeeds.
static void
checkpoint(const char* operation, const char* path, int result)
{
    static unsigned long matches = 0;
    const int saved = errno;
    const char* expected = getenv("NELSON_INSTALLER_TEST_PATH");
    const char* action = getenv("NELSON_INSTALLER_TEST_ACTION");
    if (result == 0 && expected && action && strcmp(expected, path) == 0
        && strcmp(action, operation) == 0) {
        const char* occurrence = getenv("NELSON_INSTALLER_TEST_HIT");
        const unsigned long selected = occurrence ? strtoul(occurrence, NULL, 10) : 1;
        if (++matches == selected) {
            raise(SIGSTOP);
        }
    }
    errno = saved;
}

int
rename(const char* from, const char* to)
{
    int (*original)(const char*, const char*) = dlsym(RTLD_NEXT, "rename");
    if (!original) {
        errno = ENOSYS;
        return -1;
    }
    const int result = original(from, to);
    checkpoint("publish", to, result);
    return result;
}

int
unlink(const char* path)
{
    int (*original)(const char*) = dlsym(RTLD_NEXT, "unlink");
    if (!original) {
        errno = ENOSYS;
        return -1;
    }
    const int result = original(path);
    checkpoint("remove", path, result);
    return result;
}

int
remove(const char* path)
{
    int (*original)(const char*) = dlsym(RTLD_NEXT, "remove");
    if (!original) {
        errno = ENOSYS;
        return -1;
    }
    const int result = original(path);
    checkpoint("remove", path, result);
    return result;
}

int
rmdir(const char* path)
{
    int (*original)(const char*) = dlsym(RTLD_NEXT, "rmdir");
    if (!original) {
        errno = ENOSYS;
        return -1;
    }
    const int result = original(path);
    checkpoint("remove", path, result);
    return result;
}
