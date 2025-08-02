//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if defined(__APPLE__) || defined(__MACH__)
#define _DARWIN_UNLIMITED_STREAMS
#include <sys/resource.h>
#endif
#include "MaxOpenedFiles.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
setMaxOpenedFiles()
{
#if defined(__APPLE__) || defined(__MACH__)
    // Set maximum number of open file descriptors
    struct rlimit maxfds, newfds;
    // Due to bugs in OS X (<rdar://problem/2941095>, <rdar://problem/3342704>,
    // <rdar://problem/3839173>) you have to get and set rlimits once before getrlimit will return
    // sensible values
    if (getrlimit(RLIMIT_NOFILE, &maxfds) < 0) {
        return false;
    }
    if (setrlimit(RLIMIT_NOFILE, &maxfds) < 0) {
        return false;
    }
    if (getrlimit(RLIMIT_NOFILE, &maxfds) < 0) {
        return false;
    }
    newfds.rlim_max = (maxfds.rlim_max > MIN_OPENFILES) ? maxfds.rlim_max : MIN_OPENFILES;
    newfds.rlim_cur = (maxfds.rlim_cur > MIN_OPENFILES) ? maxfds.rlim_cur : MIN_OPENFILES;
    if (newfds.rlim_max != maxfds.rlim_max || newfds.rlim_cur != maxfds.rlim_cur) {
        if (setrlimit(RLIMIT_NOFILE, &newfds) < 0) {
            return false;
        }
    }
#endif
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
