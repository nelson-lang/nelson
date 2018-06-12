//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
}
//=============================================================================
