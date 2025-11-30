#pragma once

#ifdef _WIN32
#include <cstdarg>
#include <cstdio>
#include <vector>
#include <io.h>

// Provide POSIX-like helpers missing on Windows builds.
inline int
replxx_win_dprintf(int fd, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    int required = _vscprintf(fmt, args);
    va_end(args);
    if (required < 0) {
        return required;
    }
    std::vector<char> buffer(static_cast<size_t>(required) + 1u, '\0');
    va_start(args, fmt);
    vsnprintf(buffer.data(), buffer.size(), fmt, args);
    va_end(args);
    return _write(fd, buffer.data(), required);
}

inline int
replxx_win_fsync(int fd)
{
    // return _commit(fd);
    return 0; // No-op for Windows as _commit is not always necessary.
}

#ifndef dprintf
#define dprintf replxx_win_dprintf
#endif

#ifndef fsync
#define fsync replxx_win_fsync
#endif

#endif // _WIN32
