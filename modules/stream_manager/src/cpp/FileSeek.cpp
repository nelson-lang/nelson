//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSeek.hpp"
#include "FileBackend.hpp"
#include "FileSize.hpp"
#include "FileTell.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
seekBackend(FileBackend* backend, int64 offset, int origin);
//=============================================================================
bool
FileSeek(File* fp, int64 offset, int origin)
{
    if (fp) {
        if (FileBackend* backend = fp->getBackend()) {
            return seekBackend(backend, offset, origin);
        }
        if (fp->isInterfaceMethod()) {
            return false;
        }
        FILE* fileptr = static_cast<FILE*>(fp->getFilePointer());
        if (fileptr) {
            int ORIGIN;
            switch (origin) {
            case -1: {
                ORIGIN = SEEK_SET;
            } break;
            case 0: {
                ORIGIN = SEEK_CUR;
            } break;
            case 1: {
                ORIGIN = SEEK_END;
            } break;
            default:
                return false;
            }
#if (defined(_LP64) || defined(_WIN64))
            int64 curPos = FileTell(fp);
            int64 _offset = offset;
#else
            long curPos = static_cast<long>(FileTell(fp));
            long _offset = static_cast<long>(offset);
#endif
            int res = NLSFSEEK(fileptr, _offset, ORIGIN);
            if (res == 0) {
                int64 desiredPos = FileTell(fp);
                int64 eofPos = FileSize(fp);
                if (desiredPos > eofPos || desiredPos < 0) {
                    FileSeek(fp, curPos, -1);
                    return false;
                }
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
// origin: -1 from the beginning, 0 from the current position, 1 from the end.
static bool
seekBackend(FileBackend* backend, int64 offset, int origin)
{
    switch (origin) {
    case -1:
        return backend->seek(offset);
    case 0:
        return backend->seek(backend->tell() + offset);
    case 1: {
        const int64 size = backend->size();
        return size >= 0 && backend->seek(size + offset);
    }
    default:
        return false;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
