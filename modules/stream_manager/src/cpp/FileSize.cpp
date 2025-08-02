//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSize.hpp"
#include "FileSeek.hpp"
#include "FileTell.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
int64
FileSize(File* fp)
{
    int64 sz = -1L;
    if (fp) {
        if (fp->isInterfaceMethod()) {
            return sz;
        }
        FILE* fileptr = static_cast<FILE*>(fp->getFilePointer());
        if (fileptr) {
            // save current file position
            auto curpos = static_cast<int64>(NLSFTELL(fileptr));
            // move to end of file
            int res;
            int ORIGIN = SEEK_END;
#if (defined(_LP64) || defined(_WIN64))
            int64 offset = 0;
#else
            long offset = 0;
#endif
            res = NLSFSEEK(fileptr, offset, ORIGIN);
            if (res == 0) {
                sz = static_cast<int64>(NLSFTELL(fileptr));
            }
            ORIGIN = SEEK_SET;
#if (defined(_LP64) || defined(_WIN64))
            offset = curpos;
#else
            offset = static_cast<long int>(curpos);
#endif
            // restore to initial position
            NLSFSEEK(fileptr, offset, ORIGIN);
        }
    }
    return sz;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
