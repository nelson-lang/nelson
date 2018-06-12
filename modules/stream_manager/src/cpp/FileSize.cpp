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
        FILE* fileptr = (FILE*)fp->getFilePointer();
        if (fileptr) {
            // save current file position
            int64 curpos = static_cast<int64>(NLSFTELL(fileptr));
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
}
//=============================================================================
