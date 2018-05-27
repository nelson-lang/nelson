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
#include "FileSeek.hpp"
#include "FileSize.hpp"
#include "FileTell.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
FileSeek(File* fp, int64 offset, int origin)
{
    if (fp) {
        if (fp->isInterfaceMethod()) {
            return false;
        }
        FILE* fileptr = (FILE*)fp->getFilePointer();
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
            long curPos = (long)FileTell(fp);
            long _offset = (long)offset;
#endif
            int res = NLSFSEEK(fileptr, _offset, ORIGIN);
            if (res == 0) {
                int64 desiredPos = FileTell(fp);
                int64 eofPos = FileSize(fp);
                if (desiredPos > eofPos || desiredPos < 0) {
                    FileSeek(fp, curPos, -1);
                    return false;
                } else {
                    return true;
                }
            }
        }
    }
    return false;
}
//=============================================================================
}
//=============================================================================
