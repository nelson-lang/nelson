//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
#include "FileRead.hpp"
#include "Error.hpp"
#include "FileSeek.hpp"
#include "FileTell.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
FileRead(Evaluator* eval, File* fp, int64 sizeToRead, Class classPrecision, size_t skip,
    bool bIsLittleEndian, int& sizeReallyRead)
{
    ArrayOf toRead;
    sizeReallyRead = -1;
    if (!fp) {
        return toRead;
    }
    if (fp->isInterfaceMethod()) {
        return toRead;
    }
    FILE* fileptr = static_cast<FILE*>(fp->getFilePointer());
    if (!fileptr) {
        return toRead;
    }
    if (sizeToRead == -1) {
#if (defined(_LP64) || defined(_WIN64))
        auto curpos = static_cast<int64>(NLSFTELL(fileptr));
#else
        long curpos = NLSFTELL(fileptr);
#endif
        NLSFSEEK(fileptr, 0, SEEK_END);
#if (defined(_LP64) || defined(_WIN64))
        auto endpos = static_cast<int64>(NLSFTELL(fileptr));
#else
        long endpos = NLSFTELL(fileptr);
#endif
        sizeToRead = endpos - curpos;
        NLSFSEEK(fileptr, curpos, SEEK_SET);
        switch (classPrecision) {
        case NLS_LOGICAL: {
            sizeToRead = sizeToRead / sizeof(logical);
        } break;
        case NLS_UINT8: {
            sizeToRead = sizeToRead / sizeof(uint8);
        } break;
        case NLS_INT8: {
            sizeToRead = sizeToRead / sizeof(int8);
        } break;
        case NLS_UINT16: {
            sizeToRead = sizeToRead / sizeof(uint16);
        } break;
        case NLS_INT16: {
            sizeToRead = sizeToRead / sizeof(int16);
        } break;
        case NLS_UINT32: {
            sizeToRead = sizeToRead / sizeof(uint32);
        } break;
        case NLS_INT32: {
            sizeToRead = sizeToRead / sizeof(int32);
        } break;
        case NLS_UINT64: {
            sizeToRead = sizeToRead / sizeof(uint64);
        } break;
        case NLS_INT64: {
            sizeToRead = sizeToRead / sizeof(int64);
        } break;
        case NLS_SINGLE: {
            sizeToRead = sizeToRead / sizeof(single);
        } break;
        case NLS_DOUBLE: {
            sizeToRead = sizeToRead / sizeof(double);
        } break;
        case NLS_CHAR: {
            sizeToRead = sizeToRead / sizeof(char);
        }
        }
    }
    if (classPrecision == NLS_CHAR) {
        char* str = nullptr;
        try {
            str = new char[static_cast<indexType>(sizeToRead + 1)];
        } catch (const std::bad_alloc& e) {
            e.what();
            str = nullptr;
            Error(ERROR_MEMORY_ALLOCATION);
        }
        auto count = static_cast<size_t>(sizeToRead);
        size_t elsize = sizeof(char);
        if (str) {
            sizeReallyRead = static_cast<int>(fread(str, elsize, count, fileptr));
        }
        if (sizeReallyRead < 0) {
            delete[] str;
        } else if (sizeReallyRead < sizeToRead) {
            char* resizestr = nullptr;
            try {
                resizestr = new char[(size_t)sizeReallyRead + (size_t)1];
            } catch (const std::bad_alloc& e) {
                e.what();
                delete[] str;
                str = nullptr;
            }
            if (str == nullptr) {
                Error(ERROR_MEMORY_ALLOCATION);
            }
            if (str) {
                memcpy(resizestr, str, sizeof(char) * (size_t)sizeReallyRead);
            }
            if (bIsLittleEndian != isLittleEndianFormat()) {
                for (size_t k = 0; k < static_cast<size_t>(sizeReallyRead); k++) {
                    resizestr[k] = bswap<char>(resizestr[k]);
                }
            }
            resizestr[sizeReallyRead] = 0;
            delete[] str;
            toRead = ArrayOf::characterArrayConstructor(resizestr);
            delete[] resizestr;
            if (!feof(fileptr)) {
                if (skip) {
                    NLSFSEEK(fileptr, skip, SEEK_CUR);
                }
            }
        } else {
            str[sizeReallyRead] = 0;
            toRead = ArrayOf::characterArrayConstructor(str);
            delete[] str;
        }
    } else {
        void* ptr = ArrayOf::allocateArrayOf(classPrecision, static_cast<indexType>(sizeToRead));
        Dimensions dim(static_cast<indexType>(sizeToRead), 1);
        toRead = ArrayOf(classPrecision, dim, ptr);
        size_t count(toRead.getLength());
        size_t elsize(toRead.getElementSize());
        sizeReallyRead = static_cast<int>(fread(ptr, elsize, count, fileptr));
        if (sizeReallyRead < 0) {
            Dimensions dim(static_cast<indexType>(sizeToRead), 1);
            toRead.vectorResize(0);
            return toRead;
        }
        if (sizeReallyRead < sizeToRead) {
            void* ptrResize = ArrayOf::allocateArrayOf(classPrecision, sizeReallyRead);
            Dimensions dim(sizeReallyRead, 1);
            memcpy(ptrResize, ptr, toRead.getElementSize() * sizeReallyRead);
            toRead = ArrayOf(classPrecision, dim, ptrResize);
        }
        if (bIsLittleEndian != isLittleEndianFormat()) {
            BITSWAP(toRead.getReadWriteDataPointer(), sizeReallyRead, classPrecision);
        }
        if (!feof(fileptr)) {
            if (skip) {
                NLSFSEEK(fileptr, skip, SEEK_CUR);
            }
        }
    }
    return toRead;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
