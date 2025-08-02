//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include <cstring>
#include "FileRead.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "FileSeek.hpp"
#include "FileTell.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
FileRead(File* fp, int64 sizeToRead, NelsonType classPrecision, size_t skip, bool bIsLittleEndian,
    int& sizeReallyRead)
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
        } break;
        case NLS_UNKNOWN:
        case NLS_GO_HANDLE:
        case NLS_HANDLE:
        case NLS_CELL_ARRAY:
        case NLS_STRUCT_ARRAY:
        case NLS_CLASS_ARRAY:
        case NLS_FUNCTION_HANDLE:
        case NLS_STRING_ARRAY:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        default: {
        } break;
        }
    }
    if (classPrecision == NLS_CHAR) {
        std::string encoding = wstring_to_utf8(fp->getEncoding());
        char* str = nullptr;
        try {
            str = new char[static_cast<indexType>(sizeToRead + 1)];
        } catch (const std::bad_alloc&) {
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
            } catch (const std::bad_alloc&) {
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
            if (encoding != "UTF-8") {
                std::string asUtf8;
                bool res = charsetToUtf8Converter(resizestr, encoding, asUtf8);
                delete[] resizestr;
                if (res) {
                    toRead = ArrayOf::characterArrayConstructor(asUtf8);
                } else {
                    Error(_("Cannot to use encoding:") + encoding);
                }
            } else {
                toRead = ArrayOf::characterArrayConstructor(resizestr);
                delete[] resizestr;
            }
            if (!feof(fileptr)) {
                if (skip) {
                    NLSFSEEK(fileptr, skip, SEEK_CUR);
                }
            }
        } else {
            str[sizeReallyRead] = 0;
            if (encoding != "UTF-8") {
                std::string asUtf8;
                bool res = charsetToUtf8Converter(str, encoding, asUtf8);
                delete[] str;
                if (res) {
                    toRead = ArrayOf::characterArrayConstructor(asUtf8);
                } else {
                    Error(_("Cannot to use encoding:") + encoding);
                }
            } else {
                toRead = ArrayOf::characterArrayConstructor(str);
                delete[] str;
            }
        }
    } else {
        void* ptr = ArrayOf::allocateArrayOf(
            classPrecision, static_cast<indexType>(sizeToRead), stringVector(), false);
        Dimensions dim(static_cast<indexType>(sizeToRead), 1);
        toRead = ArrayOf(classPrecision, dim, ptr);
        size_t count(toRead.getElementCount());
        size_t elsize(toRead.getElementSize());
        sizeReallyRead = static_cast<int>(fread(ptr, elsize, count, fileptr));
        if (sizeReallyRead < 0) {
            Dimensions dim(static_cast<indexType>(sizeToRead), 1);
            toRead.vectorResize(0);
            return toRead;
        }
        if (sizeReallyRead < sizeToRead) {
            void* ptrResize
                = ArrayOf::allocateArrayOf(classPrecision, sizeReallyRead, stringVector(), false);
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
