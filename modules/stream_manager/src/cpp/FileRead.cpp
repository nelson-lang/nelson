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
        char* buffer = nullptr;
        try {
            buffer = new char[static_cast<indexType>(sizeToRead + 1)];
        } catch (const std::bad_alloc&) {
            buffer = nullptr;
            Error(ERROR_MEMORY_ALLOCATION);
        }
        auto count = static_cast<size_t>(sizeToRead);
        size_t elsize = sizeof(char);
        if (buffer) {
            sizeReallyRead = static_cast<int>(fread(buffer, elsize, count, fileptr));
        }
        if (sizeReallyRead < 0) {
            delete[] buffer;
        } else {
            size_t validLength = static_cast<size_t>(sizeReallyRead);
            if (sizeReallyRead < sizeToRead) {
                char* trimmed = nullptr;
                try {
                    trimmed = new char[validLength + 1];
                } catch (const std::bad_alloc&) {
                    delete[] buffer;
                    Error(ERROR_MEMORY_ALLOCATION);
                }
                if (buffer && trimmed) {
                    memcpy(trimmed, buffer, validLength * sizeof(char));
                }
                delete[] buffer;
                buffer = trimmed;
            }
            if (buffer) {
                buffer[validLength] = 0;
            }
            if (buffer && (bIsLittleEndian != isLittleEndianFormat())) {
                for (size_t k = 0; k < validLength; k++) {
                    buffer[k] = bswap<char>(buffer[k]);
                }
            }
            std::string raw;
            if (buffer) {
                raw.assign(buffer, validLength);
            }
            delete[] buffer;
            std::string utf8Data;
            if (encoding != "UTF-8") {
                bool converted = charsetToUtf8Converter(raw, encoding, utf8Data);
                if (!converted) {
                    Error(_("Cannot to use encoding:") + encoding);
                }
            } else {
                utf8Data = raw;
            }
            toRead = ArrayOf::characterArrayConstructor(utf8Data);
            if (!feof(fileptr)) {
                if (skip) {
                    NLSFSEEK(fileptr, skip, SEEK_CUR);
                }
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
