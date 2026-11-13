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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "FileSeek.hpp"
#include "FileTell.hpp"
#include "characters_encoding.hpp"
#include "FileBackend.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
readFromBackend(File* fp, FileBackend* backend, int64 sizeToRead, NelsonType classPrecision,
    size_t skip, bool bIsLittleEndian, int& sizeReallyRead);
static ArrayOf
charactersFromBytes(const std::string& raw, const std::wstring& encoding);
static size_t
precisionSize(NelsonType classPrecision);
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
    if (FileBackend* backend = fp->getBackend()) {
        return readFromBackend(
            fp, backend, sizeToRead, classPrecision, skip, bIsLittleEndian, sizeReallyRead);
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
        case NLS_CLASSDEF_ARRAY:
        case NLS_FUNCTION_HANDLE:
        case NLS_STRING_ARRAY:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        default: {
        } break;
        }
    }
    if (classPrecision == NLS_CHAR) {
        char* buffer = nullptr;
        try {
            buffer = new char[static_cast<indexType>(sizeToRead + 1)];
        } catch (const std::bad_alloc&) {
            buffer = nullptr;
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
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
                    Error(L"Nelson:outOfMemory",
                        L"Memory allocation error... You may have run out of memory!");
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
            toRead = charactersFromBytes(raw, fp->getEncoding());
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
        size_t count(toRead.elementCount());
        size_t elsize(toRead.elementSize());
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
            memcpy(ptrResize, ptr, toRead.elementSize() * sizeReallyRead);
            toRead = ArrayOf(classPrecision, dim, ptrResize);
        }
        if (bIsLittleEndian != isLittleEndianFormat()) {
            BITSWAP(toRead.readWriteDataPointer(), sizeReallyRead, classPrecision);
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
// Same results as the C file path, for bytes produced by a backend.
static ArrayOf
readFromBackend(File* fp, FileBackend* backend, int64 sizeToRead, NelsonType classPrecision,
    size_t skip, bool bIsLittleEndian, int& sizeReallyRead)
{
    const size_t elementSize = precisionSize(classPrecision);
    std::string raw;
    if (sizeToRead < 0) {
        char chunk[64 * 1024];
        for (size_t count = backend->read(chunk, sizeof(chunk)); count > 0;
            count = backend->read(chunk, sizeof(chunk))) {
            raw.append(chunk, count);
        }
    } else {
        raw.resize(static_cast<size_t>(sizeToRead) * elementSize);
        raw.resize(backend->read(raw.data(), raw.size()));
    }
    const size_t count = raw.size() / elementSize;
    sizeReallyRead = static_cast<int>(count);
    ArrayOf result;
    if (classPrecision == NLS_CHAR) {
        result = charactersFromBytes(raw, fp->getEncoding());
    } else {
        void* ptr = ArrayOf::allocateArrayOf(
            classPrecision, static_cast<indexType>(count), stringVector(), false);
        std::memcpy(ptr, raw.data(), count * elementSize);
        result = ArrayOf(classPrecision, Dimensions(static_cast<indexType>(count), 1), ptr);
        if (bIsLittleEndian != isLittleEndianFormat()) {
            BITSWAP(result.readWriteDataPointer(), count, classPrecision);
        }
    }
    if (skip && !backend->eof()) {
        backend->seek(backend->tell() + static_cast<int64>(skip));
    }
    return result;
}
//=============================================================================
static ArrayOf
charactersFromBytes(const std::string& raw, const std::wstring& encoding)
{
    const std::string charset = wstring_to_utf8(encoding);
    if (charset == "UTF-8") {
        return ArrayOf::characterArray(raw);
    }
    std::string utf8Data;
    if (!charsetToUtf8Converter(raw, charset, utf8Data)) {
        Error("Nelson:stream_manager:invalidEncoding", "Cannot use encoding: %s", charset);
    }
    return ArrayOf::characterArray(utf8Data);
}
//=============================================================================
static size_t
precisionSize(NelsonType classPrecision)
{
    switch (classPrecision) {
    case NLS_LOGICAL:
        return sizeof(logical);
    case NLS_UINT16:
    case NLS_INT16:
        return sizeof(uint16);
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_SINGLE:
        return sizeof(uint32);
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_DOUBLE:
        return sizeof(uint64);
    default:
        return sizeof(uint8);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
