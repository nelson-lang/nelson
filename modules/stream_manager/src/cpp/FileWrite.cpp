//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
#include <memory>
#include <algorithm>
#include <cmath>
#include <cstdio>
#include "FileWrite.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isSupportedType(const ArrayOf& src);
static FWRITE_ERROR_TYPE
writeToInterface(File* fp, ArrayOf& toWrite, NelsonType destClass, size_t skip,
    bool bIsLittleEndian, int& sizeWritten, int& sizeByteWritten);
static FWRITE_ERROR_TYPE
writeToFile(File* fp, ArrayOf& toWrite, NelsonType destClass, size_t skip, bool bIsLittleEndian,
    int& sizeWritten, int& sizeByteWritten);
static bool
writeSkipBytes(FILE* filepointer, size_t skip);
static FWRITE_ERROR_TYPE
writeCharData(
    File* fp, ArrayOf& toWrite, bool bIsLittleEndian, int& sizeWritten, int& sizeByteWritten);
static FWRITE_ERROR_TYPE
writeBinaryData(FILE* filepointer, ArrayOf& toWrite, NelsonType destClass, bool bIsLittleEndian,
    int& sizeWritten, int& sizeByteWritten);
static int
computeApproxCharCount(size_t writtenBytes, size_t totalBytes, size_t totalChars);
//=============================================================================
FWRITE_ERROR_TYPE
FileWrite(File* fp, ArrayOf src, NelsonType destClass, size_t skip, bool bIsLittleEndian,
    int& sizeWritten, int& sizeByteWritten)
{
    sizeWritten = -1;
    sizeByteWritten = -1;

    if (!fp) {
        return FWRITE_INVALID_FILE;
    }

    if (!isSupportedType(src)) {
        return FWRITE_DATA_TYPE_NOT_SUPPORTED;
    }

    ArrayOf toWrite(src);
    toWrite.promoteType(destClass);

    if (fp->isInterfaceMethod()) {
        return writeToInterface(
            fp, toWrite, destClass, skip, bIsLittleEndian, sizeWritten, sizeByteWritten);
    } else {
        return writeToFile(
            fp, toWrite, destClass, skip, bIsLittleEndian, sizeWritten, sizeByteWritten);
    }
}
//=============================================================================
size_t
countUtf8CodePointsUpTo(const std::string& s, size_t bytes)
{
    const size_t n = std::min(bytes, s.size());
    size_t i = 0;
    size_t count = 0;

    while (i < n) {
        unsigned char c = static_cast<unsigned char>(s[i]);
        size_t cpLen = 0;

        if ((c & 0x80) == 0x00) {
            cpLen = 1;
        } else if ((c & 0xE0) == 0xC0) {
            cpLen = 2;
        } else if ((c & 0xF0) == 0xE0) {
            cpLen = 3;
        } else if ((c & 0xF8) == 0xF0) {
            cpLen = 4;
        } else {
            break; // invalid leading byte
        }

        if (i + cpLen > n) {
            break;
        }

        bool valid = true;
        for (size_t k = 1; k < cpLen; ++k) {
            unsigned char cc = static_cast<unsigned char>(s[i + k]);
            if ((cc & 0xC0) != 0x80) {
                valid = false;
                break;
            }
        }

        if (!valid) {
            break;
        }

        // reject simple overlong forms (stricter)
        if (cpLen == 2 && (c & 0x1E) == 0) {
            break;
        }
        if (cpLen == 3 && (c == 0xE0 && (static_cast<unsigned char>(s[i + 1]) & 0x20) == 0)) {
            break;
        }
        if (cpLen == 4 && (c == 0xF0 && (static_cast<unsigned char>(s[i + 1]) & 0x30) == 0)) {
            break;
        }

        ++count;
        i += cpLen;
    }

    return count;
}
//=============================================================================
bool
isSupportedType(const ArrayOf& src)
{
    switch (src.dataClass()) {
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    case NLS_CHAR:
        return true;
    default:
        return false;
    }
}
//=============================================================================
FWRITE_ERROR_TYPE
writeToInterface(File* fp, ArrayOf& toWrite, NelsonType /*destClass*/, size_t skip,
    bool bIsLittleEndian, int& sizeWritten, int& sizeByteWritten)
{
    // The interface supports neither skip nor endian conversion for binary data.
    if (skip != 0) {
        sizeWritten = -1;
        return FWRITE_ENDIAN_CONVERSION_NOT_SUPPORTED;
    }

    auto* io = static_cast<Interface*>(fp->getFilePointer());
    const auto& name = fp->getFileName();

    if (name != L"stdout" && name != L"stderr") {
        sizeWritten = -1;
        sizeByteWritten = -1;
        return FWRITE_FILE_DESTINATION_NOT_SUPPORTED;
    }

    toWrite.promoteType(NLS_CHAR);
    std::wstring wstr = toWrite.asWideString();
    std::string str = toWrite.asUtf8String();

    if (bIsLittleEndian != isLittleEndianFormat()) {
        for (char& c : str) {
            c = bswap<char>(c);
        }
    }

    if (name == L"stdout") {
        io->outputMessage(str);
    } else {
        io->errorMessage(str);
    }

    sizeWritten = static_cast<int>(wstr.size());
    sizeByteWritten = static_cast<int>(str.size());
    return FWRITE_NO_ERROR;
}
//=============================================================================
FWRITE_ERROR_TYPE
writeToFile(File* fp, ArrayOf& toWrite, NelsonType destClass, size_t skip, bool bIsLittleEndian,
    int& sizeWritten, int& sizeByteWritten)
{
    FILE* filepointer = static_cast<FILE*>(fp->getFilePointer());
    if (!filepointer) {
        sizeWritten = -1;
        return FWRITE_INVALID_FILE;
    }

    if (skip > 0) {
        if (!writeSkipBytes(filepointer, skip)) {
            sizeWritten = -1;
            // writeSkipBytes already raised the error; the result stays a success.
            return FWRITE_NO_ERROR;
        }
    }
    if ((destClass == toWrite.dataClass()) && destClass == NLS_CHAR) {
        return writeCharData(fp, toWrite, bIsLittleEndian, sizeWritten, sizeByteWritten);
    } else {
        return writeBinaryData(
            filepointer, toWrite, destClass, bIsLittleEndian, sizeWritten, sizeByteWritten);
    }
}
//=============================================================================
bool
writeSkipBytes(FILE* filepointer, size_t skip)
{
    try {
        std::vector<char> buf(skip, 0);
        size_t wrote = std::fwrite(buf.data(), sizeof(char), skip, filepointer);
        // A partial skip write is not fatal.
        (void)wrote;
        return true;
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
        return false;
    }
}
//=============================================================================
FWRITE_ERROR_TYPE
writeCharData(
    File* fp, ArrayOf& toWrite, bool bIsLittleEndian, int& sizeWritten, int& sizeByteWritten)
{
    FILE* filepointer = static_cast<FILE*>(fp->getFilePointer());
    if (!filepointer) {
        sizeWritten = -1;
        return FWRITE_INVALID_FILE;
    }

    std::string str = toWrite.asUtf8String();

    if (bIsLittleEndian != isLittleEndianFormat()) {
        for (char& c : str) {
            c = bswap<char>(c);
        }
    }

    const std::string encoding = wstring_to_utf8(fp->getEncoding());

    if (encoding != "UTF-8") {
        std::string data;
        if (!utf8ToCharsetConverter(str, data, encoding)) {
            sizeWritten = 0;
            return FWRITE_ERROR_ENCODING;
        }

        sizeByteWritten = (int)std::fwrite(data.c_str(), sizeof(char), data.size(), filepointer);

        std::wstring wstr = toWrite.asWideString();
        sizeWritten = computeApproxCharCount(sizeByteWritten, data.size(), wstr.size());

    } else {
        sizeByteWritten = (int)std::fwrite(str.c_str(), sizeof(char), str.size(), filepointer);
        if (sizeByteWritten == str.size()) {
            std::wstring wstr = toWrite.asWideString();
            sizeWritten = static_cast<int>(wstr.size());
        } else {
            sizeWritten = static_cast<int>(countUtf8CodePointsUpTo(str, sizeByteWritten));
        }
    }
    return FWRITE_NO_ERROR;
}
//=============================================================================
FWRITE_ERROR_TYPE
writeBinaryData(FILE* filepointer, ArrayOf& toWrite, NelsonType destClass, bool bIsLittleEndian,
    int& sizeWritten, int& sizeByteWritten)
{
    void* dp = toWrite.readWriteDataPointer();
    const size_t count = toWrite.elementCount();
    const size_t elsize = toWrite.elementSize();

    if (bIsLittleEndian != isLittleEndianFormat()) {
        BITSWAP(dp, count, destClass);
    }

    size_t written = std::fwrite(dp, elsize, count, filepointer);
    sizeWritten = static_cast<int>(written);
    sizeByteWritten = sizeWritten;
    return FWRITE_NO_ERROR;
}
//=============================================================================
int
computeApproxCharCount(size_t writtenBytes, size_t totalBytes, size_t totalChars)
{
    if (totalBytes == 0) {
        return 0;
    }
    if (writtenBytes >= totalBytes) {
        return static_cast<int>(totalChars);
    }

    double ratio = static_cast<double>(writtenBytes) / static_cast<double>(totalBytes);
    return static_cast<int>(std::floor(ratio * static_cast<double>(totalChars)));
}
//=============================================================================
bool
writeFile(const std::wstring& filename, const wstringVector& lines, const std::wstring& weol,
    const std::string& eol, const std::string& encoding, std::wstring& errorMessage)
{
#ifdef _MSC_VER
    std::ofstream of(filename, std::ios::trunc | std::ios::binary);
#else
    std::ofstream of(wstring_to_utf8(filename), std::ios::trunc | std::ios::binary);
#endif
    if (!of.is_open()) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:stream_manager:cannotOpenFile", L"Cannot open file.");
        return false;
    }
    for (size_t k = 0; k < lines.size(); ++k) {
        std::wstring line = lines[k];

        StringHelpers::replace_all(line, L"\r\n", L"\n");
        StringHelpers::replace_all(line, L"\n", weol);

        std::string data;
        if (encoding == "UTF-8") {
            data = wstring_to_utf8(line);
        } else {
            std::string asUtf8 = wstring_to_utf8(line);
            if (!utf8ToCharsetConverter(asUtf8, data, encoding)) {
                of.flush();
                errorMessage = resolveMessageTemplate(
                    L"Nelson:stream_manager:encodingSupported", L"Encoding not supported.");
                return false;
            }
        }
        of << data;
        if (!StringHelpers::ends_with(data, eol) && k != lines.size() - 1) {
            of << eol;
        }
    }
    of.flush();
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
