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
#include <cwchar>
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
writeFile(const std::wstring& filename, const std::vector<TextSpan>& lines, const std::wstring& eol,
    const std::string& encoding, std::wstring& errorMessage)
{
    std::string eolUtf8;
    appendUtf8(eolUtf8, eol.data(), eol.size());
    size_t total = eolUtf8.size() * lines.size();
    for (const TextSpan& line : lines) {
        total += line.length;
    }
    std::string text;
    text.reserve(total);
    for (size_t k = 0; k < lines.size(); ++k) {
        const wchar_t* data = lines[k].data;
        size_t length = lines[k].length;
        size_t lineStart = text.size();
        // LF and CR LF inside a line become the requested end of line.
        size_t segment = 0;
        while (segment < length) {
            const wchar_t* lf = std::wmemchr(data + segment, L'\n', length - segment);
            if (lf == nullptr) {
                break;
            }
            auto pos = static_cast<size_t>(lf - data);
            size_t segmentEnd = (pos > segment && data[pos - 1] == L'\r') ? pos - 1 : pos;
            appendUtf8(text, data + segment, segmentEnd - segment);
            text += eolUtf8;
            segment = pos + 1;
        }
        if (segment < length) {
            appendUtf8(text, data + segment, length - segment);
        }
        bool endsWithEol = text.size() - lineStart >= eolUtf8.size()
            && text.compare(text.size() - eolUtf8.size(), eolUtf8.size(), eolUtf8) == 0;
        if (!endsWithEol && k + 1 != lines.size()) {
            text += eolUtf8;
        }
    }
    switch (writeUtf8TextFile(filename, text, encoding, false)) {
    case TextWriteStatus::OK:
        return true;
    case TextWriteStatus::ENCODING_FAILED:
        errorMessage = resolveMessageTemplate(
            L"Nelson:stream_manager:encodingSupported", L"Encoding not supported.");
        return false;
    case TextWriteStatus::OPEN_FAILED:
        errorMessage
            = resolveMessageTemplate(L"Nelson:stream_manager:cannotOpenFile", L"Cannot open file.");
        return false;
    case TextWriteStatus::WRITE_FAILED:
    default:
        errorMessage = resolveMessageTemplate(
            L"Nelson:stream_manager:writeFailed", L"Unable to write all data to the file.");
        return false;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
