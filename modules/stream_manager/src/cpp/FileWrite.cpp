//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
#include "FileWrite.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "PredefinedErrorMessages.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
FWRITE_ERROR_TYPE
FileWrite(File* fp, ArrayOf src, NelsonType destClass, size_t skip, bool bIsLittleEndian,
    int& sizeWritten)
{
    FWRITE_ERROR_TYPE fwrite_error = FWRITE_NO_ERROR;
    sizeWritten = -1;
    if (fp) {
        switch (src.getDataClass()) {
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
        case NLS_CHAR: {
            ArrayOf toWrite(src);
            toWrite.promoteType(destClass);
            void* dp = toWrite.getReadWriteDataPointer();
            if (fp->isInterfaceMethod()) {
                auto* io = static_cast<Interface*>(fp->getFilePointer());
                if (skip != 0) {
                    fwrite_error = FWRITE_ENDIAN_CONVERSION_NOT_SUPPORTED;
                } else if ((fp->getFileName() == L"stdout") || (fp->getFileName() == L"stderr")) {
                    toWrite.promoteType(NLS_CHAR);
                    std::string str = toWrite.getContentAsCString();
                    if (bIsLittleEndian != isLittleEndianFormat()) {
                        for (char& k : str) {
                            k = bswap<char>(k);
                        }
                    }
                    if (fp->getFileName() == L"stdout") {
                        io->outputMessage(str);
                    } else {
                        io->errorMessage(str);
                    }
                    sizeWritten = static_cast<int>(str.size());
                    fwrite_error = FWRITE_NO_ERROR;
                } else {
                    fwrite_error = FWRITE_FILE_DESTINATION_NOT_SUPPORTED;
                }
            } else {
                FILE* filepointer = static_cast<FILE*>(fp->getFilePointer());
                if (filepointer) {
                    if (skip) {
                        char* skipdata = nullptr;
                        try {
                            skipdata = new char[skip];
                        } catch (const std::bad_alloc&) {
                            skipdata = nullptr;
                            Error(ERROR_MEMORY_ALLOCATION);
                        }
                        if (skipdata) {
                            memset(skipdata, 0, skip);
                            fwrite(skipdata, sizeof(char), skip, filepointer);
                            delete[] skipdata;
                            skipdata = nullptr;
                        }
                    }
                    size_t written = 0;
                    if ((destClass == src.getDataClass()) && destClass == NLS_CHAR) {
                        std::string str = toWrite.getContentAsCString();
                        if (bIsLittleEndian != isLittleEndianFormat()) {
                            for (char& k : str) {
                                k = bswap<char>(k);
                            }
                        }
                        std::string encoding = wstring_to_utf8(fp->getEncoding());
                        if (encoding != "UTF-8") {
                            std::string data;
                            if (utf8ToCharsetConverter(str, data, encoding)) {
                                written
                                    = fwrite(data.c_str(), sizeof(char), data.size(), filepointer);
                            } else {
                                written = 0;
                                fwrite_error = FWRITE_ERROR_ENCODING;
                                sizeWritten = static_cast<int>(written);
                                return fwrite_error;
                            }
                        } else {
                            written = fwrite(str.c_str(), sizeof(char), str.size(), filepointer);
                        }
                    } else {
                        size_t count(toWrite.getElementCount());
                        size_t elsize(toWrite.getElementSize());
                        if (bIsLittleEndian != isLittleEndianFormat()) {
                            BITSWAP(dp, count, destClass);
                        }
                        written = fwrite(dp, elsize, count, filepointer);
                    }
                    sizeWritten = static_cast<int>(written);
                }
                fwrite_error = FWRITE_NO_ERROR;
            }
        } break;
        default: {
            fwrite_error = FWRITE_DATA_TYPE_NOT_SUPPORTED;
        } break;
        }
    } else {
        fwrite_error = FWRITE_INVALID_FILE;
    }
    return fwrite_error;
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
        errorMessage = _W("Cannot open file.");
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
                errorMessage = _W("Encoding not supported.");
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
