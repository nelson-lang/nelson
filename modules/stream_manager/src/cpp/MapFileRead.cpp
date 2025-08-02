//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdio>
#include <fstream>
#include <boost/filesystem.hpp>
#include <boost/iostreams/device/mapped_file.hpp>
#include "StringHelpers.hpp"
#include "MapFileRead.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
isEmptyFile(const std::wstring& filename)
{
#ifdef _MSC_VER
    std::wifstream wif(filename, std::ios::binary);
#else
    std::wifstream wif(wstring_to_utf8(filename), std::ios::binary);
#endif
    wif.seekg(0, std::ios::end);
    return !(wif.tellg() > 0);
}
//=============================================================================
bool
MapFileRead(const std::wstring& filename, const std::wstring& eol, const std::wstring& encoding,
    std::wstring& content, std::wstring& errorMessage)
{
    bool res = false;
    content.clear();
    errorMessage.clear();
    if (isEmptyFile(filename)) {
        res = true;
        content = L"";
    } else {
        boost::filesystem::path fileAsPath(filename);
        boost::iostreams::basic_mapped_file_params<boost::filesystem::path> param;
        param.path = fileAsPath.native();
        boost::iostreams::mapped_file_source mappedFile(param);
        if (mappedFile.is_open()) {
            std::string data(mappedFile.data(), mappedFile.size());
            mappedFile.close();
            if (encoding == L"auto") {
                std::string asUtf8;
                std::string encodingDetected = detectBestEncoding(data);
                if (!charsetToUtf8Converter(data, encodingDetected, asUtf8)) {
                    errorMessage = _W("Cannot convert to unicode.");
                }
                StringHelpers::replace_all(asUtf8, "\r\n", "\n");
                if (eol != L"\n") {
                    StringHelpers::replace_all(asUtf8, "\n", wstring_to_utf8(eol));
                }
                res = true;
                content = utf8_to_wstring(asUtf8);
            } else if (encoding == L"UTF-8") {
                StringHelpers::replace_all(data, "\r\n", "\n");
                if (eol != L"\n") {
                    StringHelpers::replace_all(data, "\n", wstring_to_utf8(eol));
                }
                res = true;
                content = utf8_to_wstring(data);
            } else {
                std::string asUtf8;
                if (!charsetToUtf8Converter(data, wstring_to_utf8(encoding), asUtf8)) {
                    errorMessage = _W("Cannot convert to unicode.");
                }
                StringHelpers::replace_all(asUtf8, "\r\n", "\n");
                if (eol != L"\n") {
                    StringHelpers::replace_all(asUtf8, "\n", wstring_to_utf8(eol));
                }
                res = true;
                content = utf8_to_wstring(asUtf8);
            }
        } else {
            errorMessage = _W("Cannot open file.");
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
