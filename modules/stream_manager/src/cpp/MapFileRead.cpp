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
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/filesystem.hpp>
#include <boost/iostreams/device/mapped_file.hpp>
#include <cstdio>
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
                boost::replace_all(asUtf8, L"\r\n", L"\n");
                if (eol != L"\n") {
                    boost::replace_all(asUtf8, L"\n", eol);
                }
                res = true;
                content = utf8_to_wstring(asUtf8);
            } else if (encoding == L"UTF-8") {
                boost::replace_all(data, L"\r\n", L"\n");
                if (eol != L"\n") {
                    boost::replace_all(data, L"\n", eol);
                }
                res = true;
                content = utf8_to_wstring(data);
            } else {
                std::string asUtf8;
                if (!charsetToUtf8Converter(data, wstring_to_utf8(encoding), asUtf8)) {
                    errorMessage = _W("Cannot convert to unicode.");
                }
                boost::replace_all(asUtf8, L"\r\n", L"\n");
                if (eol != L"\n") {
                    boost::replace_all(asUtf8, L"\n", eol);
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
