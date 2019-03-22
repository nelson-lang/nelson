//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#ifdef _MSC_VER
#include <windows.h>
#endif
#include "MapFileRead.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/filesystem.hpp>
#include <boost/iostreams/device/mapped_file.hpp>
#include <cstdio>
//=============================================================================
namespace Nelson {
//=============================================================================
#ifdef _MSC_VER
static LPVOID
_MapFileRead(LPCWSTR szFileName, size_t* lpcbSize, BOOL& isEmpty)
{
    HANDLE hFile, hMapping;
    DWORD dwFileSize;
    LPVOID lpView;
    MEMORY_BASIC_INFORMATION mbi;
    isEmpty = FALSE;
    *lpcbSize = 0;
    hFile = CreateFileW(szFileName, GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, nullptr);
    if (INVALID_HANDLE_VALUE == hFile) {
        return nullptr;
    }
    dwFileSize = GetFileSize(hFile, nullptr);
    if (INVALID_FILE_SIZE == dwFileSize) {
        CloseHandle(hFile);
        return nullptr;
    }
    if (dwFileSize == 0) {
        isEmpty = TRUE;
        CloseHandle(hFile);
        return nullptr;
    }
    hMapping = CreateFileMappingW(hFile, nullptr, PAGE_READONLY, 0, 0, nullptr);
    if (nullptr == hMapping) {
        CloseHandle(hFile);
        return nullptr;
    }
    lpView = MapViewOfFile(hMapping, FILE_MAP_READ, 0, 0, 0);
    CloseHandle(hMapping);
    CloseHandle(hFile);
    if (nullptr != lpView) {
        if (VirtualQuery(lpView, &mbi, sizeof(mbi)) >= sizeof(mbi)) {
            *lpcbSize = min(dwFileSize, mbi.RegionSize);
        } else {
            *lpcbSize = dwFileSize;
        }
    }
    return lpView;
}
//=============================================================================
static BOOL
_MapFileClose(LPCVOID lpView)
{
    return UnmapViewOfFile(lpView);
}
//=============================================================================
ArrayOf
MapFileRead(const std::wstring& filename, const std::wstring& eol, std::wstring& errorMessage)
{
    errorMessage = L"";
    size_t cbSize = 0;
    BOOL isEmpty = FALSE;
    const char* fileView
        = static_cast<const char*>(_MapFileRead(filename.c_str(), &cbSize, isEmpty));
    ArrayOf res;
    if (isEmpty) {
        res = ArrayOf::characterArrayConstructor("");
    } else {
        if (fileView) {
            try {
                std::string content(fileView, cbSize);
                boost::replace_all(content, "\r\n", "\n");
                if (eol != L"\n") {
                    boost::replace_all(content, "\n", wstring_to_utf8(eol));
                }
                res = ArrayOf::characterArrayConstructor(content);
            } catch (...) {
                errorMessage = _W("Cannot read file.");
            }
            _MapFileClose(fileView);
        } else {
            errorMessage = _W("Cannot open file.");
        }
    }
    return res;
}
//=============================================================================
#else
bool
isEmptyFile(std::wstring filename)
{
    std::wifstream wif(wstring_to_utf8(filename), std::ios::binary);
    wif.seekg(0, std::ios::end);
    return !(wif.tellg() > 0);
}
//=============================================================================
ArrayOf
MapFileRead(const std::wstring& filename, const std::wstring& eol, std::wstring& errorMessage)
{
    ArrayOf res;
    errorMessage.clear();
    if (isEmptyFile(filename)) {
        res = ArrayOf::characterArrayConstructor("");
    } else {
        boost::filesystem::path fileAsPath(filename);
        boost::iostreams::basic_mapped_file_params<boost::filesystem::path> param;
        param.path = fileAsPath;
        boost::iostreams::mapped_file_source mappedFile(param);
        if (mappedFile.is_open()) {
            std::string content(mappedFile.data(), mappedFile.size());
            mappedFile.close();
            boost::replace_all(content, L"\r\n", L"\n");
            if (eol != L"\n") {
                boost::replace_all(content, L"\n", eol);
            }
            res = ArrayOf::characterArrayConstructor(content);
        } else {
            errorMessage = _W("Cannot open file.");
        }
    }
    return res;
}
#endif
//=============================================================================
} // namespace Nelson
//=============================================================================
