//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#include "FileOpen.hpp"
#include "File.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <boost/container/vector.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
getModeOsDependant(std::wstring mode)
{
    std::wstring newmode(mode);
#ifndef _MSC_VER
    boost::replace_all(newmode, L"t", L"");
#endif
    return newmode;
}
//=============================================================================
static bool
isValidMode(std::wstring mode)
{
    wstringVector supportedMode;
    supportedMode.push_back(L"r");
    supportedMode.push_back(L"w");
    supportedMode.push_back(L"a");
    supportedMode.push_back(L"rt");
    supportedMode.push_back(L"wt");
    supportedMode.push_back(L"at");
    supportedMode.push_back(L"rb");
    supportedMode.push_back(L"wb");
    supportedMode.push_back(L"ab");
    supportedMode.push_back(L"r+");
    supportedMode.push_back(L"rb+");
    supportedMode.push_back(L"r+b");
    supportedMode.push_back(L"rt+");
    supportedMode.push_back(L"r+t");
    supportedMode.push_back(L"w+");
    supportedMode.push_back(L"wb+");
    supportedMode.push_back(L"w+b");
    supportedMode.push_back(L"wt+");
    supportedMode.push_back(L"w+t");
    supportedMode.push_back(L"a+");
    supportedMode.push_back(L"ab+");
    supportedMode.push_back(L"a+b");
    supportedMode.push_back(L"at+");
    supportedMode.push_back(L"a+t");
    wstringVector::iterator it = find(supportedMode.begin(), supportedMode.end(), mode);
    return (bool)(it != supportedMode.end());
}
//=============================================================================
FOPEN_ERROR_TYPE
FileOpen(FilesManager* fm, std::wstring filename, std::wstring filemode, int& fileposition)
{
    FOPEN_ERROR_TYPE fopenError = FOPEN_NO_ERROR;
    if (filename.size() == 0) {
        fileposition = -1;
        fopenError = FOPEN_INVALID_NAME;
        return fopenError;
    }
    if (!isValidMode(filemode)) {
        fileposition = -1;
        fopenError = FOPEN_INVALID_MODE;
        return fopenError;
    }
    File* file;
    try {
        file = new File();
    } catch (const std::bad_alloc&) {
        file = nullptr;
    }
    if (!file) {
        fileposition = -1;
        fopenError = FOPEN_IMPOSSIBLE_TO_ADD_FILE;
        return fopenError;
    }
    boost::filesystem::path canonicalPath;
    try {
        canonicalPath = boost::filesystem::canonical(filename, boost::filesystem::current_path());
    } catch (const boost::filesystem::filesystem_error& e) {
        e.what();
        canonicalPath = filename;
    }
#ifdef _MSC_VER
    FILE* fp = _wfopen(canonicalPath.wstring().c_str(), getModeOsDependant(filemode).c_str());
#else
    FILE* fp = fopen(wstring_to_utf8(canonicalPath.wstring()).c_str(),
        wstring_to_utf8(getModeOsDependant(filemode)).c_str());
#endif
    if (fp == nullptr) {
        delete file;
        fileposition = -1;
        fopenError = FOPEN_CANNOT_OPEN;
        return fopenError;
    } else {
        try {
            canonicalPath
                = boost::filesystem::canonical(filename, boost::filesystem::current_path());
        } catch (const boost::filesystem::filesystem_error& e) {
            e.what();
            canonicalPath = filename;
        }
        file->setFileName(canonicalPath.wstring());
        file->setFileMode(filemode);
        file->setFilePointer((void*)fp);
        int pos = fm->addFile(file);
        if (pos == -1) {
            delete file;
            fileposition = -1;
            fopenError = FOPEN_IMPOSSIBLE_TO_ADD_FILE;
            return fopenError;
        }
        fileposition = pos;
    }
    return fopenError;
}
//=============================================================================
};
//=============================================================================
