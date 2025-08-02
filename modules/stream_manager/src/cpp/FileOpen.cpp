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
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <algorithm>
#include <vector>
#include "FileOpen.hpp"
#include "File.hpp"
#include "characters_encoding.hpp"
#include "FileSystemWrapper.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
getModeOsDependant(const std::wstring& mode)
{
    std::wstring newmode = mode;
#ifndef _MSC_VER
    StringHelpers::replace_all(newmode, L"t", L"");
#endif
    return newmode;
}
//=============================================================================
static bool
isValidMachineFormat(const std::wstring& machineFormat)
{
    wstringVector supportedMode;
    supportedMode.push_back(L"n");
    supportedMode.push_back(L"native");
    supportedMode.push_back(L"b");
    supportedMode.push_back(L"ieee-be");
    supportedMode.push_back(L"l");
    supportedMode.push_back(L"ieee-le");
    supportedMode.push_back(L"s");
    supportedMode.push_back(L"ieee-be.l64");
    supportedMode.push_back(L"a");
    supportedMode.push_back(L"ieee-le.l64");
    wstringVector::iterator it = find(supportedMode.begin(), supportedMode.end(), machineFormat);
    return (bool)(it != supportedMode.end());
}
//=============================================================================
static bool
isValidMode(const std::wstring& mode)
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
FileOpen(FilesManager* fm, const std::wstring& filename, const std::wstring& filemode,
    const std::wstring& machineFormat, const std::wstring& encoding, int& fileposition)
{
    FOPEN_ERROR_TYPE fopenError = FOPEN_NO_ERROR;
    if (filename.empty()) {
        fileposition = -1;
        fopenError = FOPEN_INVALID_NAME;
        return fopenError;
    }
    if (!isValidMode(filemode)) {
        fileposition = -1;
        fopenError = FOPEN_INVALID_MODE;
        return fopenError;
    }
    std::wstring _mode = filemode;
    if (_mode == L"w") {
        _mode = L"wb";
    }
    if (_mode == L"r") {
        _mode = L"rb";
    }
    if (_mode == L"a") {
        _mode = L"ab";
    }
    if (!isValidMachineFormat(machineFormat)) {
        fileposition = -1;
        fopenError = FOPEN_INVALID_MACHINE_FORMAT;
        return fopenError;
    }
    if (!isSupportedEncoding(wstring_to_utf8(encoding))) {
        fileposition = -1;
        fopenError = FOPEN_INVALID_ENCODING;
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
    std::string errorMessage;
    FileSystemWrapper::Path canonicalPath
        = FileSystemWrapper::Path::canonical(filename, errorMessage);
    if (!errorMessage.empty()) {
        canonicalPath = filename;
    }
#ifdef _MSC_VER
    FILE* fp = _wfopen(canonicalPath.wstring().c_str(), getModeOsDependant(_mode).c_str());
#else
    FILE* fp = fopen(wstring_to_utf8(canonicalPath.wstring()).c_str(),
        wstring_to_utf8(getModeOsDependant(filemode)).c_str());
#endif
    if (fp == nullptr) {
        delete file;
        fileposition = -1;
        fopenError = FOPEN_CANNOT_OPEN;
        return fopenError;
    }
    canonicalPath = FileSystemWrapper::Path::canonical(filename, errorMessage);
    if (!errorMessage.empty()) {
        canonicalPath = filename;
    }
    file->setFileName(canonicalPath.generic_wstring());
    file->setFileMode(_mode);
    file->setFilePointer((void*)fp);
    file->setEncoding(encoding);
    file->setMachineFormat(machineFormat);
    int pos = fm->addFile(file);
    if (pos == -1) {
        delete file;
        fileposition = -1;
        fopenError = FOPEN_IMPOSSIBLE_TO_ADD_FILE;
        return fopenError;
    }
    fileposition = pos;

    return fopenError;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
