//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "FileSystemHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
NormalizeDriveLetter(const std::wstring& path, bool& isDrive)
{
    std::wstring wresult = path;
    if (wresult.length() == std::wstring(L"c:").length() && wresult[1] == L':') {
        wresult[0] = ::towupper(wresult[0]);
        wresult += L"/";
        isDrive = true;
        return wresult;
    }
    if (wresult.length() == std::wstring(L"c:/").length() && wresult[1] == L':'
        && (wresult[2] == L'\\' || wresult[2] == L'/')) {
        wresult[0] = ::towupper(wresult[0]);
        if (wresult[2] == L'\\') {
            wresult[2] = L'/';
        }
        isDrive = true;
        return wresult;
    }
    isDrive = false;
    return wresult;
}
//=============================================================================
inline std::wstring
NormalizePath(const std::wstring& path)
{
    if (path.empty()) {
        return {};
    }
    bool isDriveLetter;
    std::wstring wresult = NormalizeDriveLetter(path, isDriveLetter);
    if (isDriveLetter) {
        return wresult;
    }

    Nelson::FileSystemWrapper::Path _path(path);
    if (!_path.is_absolute()) {
        _path = Nelson::FileSystemWrapper::Path::current_path() / _path;
    }
    _path = _path.lexically_normal();

    nfs::path norm_path = nfs::path(_path.native());
    nfs::path prePath;
    nfs::path::iterator it = norm_path.begin();

    for (it; it != norm_path.end(); ++it) {
        if (nfs::exists(prePath / *it)) {
            prePath /= *it;
        } else {
            break;
        }
    }
#ifdef _MSC_VER
    std::wstring uniformizedPath = prePath.wstring();
#else
    std::wstring uniformizedPath = utf8_to_wstring(prePath.native());
#endif
    uniformizedPath = Nelson::FileSystemWrapper::Path::getFinalPathname(uniformizedPath);

#ifdef _MSC_VER
    nfs::path postPath(uniformizedPath);
#else
    nfs::path postPath(wstring_to_utf8(uniformizedPath));
#endif
    for (it; it != norm_path.end(); ++it) {
        postPath /= *it;
    }
#ifdef _MSC_VER
    uniformizedPath = postPath.wstring();
#else
    uniformizedPath = utf8_to_wstring(postPath.native());
#endif
    uniformizedPath = Nelson::FileSystemWrapper::Path(uniformizedPath).generic_wstring();
    if (uniformizedPath.length() > 1 && uniformizedPath.back() == L'/') {
        uniformizedPath.pop_back();
    }
    if (uniformizedPath.length() == std::wstring(L"c:").length()) {
        uniformizedPath += L"/";
    }
    return uniformizedPath;
}
//=============================================================================
}
//=============================================================================
