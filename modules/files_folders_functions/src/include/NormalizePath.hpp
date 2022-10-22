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
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/algorithm/string.hpp>
#include "FileSystemHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isDrive(const std::wstring& pathname)
{
#ifdef _MSC_VER
    return boost::algorithm::ends_with(pathname, L":/")
        || boost::algorithm::ends_with(pathname, L":\\")
        || (boost::algorithm::ends_with(pathname, L":")
            && (!boost::algorithm::contains(pathname, L".")
                && !boost::algorithm::contains(pathname, L"/")
                && !boost::algorithm::contains(pathname, L"\\")));
#else
    return false;
#endif
}
//=============================================================================
std::wstring
NormalizePath(const std::wstring& path)
{
    std::wstring uniformizedPath;
    if (path.empty()) {
        return uniformizedPath;
    }
    uniformizedPath = path;
    if (isDrive(uniformizedPath)) {
        boost::replace_all(uniformizedPath, L"\\", L"/");
        if (uniformizedPath.back() != L'/') {
            uniformizedPath = uniformizedPath + L"/";
        }
    } else {
        boost::filesystem::path absPath = boost::filesystem::absolute(path);
        boost::filesystem::path::iterator it = absPath.begin();
        boost::filesystem::path result = *it++;
        for (; exists(result / *it) && it != absPath.end(); ++it) {
            result /= *it;
        }
        result = boost::filesystem::canonical(result);
        for (; it != absPath.end(); ++it) {
            if (*it == "..") {
                result = result.parent_path();
            } else if (*it != ".") {
                result /= *it;
            }
        }
        uniformizedPath = result.generic_wstring();
    }
    return uniformizedPath;
}

/*
  inline std::wstring
NormalizePath(const std::wstring& path)
{
    if (path.empty()) {
        return {};
    }
#ifdef _MSC_VER
    std::filesystem::path abs_path = std::filesystem::absolute(path);
#else
    std::filesystem::path abs_path = std::filesystem::absolute(wstring_to_utf8(path));
#endif
    std::filesystem::path::iterator it = abs_path.begin();
    std::filesystem::path result = *it++;

    for (; exists(result) && it != abs_path.end(); ++it) {
        result /= *it;
    }

    result = std::filesystem::canonical(result.parent_path());

#ifdef _MSC_VER
#define DOTDOT L".."
#define DOT L"."
#else
#define DOTDOT ".."
#define DOT "."
#endif
    for (--it; it != abs_path.end(); ++it) {
        if (*it == DOTDOT) {
            result = result.parent_path();
        } else if (*it != DOT) {
            result /= *it;
        }
    }

#ifdef _MSC_VER
    std::wstring uniformizedPath = result.wstring();
#else
    std::wstring uniformizedPath = utf8_to_wstring(result.string());
#endif

    uniformizedPath = Nelson::FileSystemWrapper::Path::getFinalPathname(uniformizedPath);
    uniformizedPath = Nelson::FileSystemWrapper::Path(uniformizedPath).generic_wstring();
    if (uniformizedPath.length() > 1 && uniformizedPath.back() == L'/') {
        uniformizedPath.pop_back();
    }
    if (uniformizedPath.length() == std::wstring(L"c:").length()) {
        uniformizedPath += L"/";
    }
    return uniformizedPath;

}
*/
//=============================================================================
}
//=============================================================================
