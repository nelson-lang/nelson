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
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
NormalizePath(const std::wstring& path)
{
    std::wstring uniformizedPath;
    if (path.empty()) {
        return uniformizedPath;
    }
    uniformizedPath = path;

    boost::filesystem::path absPath = boost::filesystem::absolute(boost::filesystem::path(path));
    boost::filesystem::path::iterator it = absPath.begin();
    boost::filesystem::path result = *it++;
    for (; boost::filesystem::exists(result / *it) && it != absPath.end(); ++it) {
        result /= *it;
    }
    result = boost::filesystem::canonical(result);
#ifdef _MSC_VER
#define DOT_FILE L"."
#define DOT_DOT_FILE L".."
#else
#define DOT_FILE "."
#define DOT_DOT_FILE ".."
#endif
    for (; it != absPath.end(); ++it) {
        if (*it == DOT_DOT_FILE) {
            result = result.parent_path();
        } else if (*it != DOT_FILE) {
            result /= *it;
        }
    }

    uniformizedPath = result.generic_wstring();
    return uniformizedPath;
}
//=============================================================================
}
//=============================================================================
