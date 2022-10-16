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
#include <boost/algorithm/string.hpp>
#include "FileSystemHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline std::wstring
NormalizePath(const std::wstring& path)
{
    std::wstring uniformizedPath;
    if (path.empty()) {
        return uniformizedPath;
    }
    uniformizedPath = path;
    std::filesystem::path result = createFileSystemPath(uniformizedPath);
    result = std::filesystem::absolute(result);
    result = result.lexically_normal();
    uniformizedPath = convertFileSytemPathToGenericWString(result);
    if (isDirectory(uniformizedPath)) {
        if (uniformizedPath.back() != L'/') {
            uniformizedPath += L"/";
        }
    }
    if (boost::algorithm::starts_with(uniformizedPath, L"./")) {
        uniformizedPath
            = convertFileSytemPathToGenericWString(createFileSystemPath(uniformizedPath.substr(2)));
    }

    return uniformizedPath;
}
//=============================================================================
}
//=============================================================================
