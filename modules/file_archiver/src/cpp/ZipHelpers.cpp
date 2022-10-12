//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/algorithm/string.hpp>
#include "FileSystemHelpers.hpp"
#include "ZipHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
normalizeZipPath(const std::wstring& path)
{
    std::filesystem::path p = createFileSystemPath(path);
    p = p.lexically_normal();
    if (boost::algorithm::starts_with(convertFileSytemPathToGenericWString(p), L"./")) {
        p = createFileSystemPath(convertFileSytemPathToGenericWString(p).substr(2));
    }
    return convertFileSytemPathToGenericWString(p);
}
//=============================================================================
std::wstring
getRootPath(const std::wstring& rootpath)
{
    std::filesystem::path p;
    if (rootpath == L".") {
        p = std::filesystem::current_path();
    } else {
        p = createFileSystemPath(rootpath);
    }
    return normalizeZipPath(convertFileSytemPathToWString(p));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
