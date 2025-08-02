//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "RelativePath.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
RelativePath(const std::wstring& path1, const std::wstring& path2, bool& bSuccess)
{
    bSuccess = false;
    FileSystemWrapper::Path pathOne(path1);
    FileSystemWrapper::Path pathTwo(path2);
    FileSystemWrapper::Path relativepath;
    pathOne = pathOne.lexically_normal();
    pathTwo = pathTwo.lexically_normal();
    relativepath = pathTwo.lexically_relative(pathOne);
    std::wstring result = relativepath.generic_wstring();
    if (result.empty()) {
        result = pathTwo.generic_wstring();
        bSuccess = false;
    } else {
        bSuccess = true;
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
