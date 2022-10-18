//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "GetCurrentDirectory.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
GetCurrentDirectory()
{
    std::wstring currentdir;
    try {
        Nelson::FileSystemWrapper::Path pwd = Nelson::FileSystemWrapper::Path::current_path();
        currentdir = pwd.generic_wstring();
    } catch (const boost::filesystem::filesystem_error&) {
    }
    return currentdir;
}
} // namespace Nelson
//=============================================================================
