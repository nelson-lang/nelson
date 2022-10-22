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
    Nelson::FileSystemWrapper::Path pwd = Nelson::FileSystemWrapper::Path::current_path();
    return pwd.getFinalPathname().generic_wstring();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
