//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <filesystem>
#include "GetCurrentDirectory.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
GetCurrentDirectory()
{
    std::wstring currentdir;
    try {
        std::filesystem::path pwd = std::filesystem::current_path();
        currentdir = pwd.generic_wstring();
    } catch (const std::filesystem::filesystem_error&) {
    }
    return currentdir;
}
} // namespace Nelson
//=============================================================================
