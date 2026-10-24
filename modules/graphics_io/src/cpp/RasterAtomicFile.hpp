//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <filesystem>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
std::filesystem::path
rasterTemporaryPath(const std::filesystem::path& target);
bool
replaceFileAtomically(const std::filesystem::path& temporary, const std::filesystem::path& target,
    std::string& error);
//=============================================================================
} // namespace Nelson
//=============================================================================
