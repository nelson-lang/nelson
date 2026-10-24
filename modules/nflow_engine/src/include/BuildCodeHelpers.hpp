//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsNflow_engine_exports.h"
#include <string>
//=============================================================================
namespace nflow {
//=============================================================================
NLSNFLOW_ENGINE_IMPEXP bool
haveCToolChain(std::wstring& errorMessage);
//=============================================================================
NLSNFLOW_ENGINE_IMPEXP bool
haveRustToolChain(std::wstring& errorMessage);
//=============================================================================
NLSNFLOW_ENGINE_IMPEXP bool
callCMake(const std::wstring& cmakeFilePath, const std::wstring& options,
    const std::wstring& buildDir, std::wstring& errorMessage,
    const std::wstring& buildConfig = L"Debug");
//=============================================================================
NLSNFLOW_ENGINE_IMPEXP bool
callCargoToml(const std::wstring& cargoFilePath, const std::wstring& options,
    const std::wstring& buildDir, std::wstring& errorMessage);
//=============================================================================
// Returns cmake -D defines pointing to the nflow_blocks headers so that
// generated plugin projects can be built from any external directory.
NLSNFLOW_ENGINE_IMPEXP std::wstring
getNflowCMakeDefines();
//=============================================================================
} // namespace nflow
//=============================================================================
