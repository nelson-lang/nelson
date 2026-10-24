//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#pragma once
//=============================================================================
#include "nlsCompiler_exports.h"
#include "Types.hpp"
#include <string>
//=============================================================================
namespace Nelson {
NLSCOMPILER_IMPEXP void
createApplicationArchive(const std::wstring& stub, const std::wstring& archive,
    const std::wstring& destination, bool gui, const std::string& installedEngineFingerprint = {},
    bool embedArchive = true);
NLSCOMPILER_IMPEXP void
packApplicationArchive(
    const std::wstring& archive, const wstringVector& files, const std::wstring& root);
//=============================================================================
} // namespace Nelson
//=============================================================================
