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
#include "CodeAnalyzer.hpp"
#include "nlsInterpreter_exports.h"
#include <filesystem>
//=============================================================================
namespace Nelson {
//=============================================================================
NLSINTERPRETER_IMPEXP bool
getCachedCodeAnalysis(const std::filesystem::path& path, const CodeAnalyzerOptions& options,
    std::vector<CodeAnalyzerDiagnostic>& diagnostics);
//=============================================================================
NLSINTERPRETER_IMPEXP void
storeCachedCodeAnalysis(const std::filesystem::path& path, const CodeAnalyzerOptions& options,
    const std::vector<CodeAnalyzerDiagnostic>& diagnostics);
//=============================================================================
} // namespace Nelson
//=============================================================================
