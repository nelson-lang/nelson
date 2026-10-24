//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#pragma once
//=============================================================================
#include "nlsCompiler_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSCOMPILER_IMPEXP std::string
collectApplicationInstallerInputs(const std::wstring& encoded);
NLSCOMPILER_IMPEXP std::string
normalizeApplicationBuildReport(const std::wstring& encoded);
NLSCOMPILER_IMPEXP std::string
mergeApplicationRuntimeReports(const std::wstring& encoded, bool verifyFiles);
//=============================================================================
} // namespace Nelson
//=============================================================================
