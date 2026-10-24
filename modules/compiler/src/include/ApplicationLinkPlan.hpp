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
#include <string>
#include <vector>
#include "nlsCompiler_exports.h"
//=============================================================================
namespace Nelson {
NLSCOMPILER_IMPEXP std::string
buildApplicationLinkPlan(const std::wstring& entry,
    const std::vector<std::wstring>& includedFiles = {}, bool autoDetectDataFiles = true);
NLSCOMPILER_IMPEXP std::string
buildApplicationLinkPlan(const std::wstring& entry, const std::vector<std::wstring>& includedFiles,
    bool autoDetectDataFiles, const std::wstring& excludedPackages);
NLSCOMPILER_IMPEXP std::string
buildApplicationRuntimePlan(const std::wstring& application, bool gui);
//=============================================================================
} // namespace Nelson
//=============================================================================
