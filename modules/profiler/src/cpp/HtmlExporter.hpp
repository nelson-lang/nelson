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
#include <vector>
#include <string>
#include <tuple>
#include <set>
#include "nlsProfiler_exports.h"
#include "Types.hpp"
#include "ProfilerHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
copyHtmlDependencies(
    const std::wstring& moduleProfilerPath, const std::wstring& directoryDestination);
//=============================================================================
void
generateProfileFileHtml(const std::wstring& srcFilename, const stringVector& functionContent,
    const std::vector<std::tuple<int, std::string, int, double>>& fiveSlowerLines,
    const std::tuple<int, int, int, int, int, double>& coverage,
    const std::vector<std::tuple<int, double>>& lineInfo, int nbCalls, double totalTime,
    const std::wstring& htmlFilename);
//=============================================================================
void
generateProfileIndexHtml(const std::wstring& htmlFilename,
    const std::vector<std::tuple<std::wstring, std::wstring, int, double, double>>& indexData);
//=============================================================================
} // namespace Nelson
//=============================================================================
