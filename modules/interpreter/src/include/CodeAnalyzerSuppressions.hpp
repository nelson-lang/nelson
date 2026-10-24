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
#include "Types.hpp"
#include <map>
#include <set>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
struct CodeAnalyzerSuppression
{
    bool all = false;
    std::set<std::string> ids;
};
//=============================================================================
using CodeAnalyzerSuppressions = std::map<int, CodeAnalyzerSuppression>;
//=============================================================================
CodeAnalyzerSuppressions
collectCodeAnalyzerSuppressions(const stringVector& lines);
//=============================================================================
bool
isCodeAnalyzerDiagnosticSuppressed(
    const CodeAnalyzerSuppressions& suppressions, int line, const std::string& checkId);
//=============================================================================
} // namespace Nelson
//=============================================================================
