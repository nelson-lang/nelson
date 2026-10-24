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
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
struct CodeAnalyzerRuleInfo
{
    std::string id;
    std::string name;
    std::string defaultSeverity;
    std::string category;
    std::string fixability;
    std::string description;
    std::string helpUri;
};
//=============================================================================
NLSINTERPRETER_IMPEXP const CodeAnalyzerRuleInfo*
codeAnalyzerRuleInfo(const std::string& id);
//=============================================================================
NLSINTERPRETER_IMPEXP std::vector<CodeAnalyzerRuleInfo>
codeAnalyzerRules();
//=============================================================================
} // namespace Nelson
//=============================================================================
