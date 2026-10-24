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
#include "CodeAnalyzerAst.hpp"
#include <set>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
const std::set<std::string>&
codeAnalyzerBuiltinFunctionNames();
//=============================================================================
std::set<std::string>
codeAnalyzerClassPropertyNamesFor(
    const CodeAnalyzerFunction& function, const std::vector<CodeAnalyzerClassdef>& classdefs);
//=============================================================================
} // namespace Nelson
//=============================================================================
