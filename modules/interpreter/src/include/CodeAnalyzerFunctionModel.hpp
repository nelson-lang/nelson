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
//=============================================================================
namespace Nelson {
//=============================================================================
std::vector<CodeAnalyzerFunction>
buildCodeAnalyzerFunctionModel(const stringVector& lines, MacroFunctionDef* parsedFunction);
//=============================================================================
std::vector<CodeAnalyzerClassdef>
buildCodeAnalyzerClassdefModel(const stringVector& lines);
//=============================================================================
} // namespace Nelson
//=============================================================================
