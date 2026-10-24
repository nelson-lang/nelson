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
#include "CodeAnalyzerAst.hpp"
#include <filesystem>
#include <functional>
//=============================================================================
namespace Nelson {
//=============================================================================
void
applyCodeAnalyzerClassdefRules(const std::filesystem::path& path, const stringVector& lines,
    const std::vector<CodeAnalyzerFunction>& functions,
    const std::vector<CodeAnalyzerClassdef>& classdefs,
    const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic);
//=============================================================================
} // namespace Nelson
//=============================================================================
