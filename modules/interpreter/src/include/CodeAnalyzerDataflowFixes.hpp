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
#include "Types.hpp"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
void
addDiagnosticSprintfFix(CodeAnalyzerDiagnostic& diagnostic, const std::string& line);
//=============================================================================
void
addDisplaySprintfFix(CodeAnalyzerDiagnostic& diagnostic, const std::string& line);
//=============================================================================
void
addUnreachableCodeFix(CodeAnalyzerDiagnostic& diagnostic, const stringVector& lines,
    const CodeAnalyzerLineUsage& usage);
//=============================================================================
void
addUnusedImportFix(
    CodeAnalyzerDiagnostic& diagnostic, const stringVector& lines, const std::string& symbol);
//=============================================================================
void
addUnusedAssignmentFix(
    CodeAnalyzerDiagnostic& diagnostic, const stringVector& lines, const std::string& symbol);
//=============================================================================
void
addDynamicLoadCaptureFix(CodeAnalyzerDiagnostic& diagnostic, const stringVector& lines);
//=============================================================================
void
addClassdefNameMismatchFix(CodeAnalyzerDiagnostic& diagnostic, const CodeAnalyzerClassdef& classdef,
    const std::string& expectedName);
//=============================================================================
void
addFunctionNameMismatchFix(CodeAnalyzerDiagnostic& diagnostic, const CodeAnalyzerFunction& function,
    const std::string& expectedName);
//=============================================================================
void
addUnusedLocalFunctionFix(CodeAnalyzerDiagnostic& diagnostic, const CodeAnalyzerFunction& function);
//=============================================================================
void
addMissingFunctionHelpFix(CodeAnalyzerDiagnostic& diagnostic, const CodeAnalyzerFunction& function,
    const stringVector& lines);
//=============================================================================
void
addMissingClassdefHelpFix(CodeAnalyzerDiagnostic& diagnostic, const CodeAnalyzerClassdef& classdef,
    const stringVector& lines);
//=============================================================================
} // namespace Nelson
//=============================================================================
