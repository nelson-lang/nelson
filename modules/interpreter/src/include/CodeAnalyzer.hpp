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
#include <map>
#include <vector>
#include "ArrayOf.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
struct CodeAnalyzerOptions
{
    bool includeSubfolders = false;
    bool includeSuppressed = false;
    bool fullPath = false;
    bool includeCyclomatic = false;
    bool includeModifiedCyclomatic = false;
    int maxLineLength = 120;
    int maxFileLines = 900;
    int maxFunctionLines = 150;
    int cyclomaticWarningThreshold = 10;
    int cyclomaticErrorThreshold = 50;
    std::wstring configFile;
    wstringVector inclusions;
    wstringVector exclusions;
    std::map<std::string, std::string> ruleSeverity;
    std::string configError;
    std::string failOn = "info";
};
//=============================================================================
struct CodeAnalyzerTextEdit
{
    int lineStart = 0;
    int lineEnd = 0;
    int columnStart = 0;
    int columnEnd = 0;
    std::string replacementText;
};
//=============================================================================
struct CodeAnalyzerFix
{
    std::string title;
    std::vector<CodeAnalyzerTextEdit> edits;
    bool safe = true;
};
//=============================================================================
struct CodeAnalyzerDiagnostic
{
    std::wstring file;
    std::string checkId;
    std::string ruleName;
    std::string severity;
    std::string category;
    std::string fixability;
    std::string helpUri;
    std::string fingerprint;
    std::string message;
    int lineStart = 0;
    int lineEnd = 0;
    int columnStart = 0;
    int columnEnd = 0;
    bool suppressed = false;
    std::vector<CodeAnalyzerFix> fixes;
};
//=============================================================================
NLSINTERPRETER_IMPEXP std::vector<CodeAnalyzerDiagnostic>
analyzeCode(const wstringVector& names, const CodeAnalyzerOptions& options);
//=============================================================================
NLSINTERPRETER_IMPEXP std::vector<CodeAnalyzerDiagnostic>
analyzeCodeBuffer(const std::wstring& displayFilename, const std::string& sourceText,
    const CodeAnalyzerOptions& options);
//=============================================================================
NLSINTERPRETER_IMPEXP wstringVector
codeAnalyzerIncludedFiles(const wstringVector& names, const CodeAnalyzerOptions& options);
//=============================================================================
NLSINTERPRETER_IMPEXP ArrayOf
codeAnalyzerDiagnosticsToStruct(const std::vector<CodeAnalyzerDiagnostic>& diagnostics);
//=============================================================================
NLSINTERPRETER_IMPEXP std::string
codeAnalyzerDiagnosticsToJson(const std::vector<CodeAnalyzerDiagnostic>& diagnostics);
//=============================================================================
NLSINTERPRETER_IMPEXP std::string
codeAnalyzerDiagnosticsToSarif(const std::vector<CodeAnalyzerDiagnostic>& diagnostics);
//=============================================================================
NLSINTERPRETER_IMPEXP std::string
codeAnalyzerDiagnosticsToText(const std::vector<CodeAnalyzerDiagnostic>& diagnostics);
//=============================================================================
} // namespace Nelson
//=============================================================================
