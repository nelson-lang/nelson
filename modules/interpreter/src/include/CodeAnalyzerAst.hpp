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
#include "ParserInterface.hpp"
#include "Types.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
struct CodeAnalyzerFunction
{
    std::string name;
    std::string classdefOwnerName;
    stringVector inputs;
    stringVector outputs;
    int line = 0;
    int endLine = 0;
    int column = 0;
    int parentLine = 0;
    int depth = 0;
    int cyclomaticComplexity = 1;
    int modifiedCyclomaticComplexity = 1;
    int cognitiveComplexity = 1;
    bool local = false;
    bool nested = false;
    bool classdefMethod = false;
    bool script = false;
};
//=============================================================================
struct CodeAnalyzerClassProperty
{
    std::string name;
    int line = 0;
    int column = 0;
};
//=============================================================================
struct CodeAnalyzerClassEvent
{
    std::string name;
    int line = 0;
    int column = 0;
};
//=============================================================================
struct CodeAnalyzerClassMethod
{
    std::string name;
    int line = 0;
    int column = 0;
};
//=============================================================================
struct CodeAnalyzerClassSuperclass
{
    std::string name;
    int line = 0;
    int column = 0;
};
//=============================================================================
struct CodeAnalyzerClassAttribute
{
    std::string name;
    int line = 0;
    int column = 0;
};
//=============================================================================
struct CodeAnalyzerClassdef
{
    std::string name;
    std::vector<CodeAnalyzerClassProperty> properties;
    std::vector<CodeAnalyzerClassEvent> events;
    std::vector<CodeAnalyzerClassMethod> externalMethods;
    std::vector<CodeAnalyzerClassSuperclass> superclasses;
    std::vector<CodeAnalyzerClassAttribute> attributes;
    int line = 0;
    int column = 0;
    int endLine = 0;
};
//=============================================================================
struct CodeAnalyzerLineUsage
{
    stringVector assigned;
    stringVector used;
    stringVector callableUsed;
    stringVector leftModified;
    stringVector globalDeclarations;
    stringVector persistentDeclarations;
    stringVector argumentDeclarations;
    stringVector argumentValidationUsed;
    stringVector importedNames;
    stringVector importedSpecs;
    int line = 0;
    bool importLine = false;
    bool importFromAst = false;
    bool wildcardImport = false;
    bool assignmentFromAst = false;
    bool usageFromAst = false;
    bool dynamicCode = false;
    std::string dynamicCallName;
    int dynamicCallColumn = 0;
    bool suspiciousFloatComparison = false;
    int suspiciousFloatComparisonColumn = 0;
    bool displaySprintfCall = false;
    int displaySprintfCallColumn = 0;
    bool diagnosticSprintfCall = false;
    int diagnosticSprintfCallColumn = 0;
    bool constantCondition = false;
    int constantConditionColumn = 0;
    bool codeLine = false;
    bool flowBoundary = false;
    bool flowBlockStart = false;
    bool exitStatement = false;
    bool startsArgumentsBlock = false;
    bool endsBlock = false;
    bool functionDeclaration = false;
};
//=============================================================================
struct CodeAnalyzerAstModel
{
    std::vector<CodeAnalyzerFunction> functions;
    std::vector<CodeAnalyzerClassdef> classdefs;
    std::vector<CodeAnalyzerLineUsage> lineUsages;
};
//=============================================================================
NLSINTERPRETER_IMPEXP CodeAnalyzerAstModel
buildCodeAnalyzerAstModel(const stringVector& lines, const ParserContext& parserContext);
//=============================================================================
NLSINTERPRETER_IMPEXP CodeAnalyzerLineUsage
collectCodeAnalyzerLineUsage(const stringVector& lines, int lineNumber);
//=============================================================================
NLSINTERPRETER_IMPEXP int
codeAnalyzerWordColumnInLine(const std::string& line, const std::string& word);
//=============================================================================
} // namespace Nelson
//=============================================================================
