//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "codeAnalyzerBuiltin.hpp"
#include "CodeAnalyzer.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
bool
optionalLogical(const ArrayOfVector& args, size_t index, bool defaultValue)
{
    if (args.size() <= index) {
        return defaultValue;
    }
    return args[index].asLogicalScalar();
}
//=============================================================================
std::wstring
optionalString(const ArrayOfVector& args, size_t index)
{
    if (args.size() <= index || args[index].isEmpty()) {
        return L"";
    }
    if (args[index].isRowVectorCharacterArray() || args[index].isStringArray()) {
        return args[index].asWideString();
    }
    Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    return L"";
}
//=============================================================================
wstringVector
namesFromArgument(const ArrayOf& arg)
{
    if (arg.isRowVectorCharacterArray()) {
        return { arg.asWideString() };
    }
    if (arg.isStringArray() || arg.isCellArrayOfCharacterVectors()) {
        return arg.asWideStringVector(false);
    }
    Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    return {};
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::codeAnalyzerBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    (void)eval;
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 7);
    CodeAnalyzerOptions options;
    options.includeSubfolders = optionalLogical(argIn, 1, false);
    options.configFile = optionalString(argIn, 2);
    options.includeSuppressed = optionalLogical(argIn, 3, false);
    options.fullPath = optionalLogical(argIn, 4, false);
    options.includeCyclomatic = optionalLogical(argIn, 5, false);
    options.includeModifiedCyclomatic = optionalLogical(argIn, 6, false);
    std::vector<CodeAnalyzerDiagnostic> diagnostics
        = analyzeCode(namesFromArgument(argIn[0]), options);
    retval << codeAnalyzerDiagnosticsToStruct(diagnostics);
    return retval;
}
//=============================================================================
