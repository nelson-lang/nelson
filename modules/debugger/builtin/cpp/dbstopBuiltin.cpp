//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dbstopBuiltin.hpp"
#include "BreakpointTarget.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
#include "ErrorID.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::DebuggerGateway;
//=============================================================================
static void
applyBreakpointStruct(Evaluator* eval, const ArrayOf& bpStruct);
static std::wstring
joinTokens(const wstringVector& tokens, size_t first);
static void
setGlobalCondition(Evaluator* eval, const std::wstring& condition);
//=============================================================================
ArrayOfVector
Nelson::DebuggerGateway::dbstopBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // dbstop in file
    // dbstop in file at location
    // dbstop in F [at L] [if expr], dbstop if error, dbstop if caught error, dbstop(b)
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 0);
    if (argIn.size() == 1 && argIn[0].isStruct()) {
        applyBreakpointStruct(eval, argIn[0]);
        return {};
    }
    wstringVector tokens = breakpointCommandTokens(argIn);
    if (tokens[0] == L"if") {
        setGlobalCondition(eval, joinTokens(tokens, 1));
        return {};
    }
    if (tokens[0] != L"in") {
        Error(L"Nelson:dbstop:unknownCommandOption", L"Unknown command option.");
    }
    if (tokens.size() < 2) {
        Error(L"Nelson:dbstop:fileNameRequired", L"A file name must be specified.");
    }
    std::wstring target = tokens[1];
    size_t index = 2;
    bool explicitPosition = false;
    size_t position = 1;
    std::wstring condition;
    if (index < tokens.size() && tokens[index] == L"at") {
        if (index + 1 >= tokens.size()) {
            Error(L"Nelson:dbstop:unknownCommandOption", L"Unknown command option.");
        }
        if (target.find(L'>') != std::wstring::npos) {
            Error(L"Nelson:dbstop:invalidOptionCombination",
                L"Invalid option combination. Specify the line number or specify the local or "
                L"nested function.");
        }
        const std::wstring& location = tokens[index + 1];
        if (parseBreakpointLineToken(location, position)) {
            explicitPosition = true;
        } else {
            // the location names a local function
            target = target + L">" + location;
        }
        index += 2;
    }
    if (index < tokens.size()) {
        if (tokens[index] != L"if") {
            Error(L"Nelson:dbstop:unknownCommandOption", L"Unknown command option.");
        }
        condition = joinTokens(tokens, index + 1);
        if (condition.empty()) {
            Error(L"Nelson:dbstop:unknownCommandOption", L"Unknown command option.");
        }
        index = tokens.size();
    }
    std::wstring errorMessage;
    if (!condition.empty() && !eval->validateBreakpointCondition(condition, errorMessage)) {
        RaiseError(errorMessage, L"Nelson:dbstop:conditionSyntaxError");
    }
    size_t adjustedLine = 0;
    bool created = false;
    if (!setBreakpointAt(
            eval, target, position, explicitPosition, adjustedLine, created, errorMessage)) {
        RaiseError(errorMessage, L"Nelson:dbstop:invalidArgument");
    }
    BreakpointTarget resolved;
    std::wstring resolveError;
    if (resolveBreakpointTarget(eval, target, resolved, resolveError)) {
        // a new dbstop on the line replaces the condition, including with none
        if (!eval->setBreakpointCondition(resolved.filename, adjustedLine, condition, errorMessage)
            && !errorMessage.empty()) {
            if (created) {
                eval->removeBreakpoint(resolved.filename, adjustedLine);
            }
            RaiseError(errorMessage, L"Nelson:dbstop:conditionSyntaxError");
        }
    }
    return {};
}
//=============================================================================
std::wstring
joinTokens(const wstringVector& tokens, size_t first)
{
    std::wstring joined;
    for (size_t k = first; k < tokens.size(); ++k) {
        if (!joined.empty()) {
            joined += L" ";
        }
        joined += tokens[k];
    }
    return joined;
}
//=============================================================================
void
setGlobalCondition(Evaluator* eval, const std::wstring& condition)
{
    if (condition == L"error") {
        eval->setStopOnError(true);
        return;
    }
    if (condition == L"caught error") {
        eval->setStopOnCaughtError(true);
        return;
    }
    if (condition == L"warning" || condition == L"naninf" || condition == L"infnan"
        || condition == L"unsuppressed output" || condition.rfind(L"error ", 0) == 0
        || condition.rfind(L"caught error ", 0) == 0 || condition.rfind(L"warning ", 0) == 0) {
        Error(L"Nelson:dbstop:unsupportedCondition",
            L"Unsupported dbstop condition '%s'. Supported conditions are 'error' and 'caught "
            L"error'.",
            condition);
    }
    Error(L"Nelson:dbstop:fileNameRequired", L"A file name must be specified.");
}
//=============================================================================
static std::wstring
elementAsText(const ArrayOf& value)
{
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        return value.asWideString();
    }
    return L"";
}
//=============================================================================
void
applyBreakpointStruct(Evaluator* eval, const ArrayOf& bpStruct)
{
    static const stringVector expectedFields
        = { "name", "file", "line", "anonymous", "expression", "cond", "identifier" };
    stringVector names = bpStruct.fieldNames();
    for (const std::string& expected : expectedFields) {
        if (std::find(names.begin(), names.end(), expected) == names.end()) {
            Error(L"Nelson:dbstop:invalidStructure", L"Invalid structure. Field '%s' is missing.",
                utf8_to_wstring(expected));
        }
    }
    ArrayOfVector files = bpStruct.fieldAsList("file");
    ArrayOfVector lines = bpStruct.fieldAsList("line");
    ArrayOfVector expressions = bpStruct.fieldAsList("expression");
    ArrayOfVector conds = bpStruct.fieldAsList("cond");
    size_t count = bpStruct.dimensions().elementCount();
    for (size_t i = 0; i < count; ++i) {
        std::wstring cond = elementAsText(conds[i]);
        if (!cond.empty()) {
            setGlobalCondition(eval, cond);
            continue;
        }
        ArrayOf lineArray = lines[i];
        if (lineArray.isEmpty()) {
            continue;
        }
        if (!lineArray.isNumeric()) {
            Error(L"Nelson:dbstop:invalidLineNumber", L"Invalid line number in breakpoint struct.");
        }
        std::vector<indexType> lineValues = lineArray.asIndexVector();
        wstringVector conditions;
        const ArrayOf& expressionCell = expressions[i];
        if (expressionCell.isCell()) {
            const auto* elements = static_cast<const ArrayOf*>(expressionCell.dataPointer());
            for (indexType k = 0; k < expressionCell.elementCount(); ++k) {
                conditions.push_back(elementAsText(elements[k]));
            }
        }
        std::wstring file = elementAsText(files[i]);
        for (size_t k = 0; k < lineValues.size(); ++k) {
            std::wstring errorMessage;
            size_t adjustedLine = 0;
            bool created = false;
            if (!setBreakpointAt(eval, file, static_cast<size_t>(lineValues[k]), true, adjustedLine,
                    created, errorMessage)) {
                RaiseError(errorMessage, L"Nelson:dbstop:invalidArgument");
            }
            std::wstring condition = k < conditions.size() ? conditions[k] : L"";
            BreakpointTarget resolved;
            std::wstring resolveError;
            if (resolveBreakpointTarget(eval, file, resolved, resolveError)
                && !eval->setBreakpointCondition(
                    resolved.filename, adjustedLine, condition, errorMessage)
                && !errorMessage.empty()) {
                RaiseError(errorMessage, L"Nelson:dbstop:conditionSyntaxError");
            }
        }
    }
}
//=============================================================================
