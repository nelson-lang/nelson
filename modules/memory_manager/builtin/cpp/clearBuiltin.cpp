//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <regex>
#include <fmt/xchar.h>
#include "clearBuiltin.hpp"
#include "Clear.hpp"
#include "ClearFunction.hpp"
#include "ClearGlobal.hpp"
#include "ClassdefParser.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
#include "IsValidVariableName.hpp"
#include "characters_encoding.hpp"
#include "GatewaysManager.hpp"
#include "NelsonGateway.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "FunctionsInMemory.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
clearByName(Evaluator* eval, const std::string& name);
static void
clearByRegexp(Evaluator* eval, const wstringVector& expressions);
static bool
isStringArgument(const ArrayOf& arg);
static std::wstring
stringArgumentContent(const ArrayOf& arg);
static void
clearClassdefVariables(Evaluator* eval);
static bool
isClassdefVariable(const ArrayOf& value);
//=============================================================================
// clear keyword
// clear varname
// clear global varname
// clear varname1 varname2 ... varnameN
// clear function
ArrayOfVector
Nelson::MemoryGateway::clearBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    if (argIn.size() == 0) {
        ClearAllVariables(eval);
    } else {
        for (size_t k = 0; k < argIn.size(); k++) {
            if (!isStringArgument(argIn[k])) {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (k + 1),
                    L"string");
            }
        }
        std::wstring arg1 = stringArgumentContent(argIn[0]);
        if (arg1 == L"-regexp") {
            if (argIn.size() == 1) {
                Error(L"Nelson:notEnoughInputs", L"Not enough input arguments.");
            }
            wstringVector expressions;
            for (size_t k = 1; k < argIn.size(); ++k) {
                expressions.push_back(stringArgumentContent(argIn[k]));
            }
            clearByRegexp(eval, expressions);
        } else if (argIn.size() == 1) {
            if (arg1 == L"global") {
                ClearAllGlobalVariables(eval);
            } else if (arg1 == L"all") {
                ClearAllVariables(eval);
                ClearAllGlobalVariables(eval);
                ClearMacroCache(eval);
                if (FunctionsInMemory::getInstance()->deleteAllMexFunctions()) {
                    FunctionsInMemory::getInstance()->freeRegisteredMexMemory();
                }
            } else if (arg1 == L"variables") {
                ClearAllVariables(eval);
            } else if (arg1 == L"functions") {
                ClearMacroCache(eval);
                ClearAllPersistentVariables(eval);
            } else if (arg1 == L"classes") {
                clearClassdefVariables(eval);
                ClearMacroCache(eval);
            } else if (arg1 == L"import") {
                eval->getContext()->clearImports();
            } else if (arg1 == L"mex") {
                if (FunctionsInMemory::getInstance()->deleteAllMexFunctions()) {
                    FunctionsInMemory::getInstance()->freeRegisteredMexMemory();
                }
            } else {
                std::string name = wstring_to_utf8(arg1);
                clearByName(eval, name);
            }
        } else if (arg1 == L"global") {
            Context* ctxt = eval->getContext();
            for (size_t k = 1; k < argIn.size(); ++k) {
                std::wstring name = stringArgumentContent(argIn[k]);
                if (ctxt->getGlobalScope()->isLockedVariable(wstring_to_utf8(name))) {
                    Warning(
                        L"Nelson:memory_manager:variableLocked", L"variable is locked: %s", name);
                }
                ClearGlobalVariable(eval, name);
            }
        } else if (argIn.size() == 2) {
            for (const auto& k : argIn) {
                std::string name = wstring_to_utf8(stringArgumentContent(k));
                clearByName(eval, name);
            }
        } else {
            for (const auto& k : argIn) {
                std::string name = wstring_to_utf8(stringArgumentContent(k));
                clearByName(eval, name);
            }
        }
    }
    return retval;
}
//=============================================================================
bool
isStringArgument(const ArrayOf& arg)
{
    return arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar());
}
//=============================================================================
std::wstring
stringArgumentContent(const ArrayOf& arg)
{
    return arg.asWideString();
}
//=============================================================================
void
clearByRegexp(Evaluator* eval, const wstringVector& expressions)
{
    stringVector names;
    eval->getContext()->getCurrentScope()->getVariablesList(false, names);
    std::vector<std::wregex> patterns;
    patterns.reserve(expressions.size());
    try {
        for (const auto& expression : expressions) {
            patterns.emplace_back(expression);
        }
    } catch (const std::regex_error&) {
        Error("Nelson:clear:invalidRegularExpression", "Invalid regular expression.");
    }
    for (const auto& name : names) {
        const std::wstring wideName = utf8_to_wstring(name);
        for (const auto& pattern : patterns) {
            if (std::regex_search(wideName, pattern)) {
                ClearVariable(eval, name);
                break;
            }
        }
    }
}
//=============================================================================
bool
isClassdefVariable(const ArrayOf& value)
{
    if (value.isClassdefType()) {
        return true;
    }
    if (!value.isHandle()) {
        return false;
    }
    return classdefLoadClass(value.handleCategory());
}
//=============================================================================
void
clearClassdefVariables(Evaluator* eval)
{
    Context* context = eval->getContext();
    stringVector names;
    context->getCurrentScope()->getVariablesList(false, names);
    for (const auto& name : names) {
        ArrayOf value;
        if (context->getCurrentScope()->lookupVariable(name, value) && isClassdefVariable(value)) {
            ClearVariable(eval, name);
        }
    }

    names.clear();
    context->getGlobalScope()->getVariablesList(false, names);
    for (const auto& name : names) {
        ArrayOf value;
        if (context->getGlobalScope()->lookupVariable(name, value) && isClassdefVariable(value)) {
            ClearGlobalVariable(eval, name);
        }
    }
}
//=============================================================================
void
clearByName(Evaluator* eval, const std::string& name)
{
    if (!IsValidVariableName(name)) {
        Error(L"Nelson:clear:invalidVariableName", L"A valid variable name expected.");
    }
    Context* ctxt = eval->getContext();
    FunctionDefPtr func = nullptr;
    if (FunctionsInMemory::getInstance()->find(name, func) && func != nullptr
        && func->type() == NLS_MACRO_FUNCTION) {
        ClearPersistentVariable(eval, name);
    }
    if (ctxt->isVariable(name)) {
        if (ctxt->isLockedVariable(name)) {
            Warning("Nelson:memory_manager:variableLocked", "variable is locked: %s", name);
        }
        if (!ClearVariable(eval, name)) {
            ClearPersistentVariable(eval, name);
        }
    } else {
        if (!FunctionsInMemory::getInstance()->deleteMexFunction(name)) {
            ClearPersistentVariable(eval, name);
            FunctionsInMemory::getInstance()->deleteMFunction(name);
        }
    }
}
//=============================================================================
