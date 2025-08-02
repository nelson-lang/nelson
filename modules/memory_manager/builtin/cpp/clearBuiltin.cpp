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
#include <fmt/printf.h>
#include <fmt/format.h>
#include "clearBuiltin.hpp"
#include "Clear.hpp"
#include "ClearFunction.hpp"
#include "ClearGlobal.hpp"
#include "Error.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
#include "IsValidVariableName.hpp"
#include "characters_encoding.hpp"
#include "GatewaysManager.hpp"
#include "NelsonGateway.hpp"
#include "mex.h"
#include "MexFunctionDef.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "FunctionsInMemory.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
clearByName(Evaluator* eval, const std::string& name);
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
            if (!argIn[k].isRowVectorCharacterArray()) {
                Error(fmt::sprintf(ERROR_WRONG_ARGUMENT_X_TYPE_STRING_EXPECTED, k + 1));
            }
        }
        if (argIn.size() == 1) {
            std::wstring arg1 = argIn[0].getContentAsWideString();
            if (arg1 == L"global") {
                ClearAllGlobalVariables(eval);
            } else if (arg1 == L"all") {
                ClearAllVariables(eval);
                ClearAllGlobalVariables(eval);
                ClearMacroCache(eval);
                if (FunctionsInMemory::getInstance()->deleteAllMexFunctions()) {
                    mexFreeAllRegisteredPointer();
                }
            } else if (arg1 == L"variables") {
                ClearAllVariables(eval);
            } else if (arg1 == L"functions") {
                ClearMacroCache(eval);
                ClearAllPersistentVariables(eval);
            } else if (arg1 == L"mex") {
                if (FunctionsInMemory::getInstance()->deleteAllMexFunctions()) {
                    mexFreeAllRegisteredPointer();
                }
            } else {
                std::string name = wstring_to_utf8(arg1);
                clearByName(eval, name);
            }
        } else if (argIn.size() == 2) {
            // clear global varname
            // clear varname1 varname2
            std::wstring arg1 = argIn[0].getContentAsWideString();
            Context* ctxt = eval->getContext();
            if (arg1 == L"global") {
                std::wstring arg2 = argIn[1].getContentAsWideString();
                if (ctxt->getGlobalScope()->isLockedVariable(wstring_to_utf8(arg2))) {
                    Warning(_W("variable is locked:") + arg2);
                }
                ClearGlobalVariable(eval, arg2);
            } else {
                for (const auto& k : argIn) {
                    std::string name = k.getContentAsCString();
                    clearByName(eval, name);
                }
            }
        } else {
            // clear varname1 varname2 ... varnameN
            for (const auto& k : argIn) {
                std::string name = k.getContentAsCString();
                clearByName(eval, name);
            }
        }
    }
    return retval;
}
//=============================================================================
void
clearByName(Evaluator* eval, const std::string& name)
{
    if (!IsValidVariableName(name)) {
        Error(_W("A valid variable name expected."));
    }
    Context* ctxt = eval->getContext();
    if (ctxt->isVariable(name)) {
        if (ctxt->isLockedVariable(name)) {
            Warning(_("variable is locked:") + name);
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
