//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <vector>
#include "whoBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Who.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
Who(Evaluator* eval, const std::wstring& filename, const stringVector& names, bool asCell);
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::whoBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    stringVector variablesName;
    std::wstring filename;
    if (argIn.empty()) {
        variablesName = Who(eval, false);
    } else {
        Scope* scope = eval->getContext()->getCurrentScope();
        stringVector names;
        std::wstring param1;
        indexType start = 1;
        if (argIn[0].isRowVectorCharacterArray()) {
            param1 = argIn[0].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        if (param1.compare(L"global") == 0) {
            scope = eval->getContext()->getGlobalScope();
        } else if (param1.compare(L"base") == 0) {
            scope = eval->getContext()->getBaseScope();
        } else if (param1.compare(L"local") == 0) {
            scope = eval->getContext()->getCurrentScope();
        } else if (param1.compare(L"caller") == 0) {
            scope = eval->getContext()->getCallerScope();
        } else if (param1.compare(L"-file") == 0) {
            if (argIn.size() > 1) {
                filename = argIn[1].getContentAsWideString();
                start++;
            } else {
                Error(_W("filename expected after '-file'."));
            }
        } else {
            names.push_back(wstring_to_utf8(param1));
        }
        for (indexType k = start; k < (indexType)argIn.size(); ++k) {
            std::string param = argIn[k].getContentAsCString();
            if (param.compare("-file") == 0) {
                Error(_W("-file must be the first argument."));
            }
            names.push_back(param);
        }
        if (filename.empty()) {
            variablesName = Who(eval, scope, false);
            if (!names.empty()) {
                stringVector result;
                for (const std::string& n : names) {
                    if (std::find(variablesName.begin(), variablesName.end(), n)
                        != variablesName.end()) {
                        result.push_back(n);
                    }
                }
                variablesName = result;
            }
        } else {
            ArrayOf v = Who(eval, filename, names, nLhs == 1);
            if (nLhs == 1) {
                retval << v;
            }
            return retval;
        }
    }
    if (nLhs == 0) {
        Interface* io = eval->getInterface();
        size_t ncharmax = io->getTerminalWidth();
        size_t nbchar = 0;
        if (!variablesName.empty()) {
            io->outputMessage(_W("Your variables are:") + L"\n\n");
        }
        for (auto& k : variablesName) {
            if (nbchar + k.size() < ncharmax) {
                io->outputMessage(k);
                io->outputMessage(" ");
                nbchar = 1 + nbchar + k.size();
            } else {
                nbchar = 0;
                io->outputMessage("\n");
                io->outputMessage(k);
                io->outputMessage(" ");
                nbchar = 1 + nbchar + k.size();
            }
        }
        if (!variablesName.empty()) {
            io->outputMessage("\n");
        }
    } else {
        retval << ArrayOf::toCellArrayOfCharacterColumnVectors(variablesName);
    }
    return retval;
}
//=============================================================================
ArrayOf
Who(Evaluator* eval, const std::wstring& filename, const stringVector& names, bool asCell)
{
    bool isNh5 = false;
    bool isMat = false;
    // try detect if it is a .nh5
    FunctionDef* funcDef = nullptr;
    if (!PathFunctionIndexerManager::getInstance()->find("isnh5file", funcDef)) {
        BuiltInFunctionDefManager::getInstance()->find("isnh5file", funcDef);
    }
    if (funcDef != nullptr) {
        ArrayOfVector inputArguments;
        inputArguments.push_back(ArrayOf::characterArrayConstructor(filename));
        ArrayOfVector res = funcDef->evaluateFunction(eval, inputArguments, 1);
        if (res.size() == 1) {
            isNh5 = res[0].getContentAsLogicalScalar();
        }
    }

    if (!isNh5) {
        // try detect if it is a .mat
        FunctionDef* funcDef = nullptr;
        if (!PathFunctionIndexerManager::getInstance()->find("ismatfile", funcDef)) {
            BuiltInFunctionDefManager::getInstance()->find("ismatfile", funcDef);
        }
        if (funcDef != nullptr) {
            ArrayOfVector inputArguments;
            inputArguments.push_back(ArrayOf::characterArrayConstructor(filename));
            ArrayOfVector res = funcDef->evaluateFunction(eval, inputArguments, 1);
            if (res.size() == 1) {
                isMat = res[0].getContentAsLogicalScalar();
            }
        }
    }
    std::string whoFileFunctionName;
    if (isNh5) {
        whoFileFunctionName = "whonh5";
    } else if (isMat) {
        whoFileFunctionName = "whomat";
    } else {
        whoFileFunctionName = "whonh5";
    }

    funcDef = nullptr;
    if (!PathFunctionIndexerManager::getInstance()->find(whoFileFunctionName, funcDef)) {
        if (!BuiltInFunctionDefManager::getInstance()->find(whoFileFunctionName, funcDef)) {
            Error(_W("who file function expected."));
        }
    }
    ArrayOfVector inputArguments;
    inputArguments.push_back(ArrayOf::characterArrayConstructor(filename));
    for (const std::string& name : names) {
        inputArguments.push_back(ArrayOf::characterArrayConstructor(name));
    }
    ArrayOfVector res = funcDef->evaluateFunction(eval, inputArguments, asCell ? 1 : 0);
    if (res.size()) {
        return res[0];
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
