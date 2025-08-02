//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "saveBuiltin.hpp"
#include "i18n.hpp"
#include "Error.hpp"
#include "IsValidVariableName.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "FileSystemWrapper.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// save(filename)
// save(filename, '-mat')
// save(filename, '-nh5')
// save(filename, '-nh5', '-append', '-nocompression')
// save(filename, '-mat', '-append', '-nocompression', variables)
// save(filename, '-mat', '-append', '-nocompression', version, variables)
//=============================================================================
static bool
isOption(const std::wstring& param)
{
    return param.size() > 2 && param[0] == L'-';
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::saveBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1);
    std::wstring paramFilename = argIn[0].getContentAsWideString();
    if (paramFilename.empty()) {
        paramFilename = L"nelson.nh5";
    }
    wstringVector names;
    bool forceAsMat = false;
    bool forceAsNh5 = false;
    bool nocompression = false;
    bool append = false;
    std::wstring version;
    for (size_t k = 1; k < argIn.size(); k++) {
        ArrayOf paramK = argIn[k];
        std::wstring param = paramK.getContentAsWideString();
        if (isOption(param)) {
            if (param == L"-mat" && !forceAsNh5) {
                forceAsMat = true;
            } else if (param == L"-nh5" && !forceAsMat) {
                forceAsNh5 = true;
            } else if (param == L"-append") {
                append = true;
            } else if (param == L"-nocompression") {
                nocompression = true;
            } else if ((param == L"-v7.3") || (param == L"-v7") || (param == L"-v6")
                || (param == L"-v4")) {
                version = param;
                forceAsMat = true;
            } else {
                Error(_W("Valid option expected."));
            }
        } else {
            if (IsValidVariableName(param)) {
                names.push_back(param);
            } else {
                Error(_W("Valid variable name expected."));
            }
        }
    }

    if (!forceAsMat && !forceAsNh5) {
        std::string extension = FileSystemWrapper::Path(paramFilename).extension().string();
        if (extension == ".nh5") {
            forceAsNh5 = true;
        } else if (extension == ".mat") {
            forceAsMat = true;
        } else if (extension.empty()) {
            forceAsNh5 = true;
            paramFilename = paramFilename + L".nh5";
        } else {
            forceAsNh5 = true;
        }
    }

    std::string saveFunctionName;
    if (forceAsNh5) {
        saveFunctionName = "savenh5";
    } else if (forceAsMat) {
        saveFunctionName = "savemat";
    }
    if (saveFunctionName.empty()) {
        Error(_W("save function expected."));
    }

    FunctionDef* funcDef = nullptr;
    if (!PathFunctionIndexerManager::getInstance()->find(saveFunctionName, funcDef)) {
        if (!BuiltInFunctionDefManager::getInstance()->find(saveFunctionName, funcDef)) {
            Error(_W("load function expected."));
        }
    }

    ArrayOfVector inputArguments;
    inputArguments.push_back(ArrayOf::characterArrayConstructor(paramFilename));
    for (const std::wstring& name : names) {
        inputArguments.push_back(ArrayOf::characterArrayConstructor(name));
    }
    if (!version.empty()) {
        inputArguments.push_back(ArrayOf::characterArrayConstructor(version));
    }
    if (append) {
        inputArguments.push_back(ArrayOf::characterArrayConstructor("-append"));
    }
    if (nocompression) {
        inputArguments.push_back(ArrayOf::characterArrayConstructor("-nocompression"));
    }
    return funcDef->evaluateFunction(eval, inputArguments, nLhs);
}
//=============================================================================
