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
#include "ErrorID.hpp"
#include "IsValidVariableName.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "FileSystemWrapper.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Context.hpp"
#include "characters_encoding.hpp"
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
    std::wstring paramFilename = argIn[0].asWideString();
    if (paramFilename.empty()) {
        paramFilename = L"nelson.nh5";
    }
    wstringVector names;
    bool forceAsMat = false;
    bool forceAsNh5 = false;
    bool nocompression = false;
    bool append = false;
    std::wstring version;
    std::wstring structName;
    for (size_t k = 1; k < argIn.size(); k++) {
        ArrayOf paramK = argIn[k];
        std::wstring param = paramK.asWideString();
        if (isOption(param)) {
            if (param == L"-struct") {
                if (!structName.empty()) {
                    Error(L"Nelson:save:structSourcesExclusive",
                        L"Only one structure can be saved with -struct.");
                }
                if (k + 1 >= argIn.size()) {
                    Error(L"Nelson:save:missingStructArg",
                        L"-struct must be followed by the name of a scalar structure variable.");
                }
                structName = argIn[++k].asWideString();
            } else if (param == L"-mat" && !forceAsNh5) {
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
                Error(L"Nelson:stream_manager:invalidOption", L"Valid option expected.");
            }
        } else {
            if (IsValidVariableName(param)) {
                names.push_back(param);
            } else {
                Error(
                    L"Nelson:stream_manager:invalidVariableName", L"Valid variable name expected.");
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
        Error(L"Nelson:save:functionExpected", L"%s function expected.", L"save");
    }

    FunctionDef* funcDef = nullptr;
    if (!PathFunctionIndexerManager::getInstance()->find(saveFunctionName, funcDef)) {
        if (!BuiltInFunctionDefManager::getInstance()->find(saveFunctionName, funcDef)) {
            Error(L"Nelson:save:functionExpected", L"%s function expected.", L"load");
        }
    }

    // with -struct, the listed names are fields saved as variables from a temporary scope
    ArrayOfVector structFields;
    if (!structName.empty()) {
        ArrayOf source;
        Context* context = eval->getContext();
        if (!context->getCurrentScope()->lookupVariable(wstring_to_utf8(structName), source)
            || !source.isStruct() || source.isClassType() || !source.isScalar()) {
            Error(L"Nelson:save:structArgNotScalarStruct",
                L"-struct expects the name of a scalar structure variable.");
        }
        if (names.empty()) {
            for (const std::string& field : source.fieldNames()) {
                names.push_back(utf8_to_wstring(field));
            }
        }
        for (const std::wstring& name : names) {
            if (!source.isField(wstring_to_utf8(name))) {
                Error(L"Nelson:save:fieldNotFoundInStruct",
                    L"Variable '%s' has no field named '%s'.", structName, name);
            }
            structFields.push_back(source.fieldAsList(wstring_to_utf8(name))[0]);
        }
    }

    ArrayOfVector inputArguments;
    inputArguments.push_back(ArrayOf::characterArray(paramFilename));
    for (const std::wstring& name : names) {
        inputArguments.push_back(ArrayOf::characterArray(name));
    }
    if (!version.empty()) {
        inputArguments.push_back(ArrayOf::characterArray(version));
    }
    if (append) {
        inputArguments.push_back(ArrayOf::characterArray("-append"));
    }
    if (nocompression) {
        inputArguments.push_back(ArrayOf::characterArray("-nocompression"));
    }
    if (structName.empty()) {
        return funcDef->evaluateFunction(eval, inputArguments, nLhs);
    }
    Context* context = eval->getContext();
    context->pushScope("save");
    for (size_t k = 0; k < names.size(); ++k) {
        context->insertVariable(wstring_to_utf8(names[k]), structFields[k]);
    }
    ArrayOfVector retval;
    try {
        retval = funcDef->evaluateFunction(eval, inputArguments, nLhs);
    } catch (...) {
        context->popScope();
        throw;
    }
    context->popScope();
    return retval;
}
//=============================================================================
