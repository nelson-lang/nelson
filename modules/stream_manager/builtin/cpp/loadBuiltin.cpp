//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "loadBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "IsValidVariableName.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "FileSystemWrapper.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// load(filename, '-mat') regardless file extension force as .mat
// load(filename, '-nh5') regardless file extension force as .nh5
// load(filename, variables)
//=============================================================================
static bool
isOption(const std::wstring& param);
//=============================================================================
static std::wstring
resolveLoadFilename(const std::wstring& paramFilename);
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::loadBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);
    std::wstring paramFilename = argIn[0].asWideString();
    if (paramFilename.empty()) {
        Error(L"Nelson:stream_manager:emptyFilename", L"Empty filename.");
    }
    wstringVector names;
    bool forceAsMat = false;
    bool forceAsNh5 = false;
    for (size_t k = 1; k < argIn.size(); k++) {
        ArrayOf paramK = argIn[k];
        std::wstring param = paramK.asWideString();
        if (isOption(param)) {
            if (param == L"-mat" && !forceAsNh5) {
                forceAsMat = true;
            } else if (param == L"-nh5" && !forceAsMat) {
                forceAsNh5 = true;
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
    std::wstring resolvedFilename = resolveLoadFilename(paramFilename);
    if (resolvedFilename.empty()) {
        Error(L"Nelson:load:fileDoesNotExist", L"File does not exist.");
    }
    paramFilename = resolvedFilename;

    std::string loadFunctionName;
    if (forceAsMat || forceAsNh5) {
        if (forceAsNh5) {
            loadFunctionName = "loadnh5";
        } else {
            loadFunctionName = "loadmat";
        }
    } else {
        bool isNh5 = false;
        bool isMat = false;
        FunctionDef* funcDef = nullptr;
        if (!PathFunctionIndexerManager::getInstance()->find("isnh5file", funcDef)) {
            BuiltInFunctionDefManager::getInstance()->find("isnh5file", funcDef);
        }
        if (funcDef) {
            ArrayOfVector inputArguments;
            inputArguments.push_back(ArrayOf::characterArray(paramFilename));
            ArrayOfVector res = funcDef->evaluateFunction(eval, inputArguments, 1);
            if (res.size() == 1) {
                isNh5 = res[0].asLogicalScalar();
            }
        }

        if (!isNh5) {
            funcDef = nullptr;
            if (!PathFunctionIndexerManager::getInstance()->find("ismatfile", funcDef)) {
                BuiltInFunctionDefManager::getInstance()->find("ismatfile", funcDef);
            }
            if (funcDef) {
                ArrayOfVector inputArguments;
                inputArguments.push_back(ArrayOf::characterArray(paramFilename));
                ArrayOfVector res = funcDef->evaluateFunction(eval, inputArguments, 1);
                if (res.size() == 1) {
                    isMat = res[0].asLogicalScalar();
                }
            }
        }
        if (isNh5) {
            loadFunctionName = "loadnh5";
        } else if (isMat) {
            loadFunctionName = "loadmat";
        } else {
            loadFunctionName = "loadnh5";
        }
    }

    FunctionDef* funcDef = nullptr;
    if (!PathFunctionIndexerManager::getInstance()->find(loadFunctionName, funcDef)) {
        if (!BuiltInFunctionDefManager::getInstance()->find(loadFunctionName, funcDef)) {
            Error(L"Nelson:load:functionExpected", L"load function expected.");
        }
    }
    ArrayOfVector inputArguments;
    inputArguments.push_back(ArrayOf::characterArray(paramFilename));
    for (const std::wstring& name : names) {
        inputArguments.push_back(ArrayOf::characterArray(name));
    }
    return funcDef->evaluateFunction(eval, inputArguments, nLhs);
}
//=============================================================================
static bool
isOption(const std::wstring& param)
{
    return param.size() > 2 && param[0] == L'-';
}
//=============================================================================
static std::wstring
resolveLoadFilename(const std::wstring& paramFilename)
{
    // 1) as given: absolute path or relative to the current directory
    if (FileSystemWrapper::Path::is_regular_file(paramFilename)) {
        return paramFilename;
    }
    const FileSystemWrapper::Path asPath(paramFilename);
    const bool hasExtension = asPath.has_extension();
    // 2) no extension: try the native .nh5 then .mat in the current directory
    if (!hasExtension) {
        const std::wstring withNh5 = paramFilename + L".nh5";
        if (FileSystemWrapper::Path::is_regular_file(withNh5)) {
            return withNh5;
        }
        const std::wstring withMat = paramFilename + L".mat";
        if (FileSystemWrapper::Path::is_regular_file(withMat)) {
            return withMat;
        }
    }
    // 3) search the path (only for a bare/relative name, never an absolute one)
    if (!asPath.is_absolute()) {
        const wstringVector paths = PathFunctionIndexerManager::getInstance()->getPathNameVector();
        for (const std::wstring& dir : paths) {
            const FileSystemWrapper::Path candidate = FileSystemWrapper::Path(dir) / paramFilename;
            if (FileSystemWrapper::Path::is_regular_file(candidate)) {
                return candidate.wstring();
            }
            if (!hasExtension) {
                const FileSystemWrapper::Path candNh5
                    = FileSystemWrapper::Path(dir) / (paramFilename + L".nh5");
                if (FileSystemWrapper::Path::is_regular_file(candNh5)) {
                    return candNh5.wstring();
                }
                const FileSystemWrapper::Path candMat
                    = FileSystemWrapper::Path(dir) / (paramFilename + L".mat");
                if (FileSystemWrapper::Path::is_regular_file(candMat)) {
                    return candMat.wstring();
                }
            }
        }
    }
    return L"";
}
//=============================================================================
