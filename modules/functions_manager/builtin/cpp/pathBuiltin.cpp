//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include "pathBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "NelsonPrint.hpp"
#include "StringHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FileSystemWrapper.hpp"
#include "Warning.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Directories of a path string, in order; missing ones are skipped with a warning.
static wstringVector
existingDirectories(const std::wstring& pathString)
{
    wstringVector parts;
#ifdef _MSC_VER
    StringHelpers::split(parts, pathString, L';');
#else
    StringHelpers::split(parts, pathString, L':');
#endif
    wstringVector directories;
    directories.reserve(parts.size());
    for (const auto& part : parts) {
        if (part.empty()) {
            continue;
        }
        if (FileSystemWrapper::Path::is_directory(part)) {
            directories.push_back(part);
        } else {
            RaiseWarning(L"Nelson:path:notADirectory",
                resolveMessageTemplate(
                    L"Nelson:functions_manager:warningDirectory", L"Warning: Not a directory:")
                    + L" " + part + L"\n");
        }
    }
    return directories;
}
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::pathBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 2);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 0) {
        if (nLhs == 0) {
            wstringVector list = PathFunctionIndexerManager::getInstance()->getPathNameVector();
            if (list.empty()) {
                NelsonPrint(_W("The path is empty. Please restore path.") + L"\n");
            } else {
                NelsonPrint(_W("Nelson's search path contains the following directories:") + L"\n");
                NelsonPrint(L"\n");
                for (auto& k : list) {
                    NelsonPrint(L"	" + k + L"\n");
                }
            }
        } else {
            retval << ArrayOf::characterArray(
                PathFunctionIndexerManager::getInstance()->getPathNameAsString());
        }
    }
    if (argIn.size() == 1) {
        ArrayOf param1 = argIn[0];
        if (!param1.isRowVectorCharacterArray()) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
        wstringVector paths = existingDirectories(param1.asWideString());
        PathFunctionIndexerManager::getInstance()->clearUserPath();
        PathFunctionIndexerManager::getInstance()->clear();
        PathFunctionIndexerManager::getInstance()->addPaths(
            wstringVector(paths.rbegin(), paths.rend()), true, false);
    }
    if (argIn.size() == 2) {
        ArrayOf param1 = argIn[0];
        if (!param1.isRowVectorCharacterArray()) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
        ArrayOf param2 = argIn[1];
        if (!param2.isRowVectorCharacterArray()) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                L"string");
        }
        wstringVector paths1 = existingDirectories(param1.asWideString());
        wstringVector paths2 = existingDirectories(param2.asWideString());
        PathFunctionIndexerManager::getInstance()->clearUserPath();
        PathFunctionIndexerManager::getInstance()->clear();
        PathFunctionIndexerManager::getInstance()->addPaths(
            wstringVector(paths1.rbegin(), paths1.rend()), true, false);
        PathFunctionIndexerManager::getInstance()->addPaths(paths2, false, false);
    }
    return retval;
}
//=============================================================================
