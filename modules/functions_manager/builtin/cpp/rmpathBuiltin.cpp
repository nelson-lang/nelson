//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "rmpathBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::rmpathBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::wstring previousPaths = PathFunctionIndexerManager::getInstance()->getPathNameAsString();
    ArrayOf param1 = argIn[0];
    if (param1.isRowVectorCharacterArray()) {
        std::wstring pathToRemove = param1.asWideString();
        if (FileSystemWrapper::Path::is_directory(pathToRemove)) {
            if (!PathFunctionIndexerManager::getInstance()->removePath(pathToRemove)) {
                RaiseWarning(L"Nelson:rmpath:notInPath",
                    resolveMessageTemplate(
                        L"Nelson:functions_manager:warningPath", L"Warning: Not in path:")
                        + L" " + pathToRemove + L"\n");
            }
        } else {
            RaiseWarning(L"Nelson:rmpath:notADirectory",
                resolveMessageTemplate(
                    L"Nelson:functions_manager:warningDirectory", L"Warning: Not a directory:")
                    + L" " + pathToRemove + L"\n");
        }
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    if (nLhs == 1) {
        retval << ArrayOf::characterArray(previousPaths);
    }
    return retval;
}
//=============================================================================
