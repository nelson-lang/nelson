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
        std::wstring pathToRemove = param1.getContentAsWideString();
        if (FileSystemWrapper::Path::is_directory(pathToRemove)) {
            if (!PathFunctionIndexerManager::getInstance()->removePath(pathToRemove)) {
                Warning(_W("Warning: Not in path:") + L" " + pathToRemove + L"\n");
            }
        } else {
            Warning(_W("Warning: Not a directory:") + L" " + pathToRemove + L"\n");
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    if (nLhs == 1) {
        retval << ArrayOf::characterArrayConstructor(previousPaths);
    }
    return retval;
}
//=============================================================================
