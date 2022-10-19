//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "FileSystemHelpers.hpp"
#include "rmpathBuiltin.hpp"
#include "Error.hpp"
#include "Warning.hpp"
#include "PathFuncManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::rmpathBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::wstring previousPaths = PathFuncManager::getInstance()->getPathNameAsString();
    ArrayOf param1 = argIn[0];
    if (param1.isRowVectorCharacterArray()) {
        std::wstring pathToRemove = param1.getContentAsWideString();
        if (isDirectory(pathToRemove)) {
            if (!PathFuncManager::getInstance()->removePath(pathToRemove)) {
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
