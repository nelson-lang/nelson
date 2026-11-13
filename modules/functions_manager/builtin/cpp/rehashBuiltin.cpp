//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "rehashBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::rehashBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 0, 1);
    if (argIn.size() == 1) {
        const std::wstring option
            = argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()
            ? argIn[0].asWideString()
            : std::wstring();
        // every option rescans the path and reloads the files changed on disk
        if (option != L"path" && option != L"toolbox" && option != L"pathreset"
            && option != L"toolboxreset" && option != L"toolboxcache") {
            Error(L"Nelson:rehash:invalidOption",
                L"Option must be 'path', 'toolbox', 'pathreset', 'toolboxreset' or "
                L"'toolboxcache'.");
        }
    }
    PathFunctionIndexerManager::getInstance()->rehash();
    return retval;
}
//=============================================================================
