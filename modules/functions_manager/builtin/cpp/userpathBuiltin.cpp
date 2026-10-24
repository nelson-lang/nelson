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
#include "userpathBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::userpathBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 1) {
        nargoutcheck(nLhs, 0, 0);
        ArrayOf param1 = argIn[0];
        if (param1.isRowVectorCharacterArray()) {
            std::wstring paramstr = param1.asWideString();
            if (paramstr == L"clear") {
                PathFunctionIndexerManager::getInstance()->clearUserPath(true);
            } else if (paramstr == L"reset") {
                PathFunctionIndexerManager::getInstance()->resetUserPath();
            } else {
                if (FileSystemWrapper::Path::is_directory(paramstr)) {
                    PathFunctionIndexerManager::getInstance()->setUserPath(paramstr, true);
                } else {
                    Error(L"Nelson:userpath:directoryDoesNotExist",
                        L"Not an existing directory: %s", paramstr);
                }
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
    } else {
        nargincheck(argIn, 0, 0);
        nargoutcheck(nLhs, 0, 1);
        retval << ArrayOf::characterArray(PathFunctionIndexerManager::getInstance()->getUserPath());
    }
    return retval;
}
//=============================================================================
