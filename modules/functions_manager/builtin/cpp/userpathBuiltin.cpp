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
            std::wstring paramstr = param1.getContentAsWideString();
            if (paramstr == L"clear") {
                PathFunctionIndexerManager::getInstance()->clearUserPath(true);
            } else if (paramstr == L"reset") {
                PathFunctionIndexerManager::getInstance()->resetUserPath();
            } else {
                if (FileSystemWrapper::Path::is_directory(paramstr)) {
                    PathFunctionIndexerManager::getInstance()->setUserPath(paramstr, true);
                } else {
                    raiseError(L"Nelson:functions_manager:ERROR_NOT_AN_EXISTING_DIRECTORY",
                        ERROR_NOT_AN_EXISTING_DIRECTORY, paramstr);
                }
            }
        } else {
            raiseError2(L"nelson:validators:mustBeType", 1, NLS_STRING_ARRAY_STR);
        }
    } else {
        nargincheck(argIn, 0, 0);
        nargoutcheck(nLhs, 0, 1);
        retval << ArrayOf::characterArrayConstructor(
            PathFunctionIndexerManager::getInstance()->getUserPath());
    }
    return retval;
}
//=============================================================================
