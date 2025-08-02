//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "rmfileBuiltin.hpp"
#include "Error.hpp"
#include "RemoveFile.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::rmfileBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 2);
    ArrayOf param1 = argIn[0];
    std::wstring filenameToDelete = param1.getContentAsWideString();
    std::wstring msg;
    bool bRes = false;
    switch (nLhs) {
    case 0: {
        bRes = RemoveFile(filenameToDelete, msg);
        if (static_cast<int>(bRes) == false) {
            Error(msg);
        }
    } break;
    case 1:
        bRes = RemoveFile(filenameToDelete, msg);
        retval << ArrayOf::logicalConstructor(bRes);
        break;
    case 2:
        bRes = RemoveFile(filenameToDelete, msg);
        retval << ArrayOf::logicalConstructor(bRes);
        retval << ArrayOf::characterArrayConstructor(msg);
        break;
    default:
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        break;
    }
    return retval;
}
//=============================================================================
