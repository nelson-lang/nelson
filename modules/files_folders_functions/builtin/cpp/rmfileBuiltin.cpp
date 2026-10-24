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
#include "ErrorID.hpp"
#include "RemoveFile.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
    std::wstring filenameToDelete = param1.asWideString();
    std::wstring msg;
    bool bRes = false;
    switch (nLhs) {
    case 0: {
        bRes = RemoveFile(filenameToDelete, msg);
        if (static_cast<int>(bRes) == false) {
            Error(L"Nelson:delete:removeFailed", msg);
        }
    } break;
    case 1:
        bRes = RemoveFile(filenameToDelete, msg);
        retval << ArrayOf::logicalScalar(bRes);
        break;
    case 2:
        bRes = RemoveFile(filenameToDelete, msg);
        retval << ArrayOf::logicalScalar(bRes);
        retval << ArrayOf::characterArray(msg);
        break;
    default:
        Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
        break;
    }
    return retval;
}
//=============================================================================
