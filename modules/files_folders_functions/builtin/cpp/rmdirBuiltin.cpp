//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "rmdirBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "RemoveDirectory.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::rmdirBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 2);
    bool bbSubfolder = false;
    std::wstring arg1 = argIn[0].asWideString();
    if (argIn.size() == 2) {
        std::wstring arg2 = argIn[1].asWideString();
        if ((arg2 == L"s") || (arg2 == L"S")) {
            bbSubfolder = true;
        } else {
            Error("Nelson:rmdir:invalidOption", "'s' expected.");
        }
    }
    std::wstring errorMessage;
    bool res = RemoveDirectory(arg1, bbSubfolder, errorMessage);
    if (nLhs == 0) {
        if (static_cast<int>(res) == false) {
            Error(L"Nelson:rmdir:removeFailed", errorMessage);
        }
    } else {
        retval << ArrayOf::logicalScalar(res);
        if (nLhs > 1) {
            retval << ArrayOf::characterArray(errorMessage);
        }
    }
    return retval;
}
//=============================================================================
