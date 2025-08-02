//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "relativepathBuiltin.hpp"
#include "Error.hpp"
#include "RelativePath.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::relativepathBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    std::wstring param1 = argIn[0].getContentAsWideString();
    std::wstring param2 = argIn[1].getContentAsWideString();
    bool bSuccess = false;
    std::wstring result = RelativePath(param1, param2, bSuccess);
    if (bSuccess) {
        bool needToAddDot = !(result[0] == L'/' || result[0] == L'\\' || result[0] == L'.');
        if (needToAddDot) {
            result = L"./" + result;
        }
    }
    retval << ArrayOf::characterArrayConstructor(result);
    return retval;
}
//=============================================================================
