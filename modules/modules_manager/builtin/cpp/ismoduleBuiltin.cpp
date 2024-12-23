//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ismoduleBuiltin.hpp"
#include "Error.hpp"
#include "ModulesManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::ismoduleBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    std::wstring moduleshortname;
    bool checkIsProtected = false;
    if (argIn.size() == 2) {
        std::wstring param = argIn[1].getContentAsWideString();
        if (param != L"isprotected") {
            Error(_("'isprotected' value expected."));
        }
        checkIsProtected = true;
    }
    if (argIn[0].isRowVectorCharacterArray() || (argIn[0].isStringArray() && argIn[0].isScalar())) {
        moduleshortname = argIn[0].getContentAsWideString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }

    bool bRes = checkIsProtected ? IsProtectedModuleName(moduleshortname)
                                 : IsExistingModuleName(moduleshortname);

    retval << ArrayOf::logicalConstructor(bRes);
    return retval;
}
//=============================================================================
