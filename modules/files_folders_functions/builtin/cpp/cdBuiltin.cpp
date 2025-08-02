//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "cdBuiltin.hpp"
#include "ChangeDirectory.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GetCurrentDirectory.hpp"
#include "NelsonPrint.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::cdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.empty()) {
        std::wstring pwd = GetCurrentDirectory();
        if (pwd.empty()) {
            Error(_W("Impossible to get current directory."));
        } else {
            if (nLhs == 0) {
                NelsonPrint(pwd);
            } else {
                retval << ArrayOf::characterArrayConstructor(pwd);
            }
        }
    } else // argIn.size() == 1
    {
        if (argIn[0].isRowVectorCharacterArray()) {
            std::wstring wpath = argIn[0].getContentAsWideString();
            ArrayOf res = Cd(wpath);
            if (nLhs == 1) {
                retval << res;
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    }
    return retval;
}
//=============================================================================
