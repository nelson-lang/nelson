//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "matchesBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "StringMatches.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::matchesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() != 2 && argIn.size() != 4) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }

    bool ignoreCase = false;
    if (argIn.size() == 4) {
        ArrayOf param3 = argIn[2];
        std::wstring fieldname = param3.getContentAsWideString();
        if (fieldname != L"IgnoreCase") {
            Error(_W("Wrong value for #3: 'IgnoreCase' expected."));
        }
        ArrayOf param4 = argIn[3];
        ignoreCase = (param4.getContentAsLogicalScalar() != 0u);
    }

    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    retval << StringMatches(A, B, ignoreCase);
    return retval;
}
//=============================================================================
