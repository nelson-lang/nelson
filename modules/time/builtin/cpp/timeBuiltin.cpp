//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "timeBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Time.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    switch (argIn.size()) {
    case 0: {
        retval << ArrayOf::doubleConstructor(TimeAsSeconds());
    } break;
    case 1: {
        std::wstring param1 = argIn[0].getContentAsWideString();
        if (param1 == L"ns") {
            retval << ArrayOf::uint64Constructor(TimeAsNanoSeconds());
        } else if (param1 == L"s") {
            retval << ArrayOf::doubleConstructor(TimeAsSeconds());
        } else {
            Error(_W("Argument #2: 'ns' or 's' expected."));
        }
    } break;
    }
    return retval;
}
//=============================================================================
