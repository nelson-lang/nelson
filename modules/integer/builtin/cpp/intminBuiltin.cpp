//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "intminBuiltin.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::IntegerGateway::intminBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.empty()) {
        int32 v = std::numeric_limits<int32>::min();
        retval << ArrayOf::int32Constructor(v);
    } else {
        ArrayOf param1 = argIn[0];
        if (!param1.isRowVectorCharacterArray()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring classInt = param1.getContentAsWideString();
        if (classInt == L"int8") {
            retval << ArrayOf::int8Constructor(-128);
        } else if (classInt == L"uint8") {
            retval << ArrayOf::uint8Constructor(0);
        } else if (classInt == L"int16") {
            retval << ArrayOf::int16Constructor(-32768);
        } else if (classInt == L"uint16") {
            retval << ArrayOf::uint16Constructor(0);
        } else if (classInt == L"int32") {
            int32 v = std::numeric_limits<int32>::min();
            retval << ArrayOf::int32Constructor(v);
        } else if (classInt == L"uint32") {
            retval << ArrayOf::uint32Constructor(0);
        } else if (classInt == L"int64") {
            int64 v = std::numeric_limits<int64>::min();
            retval << ArrayOf::int64Constructor(v);
        } else if (classInt == L"uint64") {
            retval << ArrayOf::uint64Constructor(0);
        } else {
            Error(_W("The name of an integer class expected."));
        }
    }
    return retval;
}
//=============================================================================
