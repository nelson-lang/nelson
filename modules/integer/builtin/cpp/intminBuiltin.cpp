//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "intminBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::IntegerGateway::intminBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.empty()) {
        int32 v = std::numeric_limits<int32>::min();
        retval << ArrayOf::int32Constructor(v);
    } else {
        ArrayOf param1 = argIn[0];
        bool isSupportedInput
            = param1.isRowVectorCharacterArray() || (param1.isStringArray() && param1.isScalar());
        if (!isSupportedInput) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring classInt = param1.getContentAsWideString();
        if (classInt == L"int8") {
            retval << ArrayOf::int8Constructor(std::numeric_limits<int8>::min());
        } else if (classInt == L"uint8") {
            retval << ArrayOf::uint8Constructor(std::numeric_limits<uint8>::min());
        } else if (classInt == L"int16") {
            retval << ArrayOf::int16Constructor(std::numeric_limits<int16>::min());
        } else if (classInt == L"uint16") {
            retval << ArrayOf::uint16Constructor(std::numeric_limits<uint16>::min());
        } else if (classInt == L"int32") {
            retval << ArrayOf::int32Constructor(std::numeric_limits<int32>::min());
        } else if (classInt == L"uint32") {
            retval << ArrayOf::uint32Constructor(std::numeric_limits<uint32>::min());
        } else if (classInt == L"int64") {
            retval << ArrayOf::int64Constructor(std::numeric_limits<int64>::min());
        } else if (classInt == L"uint64") {
            retval << ArrayOf::uint64Constructor(std::numeric_limits<uint64>::min());
        } else {
            Error(_W("The name of an integer class expected."));
        }
    }
    return retval;
}
//=============================================================================
