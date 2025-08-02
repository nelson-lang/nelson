//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <float.h>
#include "uint64Builtin.hpp"
#include "ToInteger.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "Warning.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::IntegerGateway::uint64Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
        || argIn[0].isClassType()) {
        OverloadRequired("uint64");
    }
    if (argIn[0].isScalar() && argIn[0].isDoubleClass()) {
        double asDouble = argIn[0].getContentAsDoubleScalar();
        if (std::abs(asDouble) > (1ULL << DBL_MANT_DIG)) {
            Warning("Nelson:uint64:InputExceedsFlintmax",
                _("Numbers representing integers greater than or equal to flintmax might not "
                  "be represented exactly as double-precision floating-point values."));
        }
    }
    retval << ToInteger(NLS_UINT64, argIn[0]);
    return retval;
}
//=============================================================================
