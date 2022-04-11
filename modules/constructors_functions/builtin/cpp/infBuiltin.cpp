//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "infBuiltin.hpp"
#include "Error.hpp"
#include "Inf.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::infBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    uint32 m = 1;
    uint32 n = 1;
    ArrayOf p;
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    if (argIn.empty()) {
        m = 1;
        n = 1;
    } else {
        if (argIn[0].isNumeric()) {
            p = argIn[0];
            m = p.getContentAsInteger32Scalar();
            n = m;
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_NUMERIC_EXPECTED);
        }
        if (argIn.size() > 1) {
            if (argIn[0].isNumeric()) {
                p = argIn[1];
                n = p.getContentAsInteger32Scalar();
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_NUMERIC_EXPECTED);
            }
        }
    }
    retval << Inf(m, n);
    return retval;
}
//=============================================================================
