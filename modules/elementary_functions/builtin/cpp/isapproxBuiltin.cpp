//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isapproxBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "IsApprox.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isapproxBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
        || argIn[0].isClassType()) {
        OverloadRequired("isapprox");
    }
    double precision = 0.;
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        precision = param3.getContentAsDoubleScalar();
    }
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    if (param1.isNumeric() && param2.isNumeric()) {
        if (param1.isSparse() || param2.isSparse()) {
            Error(_W("Sparse type not supported."));
        }
        if (param1.isComplex() || param2.isComplex()) {
            param1.promoteType(NLS_DCOMPLEX);
            param2.promoteType(NLS_DCOMPLEX);
        } else {
            param1.promoteType(NLS_DOUBLE);
            param2.promoteType(NLS_DOUBLE);
        }
        retval << ArrayOf::logicalConstructor(IsApprox(param1, param2, precision));
    } else {
        Error(_W("Numerics types expected."));
    }
    return retval;
}
//=============================================================================
