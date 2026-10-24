//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ismembertolBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "IsMemberTol.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::ismembertolBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 2);
    double tol = -1.0; // sentinel: use class-dependent default
    if (argIn.size() == 3) {
        if (!argIn[2].isScalar() || !argIn[2].isNumeric()) {
            Error("Nelson:data_analysis:TOLNonnegativeNumericScalar",
                "TOL must be a nonnegative numeric scalar.");
        }
        tol = argIn[2].asDoubleScalar();
        if (!(tol >= 0.0)) {
            Error("Nelson:data_analysis:TOLNonnegativeNumericScalar",
                "TOL must be a nonnegative numeric scalar.");
        }
    }
    bool computeLocb = (nLhs >= 2);
    bool needToOverload = false;
    ArrayOf lia;
    ArrayOf locb;
    IsMemberTol(argIn[0], argIn[1], tol, computeLocb, lia, locb, needToOverload);
    if (needToOverload) {
        OverloadRequired("ismembertol");
    }
    retval << lia;
    if (computeLocb) {
        retval << locb;
    }
    return retval;
}
//=============================================================================
