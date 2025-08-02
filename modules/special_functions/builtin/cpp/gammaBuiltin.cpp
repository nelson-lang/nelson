//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gammaBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Gamma.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::gammaBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isSparse() || argIn[0].getDataClass() == NLS_DCOMPLEX
        || argIn[0].getDataClass() == NLS_SCOMPLEX) {
        Error(_W("Input argument must be dense and real."));
    }
    if (argIn[0].getDataClass() == NLS_DOUBLE || argIn[0].getDataClass() == NLS_SINGLE) {
        retval << Gamma(argIn[0]);
    } else {
        OverloadRequired("gamma");
    }
    return retval;
}
//=============================================================================
