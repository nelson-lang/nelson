//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "triuBuiltin.hpp"
#include "UpperTrianglePart.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::triuBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    signedIndexType offset = 0;
    ArrayOf A;
    nargincheck(argIn, 1, 2);
    switch (argIn.size()) {
    case 1: {
        A = argIn[0];
    } break;
    case 2: {
        A = argIn[0];
        ArrayOf param2 = argIn[1];
        double param2AsDouble = param2.getContentAsDoubleScalar();
        offset = (signedIndexType)std::trunc(param2AsDouble);
        if ((double)offset != param2AsDouble) {
            raiseError(
                L"Nelson:elementary_functions:ERROR_K_TH_DIAGONAL_INPUT_MUST_BE_AN_INTEGER_SCALAR",
                ERROR_K_TH_DIAGONAL_INPUT_MUST_BE_AN_INTEGER_SCALAR);
        }
    } break;
    default: {
        raiseError2(L"nelson:arguments:tooManyInputs");
    } break;
    }
    bool needToOverload;
    ArrayOf res = UpperTrianglePart(A, offset, needToOverload);
    if (needToOverload) {
        OverloadRequired("triu");
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
