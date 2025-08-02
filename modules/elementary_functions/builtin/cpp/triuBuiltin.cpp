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
            Error(_W("K-th diagonal input must be an integer scalar."));
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
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
