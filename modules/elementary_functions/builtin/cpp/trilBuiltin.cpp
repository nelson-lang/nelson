//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "trilBuiltin.hpp"
#include "LowerTrianglePart.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::trilBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
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
    ArrayOf res = LowerTrianglePart(A, offset, needToOverload);
    if (needToOverload) {
        retval = OverloadFunction(eval, nLhs, argIn, "tril");
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
