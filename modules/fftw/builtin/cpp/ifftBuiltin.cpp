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
#include "ifftBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "InverseFft.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
ifftBuiltinPrivate(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    ArrayOf res;
    nargincheck(argIn, 1, 3);
    ArrayOf X = argIn[0];
    switch (argIn.size()) {
    case 1: {
        // ifft(X)
        res = InverseFft(X);
    } break;
    case 2: {
        ArrayOf N = argIn[1];
        if (N.isNumeric() && N.isEmpty()) {
            // ifft(X, []);
            res = InverseFft(X);
        } else {
            // ifft(X, n)
            indexType n = N.getContentAsScalarIndex(false);
            res = InverseFft(X, n);
        }
    } break;
    case 3: {
        // ifft(X, n, dim)
        ArrayOf N = argIn[1];
        ArrayOf DIM = argIn[2];
        indexType n;
        indexType dim = DIM.getContentAsScalarIndex(false);
        if (N.isNumeric() && N.isEmpty()) {
            // fft(X, [], dim)
            if (X.isScalar()) {
                n = 1;
            } else {
                n = X.getDimensionLength(static_cast<int>(dim) - 1);
            }
        } else {
            n = N.getContentAsScalarIndex(false);
        }
        res = InverseFft(X, n, dim - 1);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    retval << res;
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::FftwGateway::ifftBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1);
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "ifft", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassStruct()) {
            retval = OverloadFunction(eval, nLhs, argIn, "ifft", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        retval = ifftBuiltinPrivate(eval, nLhs, argIn);
    }
    return retval;
}
//=============================================================================
