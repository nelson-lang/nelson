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
#include "log2Builtin.hpp"
#include "Error.hpp"
#include "Logarithm2.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::log2Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 2);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "log2", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        if (nLhs < 2) {
            retval << Logarithm2(argIn[0], needToOverload);
        } else {
            retval = Frexp(argIn[0], needToOverload);
        }
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "log2");
        }
    }
    return retval;
}
//=============================================================================
