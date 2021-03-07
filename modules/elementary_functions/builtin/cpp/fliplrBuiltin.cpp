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
#include "fliplrBuiltin.hpp"
#include "Flip.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "TruncateFunctions.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::fliplrBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "fliplr", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload = false;
        ArrayOf res = Fliplr(argIn[0], needToOverload);
        if (needToOverload) {
            ArrayOfVector args = argIn;
            args.push_back(ArrayOf::doubleConstructor(2.));
            Context* context = eval->getContext();
            if (context != nullptr) {
                FunctionDef* funcDef = nullptr;
                if (context->lookupFunction("flip", funcDef)) {
                    if ((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                        || (funcDef->type() == NLS_MACRO_FUNCTION)) {
                        return funcDef->evaluateFunction(eval, args, nLhs);
                    }
                }
            }
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
