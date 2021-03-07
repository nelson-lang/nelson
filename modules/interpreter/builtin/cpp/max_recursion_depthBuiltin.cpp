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
#include "max_recursion_depthBuiltin.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::max_recursion_depthBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    Context* context = eval->getContext();
    if (argIn.size() == 0) {
        size_t recursiondepth = context->getRecursionDepth();
        retval << ArrayOf::doubleConstructor((double)recursiondepth);
    } else if (argIn.size() == 1) {
        size_t previousrecursiondepth = context->getRecursionDepth();
        ArrayOf param1 = argIn[0];
        if (param1.isRowVectorCharacterArray()) {
            std::wstring param = param1.getContentAsWideString();
            if (param == L"max") {
                context->setRecursionDepth(context->getMaximumRecursionDepth());
            } else {
                Error(_W("Argument #1: 'max' expected."));
            }
        } else {
            indexType value = param1.getContentAsScalarIndex();
            if (value <= (indexType)context->getMaximumRecursionDepth()) {
                context->setRecursionDepth((size_t)value);
            } else {
                Error(_W("Argument #1: valid value expected."));
            }
        }
        retval << ArrayOf::doubleConstructor((double)previousrecursiondepth);
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
