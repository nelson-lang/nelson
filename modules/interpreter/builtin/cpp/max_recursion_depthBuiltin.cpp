//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0) {
        size_t recursiondepth = eval->getContext()->getRecursionDepth();
        retval.push_back(ArrayOf::doubleConstructor((double)recursiondepth));
    } else if (argIn.size() == 1) {
        size_t previousrecursiondepth = eval->getContext()->getRecursionDepth();
        ArrayOf param1 = argIn[0];
        if (param1.isRowVectorCharacterArray()) {
            std::wstring param = param1.getContentAsWideString();
            if (param == L"max") {
                eval->getContext()->setRecursionDepth(
                    eval->getContext()->getMaximumRecursionDepth());
            } else {
                Error(_W("Argument #1: 'max' expected."));
            }
        } else {
            indexType value = param1.getContentAsScalarIndex();
            if (value <= (indexType)eval->getContext()->getMaximumRecursionDepth()) {
                eval->getContext()->setRecursionDepth((size_t)value);
            } else {
                Error(_W("Argument #1: valid value expected."));
            }
        }
        retval.push_back(ArrayOf::doubleConstructor((double)previousrecursiondepth));
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
