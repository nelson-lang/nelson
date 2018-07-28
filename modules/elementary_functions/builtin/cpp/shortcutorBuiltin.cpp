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
#include "shortcutorBuiltin.hpp"
#include "Error.hpp"
#include "OverloadBinaryOperator.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
needToOverload(ArrayOf a)
{
    return ((a.getDataClass() == NLS_STRUCT_ARRAY) || (a.getDataClass() == NLS_CELL_ARRAY)
        || a.isSparse() || a.isHandle());
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::shortcutorBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 2) {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    if (eval->overloadOnBasicTypes || needToOverload(A) || needToOverload(B)) {
        retval.push_back(OverloadBinaryOperator(eval, A, B, "shortcutpr"));
    } else {
        if (A.isScalar() && B.isScalar()) {
            bool a = A.getContentAsLogicalScalar();
            if (a) {
                retval.push_back(ArrayOf::logicalConstructor(a));
            } else {
                bool b = B.getContentAsLogicalScalar();
                retval.push_back(ArrayOf::logicalConstructor(a || b));
            }
        } else {
            std::wstring msg
                = _W("Operand to || operator must be convertible to logical scalar values.");
            Error(eval, msg);
        }
    }
    return retval;
}
//=============================================================================
