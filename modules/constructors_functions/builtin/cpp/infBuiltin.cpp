//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include "infBuiltin.hpp"
#include "Error.hpp"
#include "Inf.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::infBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    uint32 m = 1;
    uint32 n = 1;
    ArrayOfVector retval;
    ArrayOf p;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.empty()) {
        m = 1;
        n = 1;
    } else {
        if (argIn[0].isNumeric()) {
            p = argIn[0];
            m = p.getContentAsInteger32Scalar();
            n = m;
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_NUMERIC_EXPECTED);
        }
        if (argIn.size() > 1) {
            if (argIn[0].isNumeric()) {
                p = argIn[1];
                n = p.getContentAsInteger32Scalar();
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_NUMERIC_EXPECTED);
            }
        }
    }
    ArrayOf Mat = Inf(m, n);
    retval.push_back(Mat);
    return retval;
}
//=============================================================================
