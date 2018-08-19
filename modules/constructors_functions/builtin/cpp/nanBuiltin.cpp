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
#include "nanBuiltin.hpp"
#include "Error.hpp"
#include "NaN.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::nanBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    uint32 m = 1;
    uint32 n = 1;
    ArrayOfVector retval;
    ArrayOf p;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0) {
        m = 1;
        n = 1;
    } else if (argIn.size() > 0) {
        p = argIn[0];
        m = p.getContentAsInteger32Scalar();
        n = m;
    }
    if (argIn.size() > 1) {
        p = argIn[1];
        n = p.getContentAsInteger32Scalar();
    }
    ArrayOf Mat = NaN(m, n);
    retval.push_back(Mat);
    return retval;
}
//=============================================================================
