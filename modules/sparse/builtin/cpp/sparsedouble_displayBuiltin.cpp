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
#include "sparsedouble_displayBuiltin.hpp"
#include "Error.hpp"
#include "SparseDisplay.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::sparsedouble_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    std::string name;
    if (argIn.size() == 2) {
        name = argIn[1].getContentAsCString();
    }
    if (!argIn[0].isSparse()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_DOUBLE_EXPECTED);
    } else {
        if (argIn[0].getDataClass() == NLS_DOUBLE || argIn[0].getDataClass() == NLS_DCOMPLEX) {
            Interface* io = eval->getInterface();
            SparseDisplay(io, argIn[0], name);
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_DOUBLE_EXPECTED);
        }
    }
    return retval;
}
//=============================================================================
