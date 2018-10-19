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
#include "createGUIDBuiltin.hpp"
#include "CreateGUID.hpp"
#include "Error.hpp"
#include "ToCellString.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::createGUIDBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOfVector retval;
    if (argIn.size() == 0) {
        retval.push_back(ArrayOf::characterArrayConstructor(CreateGUID()));
    } else {
        ArrayOf arg1 = argIn[0];
        if (arg1.isNumeric()) {
            indexType idx = arg1.getContentAsScalarIndex();
            wstringVector strs = CreateGUID((size_t)idx);
            retval.push_back(ToCellStringAsColumn(strs));
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_DOUBLE_EXPECTED);
        }
    }
    return retval;
}
//=============================================================================
