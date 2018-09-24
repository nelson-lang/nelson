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
#include "COM_rangeBuiltin.hpp"
#include "ComExcelHelpers.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ComEngineGateway::COM_rangeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if ((argIn.size() == 0) || (argIn.size() > 2)) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOfVector retval;
    if (argIn.size() == 1) {
        ArrayOf param1 = argIn[0];
        std::wstring range = param1.getContentAsWideString();
        retval.push_back(ArrayOf::logicalConstructor(isValidRange(range)));
    } else {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        indexType m = param1.getContentAsScalarIndex();
        indexType n = param2.getContentAsScalarIndex();
        retval.push_back(ArrayOf::characterArrayConstructor(xlsIndexToRange(m, n)));
    }
    return retval;
}
//=============================================================================
