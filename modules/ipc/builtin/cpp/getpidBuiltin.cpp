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
#include "getpidBuiltin.hpp"
#include "Error.hpp"
#include "NelsonPIDs.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::IpcGateway::getpidBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    switch (argIn.size()) {
    case 0: {
        retval.push_back(ArrayOf::doubleConstructor((double)getCurrentPID()));
    } break;
    case 1: {
        std::wstring param = argIn[0].getContentAsWideString();
        if (param == L"available") {
            std::vector<int> pids = getNelsonPIDs();
            ArrayOf res;
            Dimensions dims(1, pids.size());
            double* pd = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, (indexType)pids.size());
            res = ArrayOf(NLS_DOUBLE, dims, pd);
            for (indexType k = 0; k < dims.getElementCount(); ++k) {
                pd[k] = (double)pids[k];
            }
            retval.push_back(res);
        } else {
            Error(_("Wrong value for #1 argument: 'available' expected."));
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
