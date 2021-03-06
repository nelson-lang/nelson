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
#include "getmodulesBuiltin.hpp"
#include "Error.hpp"
#include "ModulesManager.hpp"
#include "ToCellString.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::getmodulesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 4) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    bool bReverse = false;
    if (argIn.size() == 1) {
        std::wstring param;
        if (argIn[0].isRowVectorCharacterArray()) {
            param = argIn[0].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        if (param != L"reverse") {
            Error(_W("Wrong value for #1 argument, \'reverse\' expected."));
        }
        bReverse = true;
    }
    retval << ToCellStringAsColumn(GetModulesName(bReverse));
    if (nLhs > 1) {
        retval << ToCellStringAsColumn(GetModulesPath(bReverse));
    }
    if (nLhs > 2) {
        std::vector<versionElement> versionList = GetModulesVersion(bReverse);
        ArrayOf* pCells = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, versionList.size());
        Dimensions dims(versionList.size(), 1);
        ArrayOf data = ArrayOf(NLS_CELL_ARRAY, dims, pCells);
        for (size_t i = 0; i < versionList.size(); i++) {
            double* pData = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, 3);
            Dimensions dims(1, 3);
            pCells[i] = ArrayOf(NLS_DOUBLE, dims, pData);
            pData[0] = std::get<0>(versionList[i]);
            pData[1] = std::get<1>(versionList[i]);
            pData[2] = std::get<2>(versionList[i]);
        }
        retval << data;
    }
    if (nLhs > 3) {
        std::vector<bool> protectedList = GetModulesProtected(bReverse);
        logical* pData = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, protectedList.size());
        Dimensions dims(protectedList.size(), 1);
        ArrayOf data = ArrayOf(NLS_LOGICAL, dims, pData);
        for (size_t i = 0; i < protectedList.size(); i++) {
            pData[i] = protectedList[i];
        }
        retval << data;
    }
    return retval;
}
//=============================================================================
