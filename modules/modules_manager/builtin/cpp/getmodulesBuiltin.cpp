//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "getmodulesBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ModulesManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::getmodulesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 4);
    bool bReverse = false;
    if (argIn.size() == 1) {
        std::wstring param;
        if (argIn[0].isRowVectorCharacterArray()) {
            param = argIn[0].asWideString();
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
        if (param != L"reverse") {
            Error(L"Nelson:getmodules:invalidValue",
                L"Wrong value for #%d argument, \'reverse\' expected.", 1);
        }
        bReverse = true;
    }
    retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(GetModulesName(bReverse));
    if (nLhs > 1) {
        retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(GetModulesPath(bReverse));
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
