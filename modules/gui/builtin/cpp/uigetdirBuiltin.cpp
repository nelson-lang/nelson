//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "uigetdirBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "UiGetDirectory.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GuiGateway::uigetdirBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 0, 2);
    std::wstring pathSelected;
    std::wstring pathOrigin;
    std::wstring title;
    if (!argIn.empty()) {
        if (argIn[0].isRowVectorCharacterArray()) {
            pathOrigin = argIn[0].asWideString();
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
    }
    if (argIn.size() > 1) {
        if (argIn[1].isRowVectorCharacterArray()) {
            title = argIn[1].asWideString();
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                L"string");
        }
    }
    bool bCancelled = UiGetDirectory(pathOrigin, title, pathSelected);
    if (bCancelled) {
        retval << ArrayOf::doubleScalar(0);
    } else {
        retval << ArrayOf::characterArray(pathSelected);
    }
    return retval;
}
//=============================================================================
