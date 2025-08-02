//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "qt_constantBuiltin.hpp"
#include "QtConstants.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::QmlEngineGateway::qt_constantBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    switch (argIn.size()) {
    case 0: {
        retval.push_back(ArrayOf::toCellArrayOfCharacterColumnVectors(QtConstants()));
    } break;
    case 1: {
        std::wstring constantName = argIn[0].getContentAsWideString();
        bool found;
        ArrayOf res = QtConstant(constantName, found);
        if (!found) {
            Error(_W("Name not found."));
        } else {
            retval.push_back(res);
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
