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
#include "ErrorID.hpp"
#include "i18n.hpp"
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
        retval.push_back(ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(QtConstants()));
    } break;
    case 1: {
        std::wstring constantName = argIn[0].asWideString();
        bool found;
        ArrayOf res = QtConstant(constantName, found);
        if (!found) {
            Error(L"Nelson:qt_constant:nameNotFound", L"Name not found.");
        } else {
            retval.push_back(res);
        }
    } break;
    default: {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    } break;
    }
    return retval;
}
//=============================================================================
