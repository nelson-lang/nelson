//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QObject_fieldnamesBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "fieldnamesQObjectHandleObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::QmlEngineGateway::QObject_fieldnamesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    bool fullList = false;
    ArrayOfVector retval;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        std::wstring param2str = param2.asWideString();
        if (param2str == L"-full") {
            fullList = true;
        } else {
            Error(
                L"Nelson:QObject:unrecognizedOption", L"Unrecognized option. \"-full\" expected.");
        }
    }
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    wstringVector fieldnames;
    fieldnamesQObjectHandleObject(param1, fullList, fieldnames);
    retval.push_back(ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(fieldnames));
    return retval;
}
//=============================================================================
