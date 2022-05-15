//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QObject_fieldnamesBuiltin.hpp"
#include "Error.hpp"
#include "ToCellString.hpp"
#include "fieldnamesQObjectHandleObject.hpp"
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
        std::wstring param2str = param2.getContentAsWideString();
        if (param2str == L"-full") {
            fullList = true;
        } else {
            Error(_W("Unrecognized option. \"-full\" expected."));
        }
    }
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
    }
    wstringVector fieldnames;
    fieldnamesQObjectHandleObject(param1, fullList, fieldnames);
    retval.push_back(ToCellStringAsColumn(fieldnames));
    return retval;
}
//=============================================================================
