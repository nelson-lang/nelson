//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "COM_fieldnamesBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "fieldnamesComHandleObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ComEngineGateway::COM_fieldnamesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    bool fullList = false;
    ArrayOfVector retval;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        std::wstring param2str = param2.getContentAsWideString();
        if (param2str == L"-full") {
            fullList = true;
        } else {
            raiseError(L"Nelson:com_engine:ERROR_COM_FIELDNAMES_UNRECOGNIZED_OPTION",
                ERROR_COM_FIELDNAMES_UNRECOGNIZED_OPTION);
        }
    }
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        raiseError2(L"Nelson:error_manager:wrong_type_with_expected", 1, NLS_HANDLE_STR);
    }
    wstringVector fieldnames;
    fieldnamesComHandleObject(param1, fullList, fieldnames);
    retval << ArrayOf::toCellArrayOfCharacterColumnVectors(fieldnames);
    return retval;
}
//=============================================================================
