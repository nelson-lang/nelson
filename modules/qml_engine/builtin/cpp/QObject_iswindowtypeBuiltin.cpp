//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QObject_iswindowtypeBuiltin.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "QObjectHandleObject.hpp"
#include "iswindowtypeQObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::QmlEngineGateway::QObject_iswindowtypeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        std::string className;
        ClassName(param1, className);
        if (className != NLS_HANDLE_QOBJECT_CATEGORY_STR) {
            Error(L"Nelson:QObject:notHandle", L"QObject handle expected.");
        }
        retval.push_back(iswindowtypeQObject(param1));
    } else {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    return retval;
}
//=============================================================================
