//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QObject_iswidgettypeBuiltin.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
#include "QObjectHandleObject.hpp"
#include "iswidgettypeQObject.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::QmlEngineGateway::QObject_iswidgettypeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        std::string className;
        ClassName(param1, className);
        if (className != NLS_HANDLE_QOBJECT_CATEGORY_STR) {
            raiseError2(_E("nelson:validators:mustBeTypeAtPosition"), 1,
                utf8_to_wstring(NLS_HANDLE_QOBJECT_CATEGORY_STR));
        }
        retval.push_back(iswidgettypeQObject(param1));
    } else {
        raiseError2(
            _E("nelson:validators:mustBeTypeAtPosition"), 1, utf8_to_wstring(NLS_HANDLE_STR));
    }
    return retval;
}
//=============================================================================
