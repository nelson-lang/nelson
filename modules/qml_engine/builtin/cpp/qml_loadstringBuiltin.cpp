//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "qml_loadstringBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "QmlEngine.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::QmlEngineGateway::qml_loadstringBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    QObjectHandleObject* qmlhandle
        = QmlEngine::getInstance()->setData(param1.getContentAsWideString());
    ArrayOfVector retval;
    retval.push_back(ArrayOf::handleConstructor(qmlhandle));
    return retval;
}
//=============================================================================
