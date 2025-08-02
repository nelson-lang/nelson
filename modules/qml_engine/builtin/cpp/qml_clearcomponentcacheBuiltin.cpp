//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "qml_clearcomponentcacheBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "QmlEngine.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::QmlEngineGateway::qml_clearcomponentcacheBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 0);
    QmlEngine::getInstance()->clearComponentCache();
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
