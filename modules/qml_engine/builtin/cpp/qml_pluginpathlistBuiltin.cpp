//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "qml_pluginpathlistBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "QmlEngine.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::QmlEngineGateway::qml_pluginpathlistBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    wstringVector res = QmlEngine::getInstance()->pluginPathList();
    retval.push_back(ArrayOf::toCellArrayOfCharacterColumnVectors(res));
    return retval;
}
//=============================================================================
