//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "qml_offlinestoragepathBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "QmlEngine.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::QmlEngineGateway::qml_offlinestoragepathBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    std::wstring res = QmlEngine::getInstance()->offlineStoragePath();
    retval.push_back(ArrayOf::characterArrayConstructor(res));
    return retval;
}
//=============================================================================
