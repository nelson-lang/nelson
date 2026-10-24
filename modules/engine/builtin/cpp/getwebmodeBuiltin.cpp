//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "getwebmodeBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "WebDesktopBridge.hpp"
//=============================================================================
namespace Nelson::EngineGateway {
//=============================================================================
ArrayOfVector
getwebmodeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector result;
    result << ArrayOf::characterArray(WebDesktopBridge::getInstance()->getWebLaunchMode());
    return result;
}
//=============================================================================
} // namespace Nelson::EngineGateway
//=============================================================================
