//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "getweburlBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "WebDesktopBridge.hpp"
//=============================================================================
namespace Nelson::EngineGateway {
//=============================================================================
ArrayOfVector
getweburlBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 2);
    auto* bridge = WebDesktopBridge::getInstance();
    ArrayOfVector result;
    result << ArrayOf::characterArray(bridge->getWebLaunchUrl());
    if (nLhs > 1) {
        result << ArrayOf::doubleScalar(static_cast<double>(bridge->getWebLaunchPort()));
    }
    return result;
}
//=============================================================================
} // namespace Nelson::EngineGateway
//=============================================================================
