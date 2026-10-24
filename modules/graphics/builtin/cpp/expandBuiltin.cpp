//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "expandBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "UIComponentBuilder.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
expandBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    if (!argIn[0].isGraphicsObject()) {
        Error(L"Nelson:uicomponent:expectedTree", L"Tree or TreeNode graphics object expected.");
    }
    UITreeExpand(argIn[0].asGraphicsObjectScalar(), true);
    return {};
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
