//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "listdlgBuiltin.hpp"
#include "GuiDialogs.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GuiGateway {
//=============================================================================
ArrayOfVector
listdlgBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 0);
    return ListDialog(argIn);
}
//=============================================================================
} // namespace Nelson::GuiGateway
//=============================================================================
