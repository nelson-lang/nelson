//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "uisaveBuiltin.hpp"
#include "GuiDialogs.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GuiGateway {
//=============================================================================
ArrayOfVector
uisaveBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 0, 2);
    UiSave(eval, argIn);
    return {};
}
//=============================================================================
} // namespace Nelson::GuiGateway
//=============================================================================
