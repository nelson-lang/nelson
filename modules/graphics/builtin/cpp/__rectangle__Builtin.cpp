//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__rectangle__Builtin.hpp"
#include "GOConstructorHelpers.hpp"
#include "GORectangle.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
__rectangle__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << graphicsObjectHandle(GOCommonConstructorHelper(new GORectangle, argIn));
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
