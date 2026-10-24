//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__fcontour__Builtin.hpp"
#include "GOHelpers.hpp"
#include "GOConstructorHelpers.hpp"
#include "GOFunctionContour.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
__fcontour__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    retval << graphicsObjectHandle(GOCommonConstructorHelper(new GOFunctionContour, argIn));
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
