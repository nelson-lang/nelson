//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__functionline__Builtin.hpp"
#include "GOHelpers.hpp"
#include "GOConstructorHelpers.hpp"
#include "GOFunctionLine.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
__functionline__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    retval << graphicsObjectHandle(GOCommonConstructorHelper(new GOFunctionLine, argIn));
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
