//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__parallelplot__Builtin.hpp"
#include "GOHelpers.hpp"
#include "GOConstructorHelpers.hpp"
#include "GOParallelPlot.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
__parallelplot__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    retval << graphicsObjectHandle(GOCommonConstructorHelper(new GOParallelPlot, argIn));
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
