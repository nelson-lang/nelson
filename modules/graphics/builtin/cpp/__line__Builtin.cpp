//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__line__Builtin.hpp"
#include "GOConstructorHelpers.hpp"
#include "GOLineSeries.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
__line__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    retval << ArrayOf::graphicsObjectConstructor(
        GOCommonConstructorHelper(new GOLineSeries, argIn));
    return retval;
}
//=============================================================================
}
//=============================================================================
