//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__size_changed_deferred__Builtin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "SizeChangedDispatcher.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
__size_changed_deferred__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 0);
    runDeferredSizeChanged();
    return {};
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
