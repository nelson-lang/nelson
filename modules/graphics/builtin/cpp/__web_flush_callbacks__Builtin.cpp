//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__web_flush_callbacks__Builtin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "QueueProcessing.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// n = __web_flush_callbacks__(): runs every queued graphics callback now and
// returns how many ran. The web hosts call it right after a figure action
// evaluated while the interpreter is idle (a direct RPC), so the frames
// returned by that RPC already reflect the callbacks. A running script never
// needs it: its queued callbacks execute at the next drawnow()/pause().
ArrayOfVector
Nelson::GraphicsGateway::__web_flush_callbacks__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector result;
    result << ArrayOf::doubleScalar(static_cast<double>(processPendingGraphicsCallbacksNow()));
    return result;
}
//=============================================================================
