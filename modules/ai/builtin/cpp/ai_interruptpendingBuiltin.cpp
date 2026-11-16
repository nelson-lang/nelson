//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ai_interruptpendingBuiltin.hpp"
#include "AssistantSink.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// tf = __ai_interrupt_pending__([surfaceId]): true when a Stop has been requested
// for the given assistant surface ('web'/'qt'/'nflow'), consuming it. With no
// surfaceId it reads the shared (legacy) flag. The surface's poll loop checks
// this to stop cleanly; it reads a dedicated per-surface Stop flag, not the
// terminal interrupt, since the loop runs off the prompt where term.interrupt
// never reaches it.
ArrayOfVector
Nelson::AiGateway::ai_interruptpendingBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    bool pending;
    if (!argIn.empty()) {
        // Per-surface flag, falling back to the shared (legacy) flag so a host
        // that has not migrated to per-surface Stop still halts its one surface.
        const std::string surfaceId = argIn[0].asUtf8String();
        pending = Nelson::ConsumeAssistantStop(surfaceId) || Nelson::ConsumeAssistantStop();
    } else {
        pending = Nelson::ConsumeAssistantStop();
    }
    return ArrayOfVector { ArrayOf::logicalScalar(pending) };
}
//=============================================================================
