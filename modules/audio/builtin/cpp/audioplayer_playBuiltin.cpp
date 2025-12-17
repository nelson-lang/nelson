//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audioplayer_playBuiltin.hpp"
#include "AudioplayerObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "TimerQueue.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOfVector
audioplayer_playCommonBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool blocking)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    ArrayOf param1 = argIn[0];
    double start = -1;
    double end = -1;
    bool startOnly = false;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        if (param2.isScalar()) {
            start = param2.getContentAsDoubleScalar();
            startOnly = true;
            if (start < 1) {
                Error(_W("start >= 1 expected."));
            }
            start = start - 1;
        } else if (param2.isVector() && param2.isNumeric() && (param2.getElementCount() == 2)) {
            param2.promoteType(NLS_DOUBLE);
            auto* ptr = (double*)param2.getDataPointer();
            start = ptr[0];
            end = ptr[1];
            if (start < 1 || end < 1) {
                Error(_W("Index >= 1 expected."));
            }
            start = start - 1;
            end = end - 1;
        } else {
            Error(_W("scalar or [start, end] vector expected."));
        }
    }
    if (param1.getHandleCategory() != NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR) {
        Error(_W("audioplayer handle expected."));
    }
    auto* objPlayer = (AudioplayerObject*)param1.getContentAsHandleScalar();
    if (argIn.size() == 1) {
        objPlayer->play();
    } else {
        if (startOnly) {
            if (start > objPlayer->getTotalSamples()) {
                Error(_W("Invalid range."));
            }
            objPlayer->play(static_cast<int>(start));
        } else {
            if (start > objPlayer->getTotalSamples() || start > end
                || end > objPlayer->getTotalSamples()) {
                Error(_W("Invalid range."));
            }
            objPlayer->play(static_cast<int>(start), static_cast<int>(end));
        }
    }
    if (blocking) {
        do {
            TimerQueue::getInstance()->processCallback(eval);
            if (eval->haveEventsLoop()) {
                ProcessEventsDynamicFunctionWithoutWait();
            }
        } while (!NelsonConfiguration::getInstance()->getInterruptPending(eval->getID())
            && objPlayer->getRunning());
        objPlayer->stop();
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audioplayer_playblockingBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return audioplayer_playCommonBuiltin(eval, nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audioplayer_playBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return audioplayer_playCommonBuiltin(eval, nLhs, argIn, false);
}
//=============================================================================
}
//=============================================================================
