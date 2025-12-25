//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiorecorder_playBuiltin.hpp"
#include "audiorecorder_playblockingBuiltin.hpp"
#include "AudiorecorderObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "QueueProcessing.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOfVector
audiorecoder_playCommonBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool blocking)
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
    if (param1.getHandleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(_W("audiorecorder handle expected."));
    }
    auto* objRecorder = (AudiorecorderObject*)param1.getContentAsHandleScalar();
    ArrayOf player = objRecorder->getPlayer();
    auto* objPlayer = (AudioplayerObject*)player.getContentAsHandleScalar();
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
            processPendingCallbacksAndTimers(eval);
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
Nelson::AudioGateway::audiorecorder_playblockingBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return audiorecoder_playCommonBuiltin(eval, nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiorecorder_playBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return audiorecoder_playCommonBuiltin(eval, nLhs, argIn, false);
}
//=============================================================================
}
//=============================================================================
