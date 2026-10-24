//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <chrono>
#include <thread>
#include "audiorecorder_recordblockingBuiltin.hpp"
#include "audiorecorder_recordBuiltin.hpp"
#include "AudiorecorderObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "RuntimeDispatch.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "QueueProcessing.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiorecorder_recordblockingBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf handleArray = argIn[0];
    ArrayOf param2 = argIn[1];
    if (handleArray.handleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(L"Nelson:audiorecorder:invalidHandle", L"audiorecorder handle expected.");
    }
    if (!param2.isScalar() || !param2.isNumeric()) {
        Error(L"Nelson:audiorecorder_record:durationExpected", L"duration in seconds expected.");
    }
    double duration = param2.asDoubleScalar();
    if (duration <= 0) {
        Error(L"Nelson:audiorecorder_record:invalidDuration", L"duration > 0 expected.");
    }
    auto* objRec = (AudiorecorderObject*)handleArray.asHandleScalar();
    if (!objRec) {
        Error(L"Nelson:audiorecorder:invalidHandle", L"audiorecorder handle expected.");
    }
    checkAudioStreamError(objRec->start());
    checkAudioStreamError(objRec->record(duration));
    // Blocking recording lasts about `duration` seconds. A capture device that never
    // signals completion would hang the call, so the wait is bounded.
    auto deadline = std::chrono::steady_clock::now()
        + std::chrono::milliseconds(static_cast<long long>((duration + 10.0) * 1000.0));
    do {
        processPendingCallbacksAndTimers(eval);
        if (eval->haveEventsLoop()) {
            Nelson::processGuiEvents(false);
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
        if (std::chrono::steady_clock::now() > deadline) {
            break;
        }
    } while (!NelsonConfiguration::getInstance()->getInterruptPending(eval->getID())
        && objRec->getRunning());
    objRec->stop();
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiorecorder_recordBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf handleArray = argIn[0];
    if (handleArray.handleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(L"Nelson:audiorecorder:invalidHandle", L"audiorecorder handle expected.");
    }
    auto* objRec = (AudiorecorderObject*)handleArray.asHandleScalar();
    if (!objRec) {
        Error(L"Nelson:audiorecorder:invalidHandle", L"audiorecorder handle expected.");
    }
    double duration = -1.0;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        if (!param2.isScalar() || !param2.isNumeric()) {
            Error(
                L"Nelson:audiorecorder_record:durationExpected", L"duration in seconds expected.");
        }
        duration = param2.asDoubleScalar();
        if (duration <= 0) {
            Error(L"Nelson:audiorecorder_record:invalidDuration", L"duration > 0 expected.");
        }
    }
    checkAudioStreamError(objRec->start());
    if (duration > 0) {
        checkAudioStreamError(objRec->record(duration));
    }
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
