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
#include "audiorecorder_playBuiltin.hpp"
#include "audiorecorder_playblockingBuiltin.hpp"
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
            start = param2.asDoubleScalar();
            startOnly = true;
            if (start < 1) {
                Error(L"Nelson:audioplayer:invalidStart", L"start >= 1 expected.");
            }
            start = start - 1;
        } else if (param2.isVector() && param2.isNumeric() && (param2.elementCount() == 2)) {
            param2.promoteType(NLS_DOUBLE);
            auto* ptr = (double*)param2.dataPointer();
            start = ptr[0];
            end = ptr[1];
            if (start < 1 || end < 1) {
                Error(L"Nelson:audio:invalidIndex", L"Index >= 1 expected.");
            }
            start = start - 1;
            end = end - 1;
        } else {
            Error(L"Nelson:audioplayer:invalidRange", L"scalar or [start, end] vector expected.");
        }
    }
    if (param1.handleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(L"Nelson:audiorecorder:invalidHandle", L"audiorecorder handle expected.");
    }
    auto* objRecorder = (AudiorecorderObject*)param1.asHandleScalar();
    ArrayOf player = objRecorder->getPlayer();
    auto* objPlayer = (AudioplayerObject*)player.asHandleScalar();
    if (argIn.size() == 1) {
        checkAudioStreamError(objPlayer->play());
    } else {
        if (startOnly) {
            if (start > objPlayer->getTotalSamples()) {
                Error(L"Nelson:audio:invalidRange", L"Invalid range.");
            }
            checkAudioStreamError(objPlayer->play(static_cast<int>(start)));
        } else {
            if (start > objPlayer->getTotalSamples() || start > end
                || end > objPlayer->getTotalSamples()) {
                Error(L"Nelson:audio:invalidRange", L"Invalid range.");
            }
            checkAudioStreamError(objPlayer->play(static_cast<int>(start), static_cast<int>(end)));
        }
    }
    if (blocking) {
        // Safety net: bound the wait by the expected playback duration plus a
        // margin so a backend that never signals completion cannot hang.
        bool hasDeadline = false;
        std::chrono::steady_clock::time_point deadline;
        int sampleRate = objPlayer->getSampleRate();
        int totalSamples = objPlayer->getTotalSamples();
        if (sampleRate > 0 && totalSamples > 0) {
            double expectedSeconds = static_cast<double>(totalSamples) / sampleRate;
            deadline = std::chrono::steady_clock::now()
                + std::chrono::milliseconds(
                    static_cast<long long>((expectedSeconds + 10.0) * 1000.0));
            hasDeadline = true;
        }
        do {
            processPendingCallbacksAndTimers(eval);
            if (eval->haveEventsLoop()) {
                Nelson::processGuiEvents(false);
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(1));
            if (hasDeadline && std::chrono::steady_clock::now() > deadline) {
                break;
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
} // namespace Nelson
//=============================================================================
