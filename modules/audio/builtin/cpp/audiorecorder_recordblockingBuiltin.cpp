//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiorecorder_recordblockingBuiltin.hpp"
#include "AudiorecorderObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <thread>
#include <chrono>
#include <cmath>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiorecorder_recordblockingBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 2, 2);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    if (param1.getHandleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(_W("audiorecorder handle expected."));
    }
    if (!param2.isScalar() || !param2.isNumeric()) {
        Error(_W("duration in seconds expected."));
    }
    double duration = param2.getContentAsDoubleScalar();
    if (duration <= 0) {
        Error(_W("duration > 0 expected."));
    }
    auto* objRec = (AudiorecorderObject*)param1.getContentAsHandleScalar();
    int sampleRate = objRec->getSampleRate();
    uint64_t targetFrames
        = static_cast<uint64_t>(std::ceil(duration * static_cast<double>(sampleRate)));
    uint64_t durationMs = static_cast<uint64_t>(std::ceil(duration * 1000.0));

    auto t0 = std::chrono::steady_clock::now();

    objRec->start();

    do {
        if (eval->haveEventsLoop()) {
            ProcessEventsDynamicFunctionWithoutWait();
        }
        // stop when reached target frames or interrupt requested
        if (NelsonConfiguration::getInstance()->getInterruptPending(eval->getID())) {
            break;
        }
        if (static_cast<uint64_t>(objRec->getTotalSamples()) >= targetFrames) {
            break;
        }
        // stop when elapsed time >= requested duration
        auto now = std::chrono::steady_clock::now();
        auto elapsedMs = std::chrono::duration_cast<std::chrono::milliseconds>(now - t0).count();
        if (static_cast<uint64_t>(elapsedMs) >= durationMs) {
            break;
        }
        // yield briefly to avoid busy spin while still processing events
        std::this_thread::yield();
    } while (true);

    objRec->stop();

    return retval;
}
//=============================================================================
