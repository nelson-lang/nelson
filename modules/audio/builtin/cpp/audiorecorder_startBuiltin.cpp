//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiorecorder_startBuiltin.hpp"
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
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiorecorder_startBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    double durationSeconds = 0.0;
    bool blocking = false;
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(_W("audiorecorder handle expected."));
    }
    auto* objRec = (AudiorecorderObject*)param1.getContentAsHandleScalar();
    if (argIn.size() == 1) {
        objRec->start();
    } else {
        // duration provided
        objRec->start();
    }
    if (blocking && durationSeconds > 0.0) {
        double t0 = 0.0; // not used; use sleep
        // simple blocking loop
        uint32_t waitMs = static_cast<uint32_t>(durationSeconds * 1000.0);
        uint32_t waited = 0;
        while (waited < waitMs
            && !NelsonConfiguration::getInstance()->getInterruptPending(eval->getID())
            && objRec->getRunning()) {
            if (eval->haveEventsLoop()) {
                ProcessEventsDynamicFunctionWithoutWait();
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(20));
            waited += 20;
        }
        objRec->stop();
    }
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
