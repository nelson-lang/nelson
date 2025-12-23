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
#include "audiorecorder_recordBuiltin.hpp"
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
ArrayOfVector
Nelson::AudioGateway::audiorecorder_recordblockingBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf handleArray = argIn[0];
    ArrayOf param2 = argIn[1];
    if (handleArray.getHandleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(_W("audiorecorder handle expected."));
    }
    if (!param2.isScalar() || !param2.isNumeric()) {
        Error(_W("duration in seconds expected."));
    }
    double duration = param2.getContentAsDoubleScalar();
    if (duration <= 0) {
        Error(_W("duration > 0 expected."));
    }
    auto* objRec = (AudiorecorderObject*)handleArray.getContentAsHandleScalar();
    if (!objRec) {
        Error(_W("Invalid audiorecorder handle."));
    }
    objRec->start();
    objRec->record(duration);
    do {
        processPendingCallbacksAndTimers(eval);
        if (eval->haveEventsLoop()) {
            ProcessEventsDynamicFunctionWithoutWait();
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
    if (handleArray.getHandleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(_W("audiorecorder handle expected."));
    }
    auto* objRec = (AudiorecorderObject*)handleArray.getContentAsHandleScalar();
    if (!objRec) {
        Error(_W("Invalid audiorecorder handle."));
    }
    double duration = -1.0;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        if (!param2.isScalar() || !param2.isNumeric()) {
            Error(_W("duration in seconds expected."));
        }
        duration = param2.getContentAsDoubleScalar();
        if (duration <= 0) {
            Error(_W("duration > 0 expected."));
        }
    }
    objRec->start();
    if (duration > 0) {
        objRec->record(duration);
    }
    return retval;
}
//=============================================================================
}
//=============================================================================
