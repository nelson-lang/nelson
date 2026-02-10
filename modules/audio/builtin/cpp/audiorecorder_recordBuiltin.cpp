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
        raiseError(L"Nelson:audio:ERROR_AUDIORECORDER_HANDLE_EXPECTED",
            ERROR_AUDIORECORDER_HANDLE_EXPECTED);
    }
    if (!param2.isScalar() || !param2.isNumeric()) {
        raiseError(
            L"Nelson:audio:ERROR_DURATION_IN_SECONDS_EXPECTED", ERROR_DURATION_IN_SECONDS_EXPECTED);
    }
    double duration = param2.getContentAsDoubleScalar();
    if (duration <= 0) {
        raiseError(
            L"Nelson:audio:ERROR_DURATION_GT_ZERO_EXPECTED", ERROR_DURATION_GT_ZERO_EXPECTED);
    }
    auto* objRec = (AudiorecorderObject*)handleArray.getContentAsHandleScalar();
    if (!objRec) {
        raiseError(
            L"Nelson:audio:ERROR_INVALID_AUDIORECORDER_HANDLE", ERROR_INVALID_AUDIORECORDER_HANDLE);
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
        raiseError(L"Nelson:audio:ERROR_AUDIORECORDER_HANDLE_EXPECTED",
            ERROR_AUDIORECORDER_HANDLE_EXPECTED);
    }
    auto* objRec = (AudiorecorderObject*)handleArray.getContentAsHandleScalar();
    if (!objRec) {
        raiseError(
            L"Nelson:audio:ERROR_INVALID_AUDIORECORDER_HANDLE", ERROR_INVALID_AUDIORECORDER_HANDLE);
    }
    double duration = -1.0;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        if (!param2.isScalar() || !param2.isNumeric()) {
            raiseError(L"Nelson:audio:ERROR_DURATION_IN_SECONDS_EXPECTED",
                ERROR_DURATION_IN_SECONDS_EXPECTED);
        }
        duration = param2.getContentAsDoubleScalar();
        if (duration <= 0) {
            raiseError(
                L"Nelson:audio:ERROR_DURATION_GT_ZERO_EXPECTED", ERROR_DURATION_GT_ZERO_EXPECTED);
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
