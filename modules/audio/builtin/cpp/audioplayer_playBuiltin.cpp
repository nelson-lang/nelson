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
#include "QueueProcessing.hpp"
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
                raiseError(L"Nelson:audio:ERROR_START_GE_1_EXPECTED", ERROR_START_GE_1_EXPECTED);
            }
            start = start - 1;
        } else if (param2.isVector() && param2.isNumeric() && (param2.getElementCount() == 2)) {
            param2.promoteType(NLS_DOUBLE);
            auto* ptr = (double*)param2.getDataPointer();
            start = ptr[0];
            end = ptr[1];
            if (start < 1 || end < 1) {
                raiseError(L"Nelson:audio:ERROR_INDEX_GE_1_EXPECTED", ERROR_INDEX_GE_1_EXPECTED);
            }
            start = start - 1;
            end = end - 1;
        } else {
            raiseError(L"Nelson:audio:ERROR_SCALAR_OR_START_END_VECTOR_EXPECTED",
                ERROR_SCALAR_OR_START_END_VECTOR_EXPECTED);
        }
    }
    if (param1.getHandleCategory() != NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR) {
        raiseError(
            L"Nelson:audio:ERROR_AUDIOPLAYER_HANDLE_EXPECTED", ERROR_AUDIOPLAYER_HANDLE_EXPECTED);
    }
    auto* objPlayer = (AudioplayerObject*)param1.getContentAsHandleScalar();
    if (argIn.size() == 1) {
        objPlayer->play();
    } else {
        if (startOnly) {
            if (start > objPlayer->getTotalSamples()) {
                raiseError(L"Nelson:audio:ERROR_INVALID_RANGE", ERROR_INVALID_RANGE);
            }
            objPlayer->play(static_cast<int>(start));
        } else {
            if (start > objPlayer->getTotalSamples() || start > end
                || end > objPlayer->getTotalSamples()) {
                raiseError(L"Nelson:audio:ERROR_INVALID_RANGE", ERROR_INVALID_RANGE);
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
