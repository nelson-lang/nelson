//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <chrono>
#include <condition_variable>
#include <mutex>
#include <string>
#include <thread>
#include <vector>
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "HandleGenericObject.hpp"
#include "nlsTime_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSTIME_IMPEXP TimerObject : public HandleGenericObject
{
public:
    TimerObject();
    ~TimerObject() override;

    void
    onDelete(Evaluator* eval) override;

    bool
    get(const std::wstring& propertyName, ArrayOf& res);
    bool
    set(const std::wstring& propertyName, const ArrayOf& propertyValue, std::wstring& errorMessage);
    bool
    isWritableProperty(const std::wstring& propertyName);
    bool
    isProperty(const std::wstring& propertyName) override;
    bool
    isMethod(const std::wstring& methodName) override;
    wstringVector
    properties() override;
    wstringVector
    methods() override;

    bool
    start(double delaySeconds, std::wstring& errorMessage);
    void
    stop(bool runStopCallback);
    void
    wait(Evaluator* eval);
    bool
    disp(Interface* io);
    void
    retainUntilDeleted();
    void
    releaseRetainedReference();

#ifdef __EMSCRIPTEN__
    // WebAssembly is single-threaded: there is no timer worker thread, so the
    // clock is driven cooperatively. This services every live timer that is due
    // and is called from the shared processPendingCallbacksAndTimers() drain.
    static void
    pumpActiveTimers();
#endif

    ArrayOf
    toStruct();
    std::wstring
    getObjectVisibility();

private:
    wstringVector propertiesNames;
    wstringVector methodsNames;

    ArrayOf timerFunc;
    ArrayOf startFunc;
    ArrayOf stopFunc;
    ArrayOf errorFunc;
    ArrayOf userData;

    double period;
    double startDelay;
    double tasksToExecute;
    std::wstring busyMode;
    std::wstring executionMode;
    std::wstring name;
    std::wstring tag;
    std::wstring objectVisibility;
    double averagePeriod;
    double instantPeriod;
    int tasksExecuted;
    double tasksLimitThisRun;
    int timerCallbacksInQueue;
    int fixedRatePendingCallbacks;
    bool running;
    bool stopRequested;
    bool fixedRateErrorPending;
    bool hasLastExecutionClock;
    std::chrono::steady_clock::time_point lastExecutionClock;
    double totalPeriod;
    std::thread worker;
    std::mutex mutex;
    std::condition_variable condition;

#ifdef __EMSCRIPTEN__
    // Next time this timer is due to fire (cooperative clock, see
    // pumpActiveTimers). Set by start(), advanced by wasmServiceTick().
    std::chrono::steady_clock::time_point wasmNextDue;
    void
    wasmServiceTick(const std::chrono::steady_clock::time_point& now);
#endif

    bool
    setCallbackProperty(
        ArrayOf& destination, const ArrayOf& propertyValue, std::wstring& errorMessage);
    bool
    setTextProperty(std::wstring& destination, const ArrayOf& propertyValue);
    void
    enqueueCallback(const ArrayOf& callbackArrayOf, const std::wstring& eventType,
        bool notifyFailure = true, bool notifyCompletion = false);
    bool
    enqueueTimerCallback();
    void
    handleTimerCallbackComplete();
    void
    handleCallbackFailure(const std::wstring& eventType);
    void
    workerLoop(double delaySeconds);
    void
    workerLoopFixedRate();
    void
    workerLoopSerial();
    void
    handleFixedRateConflict();
    bool
    waitForStopOrTimeout(double seconds);
    void
    waitForTimerCallbacksToComplete();
    void
    finishRun(bool runStopCallback);
    void
    joinWorker();
    ArrayOf
    makeEvent(const std::wstring& eventType);
    ArrayOf
    callbackToInvokeArray(const ArrayOf& callbackArrayOf, const std::wstring& eventType);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
