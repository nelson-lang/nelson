//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "TimerObject.hpp"
#include <algorithm>
#include <atomic>
#include <cmath>
#include <functional>
#include <limits>
#include <sstream>
#include "ClassName.hpp"
#include "EventCallback.hpp"
#include "EventQueue.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
#include "Now.hpp"
#include "QueueProcessing.hpp"
#include "StringHelpers.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::atomic<int> timerCounter { 0 };
//=============================================================================
namespace {
    //=============================================================================
    std::mutex retainedTimersMutex;
    std::vector<HandleRef> retainedTimers;
    //=============================================================================
    bool
    isRetainedTimerRef(const HandleRef& ref, const TimerObject* object)
    {
        if (!ref || !ref->isValid()) {
            return false;
        }
        return ref->nativePointer() == object;
    }
    //=============================================================================
} // namespace
//=============================================================================
#ifdef __EMSCRIPTEN__
static void
ensureWasmTimerPumpHook()
{
    // Register the cooperative timer clock into the shared callback drain once,
    // the first time any timer starts.
    static std::atomic<bool> registered { false };
    bool expected = false;
    if (registered.compare_exchange_strong(expected, true)) {
        setPendingTimerPumpHook(&TimerObject::pumpActiveTimers);
    }
}
#endif
//=============================================================================
static bool
isEmptyCallback(const ArrayOf& value)
{
    if (value.isEmpty()) {
        return true;
    }
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        return value.asWideString().empty();
    }
    return false;
}
//=============================================================================
static bool
isCallbackValid(const ArrayOf& value)
{
    if (value.isEmpty()) {
        return true;
    }
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        return true;
    }
    if (value.isFunctionHandle()) {
        return true;
    }
    if (value.isCell() && value.elementCount() >= 1) {
        const auto* elements = static_cast<const ArrayOf*>(value.dataPointer());
        return elements != nullptr && elements[0].isFunctionHandle();
    }
    return false;
}
//=============================================================================
static ArrayOf
emptyText()
{
    return ArrayOf::characterArray("");
}
//=============================================================================
static std::wstring
callbackSummary(const ArrayOf& value)
{
    if (isEmptyCallback(value)) {
        return L"''";
    }
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        return L"'" + value.asWideString() + L"'";
    }
    if (value.isFunctionHandle()) {
        return L"function_handle";
    }
    if (value.isCell()) {
        return L"{" + value.dimensions().toWideString() + L" cell}";
    }
    return L"";
}
//=============================================================================
static void
appendTimerPropertyLine(std::wstring& text, const std::wstring& name, const std::wstring& value)
{
    constexpr size_t width = 16;
    std::wstring padding(width > name.length() ? width - name.length() : 0, L' ');
    text += L"      " + padding + name + L": " + value + L"\n";
}
//=============================================================================
static std::wstring
userDataSummary(const ArrayOf& value)
{
    if (value.isEmpty(true)) {
        return L"[]";
    }
    std::string className;
    ClassName(value, className);
    return L"[" + utf8_to_wstring(className) + L"] - size: " + value.dimensions().toWideString();
}
//=============================================================================
static std::wstring
nextTimerName()
{
    int index = ++timerCounter;
    return L"timer-" + std::to_wstring(index);
}
//=============================================================================
static bool
findCanonicalName(
    const std::wstring& value, const std::vector<std::wstring>& allowed, std::wstring& canonical);
//=============================================================================
TimerObject::TimerObject()
    : HandleGenericObject(NLS_HANDLE_TIMER_CATEGORY_STR, this, false)
    , timerFunc(emptyText())
    , startFunc(emptyText())
    , stopFunc(emptyText())
    , errorFunc(emptyText())
    , userData(ArrayOf::empty())
    , period(1.)
    , startDelay(0.)
    , tasksToExecute(std::numeric_limits<double>::infinity())
    , busyMode(L"drop")
    , executionMode(L"singleShot")
    , name(nextTimerName())
    , objectVisibility(L"on")
    , averagePeriod(std::nan(""))
    , instantPeriod(std::nan(""))
    , tasksExecuted(0)
    , tasksLimitThisRun(1.)
    , timerCallbacksInQueue(0)
    , fixedRatePendingCallbacks(0)
    , running(false)
    , stopRequested(false)
    , fixedRateErrorPending(false)
    , hasLastExecutionClock(false)
    , totalPeriod(0.)
{
    propertiesNames
        = { L"AveragePeriod", L"BusyMode", L"ErrorFcn", L"ExecutionMode", L"InstantPeriod", L"Name",
              L"ObjectVisibility", L"Period", L"Running", L"StartDelay", L"StartFcn", L"StopFcn",
              L"Tag", L"TasksExecuted", L"TasksToExecute", L"TimerFcn", L"Type", L"UserData" };
    methodsNames
        = { L"delete", L"get", L"isvalid", L"set", L"start", L"startat", L"stop", L"wait" };
}
//=============================================================================
TimerObject::~TimerObject()
{
    stop(false);
    joinWorker();
}
//=============================================================================
void
TimerObject::onDelete(Evaluator* eval)
{
    stop(false);
    releaseRetainedReference();
}
//=============================================================================
void
TimerObject::retainUntilDeleted()
{
    nelson_handle handleId = HandleManager::getInstance()->findByPointerValue(this);
    HandleRef ref = HandleManager::getInstance()->getHandleRef(handleId);
    if (!ref || !ref->isValid()) {
        return;
    }
    std::lock_guard<std::mutex> lock(retainedTimersMutex);
    auto found = std::find_if(retainedTimers.begin(), retainedTimers.end(),
        [this](const HandleRef& current) { return isRetainedTimerRef(current, this); });
    if (found == retainedTimers.end()) {
        retainedTimers.push_back(ref);
    }
}
//=============================================================================
void
TimerObject::releaseRetainedReference()
{
    std::lock_guard<std::mutex> lock(retainedTimersMutex);
    retainedTimers.erase(std::remove_if(retainedTimers.begin(), retainedTimers.end(),
                             [this](const HandleRef& ref) {
                                 return !ref || !ref->isValid() || ref->nativePointer() == this;
                             }),
        retainedTimers.end());
}
//=============================================================================
bool
TimerObject::isProperty(const std::wstring& propertyName)
{
    std::wstring canonical;
    return findCanonicalName(propertyName, propertiesNames, canonical);
}
//=============================================================================
bool
TimerObject::isMethod(const std::wstring& methodName)
{
    return std::find(methodsNames.begin(), methodsNames.end(), methodName) != methodsNames.end();
}
//=============================================================================
wstringVector
TimerObject::properties()
{
    return propertiesNames;
}
//=============================================================================
wstringVector
TimerObject::methods()
{
    return methodsNames;
}
//=============================================================================
bool
TimerObject::isWritableProperty(const std::wstring& propertyName)
{
    static const std::vector<std::wstring> writable = { L"BusyMode", L"ErrorFcn", L"ExecutionMode",
        L"Name", L"ObjectVisibility", L"Period", L"StartDelay", L"StartFcn", L"StopFcn", L"Tag",
        L"TasksToExecute", L"TimerFcn", L"UserData" };
    std::wstring canonical;
    return findCanonicalName(propertyName, writable, canonical);
}
//=============================================================================
bool
TimerObject::get(const std::wstring& propertyName, ArrayOf& res)
{
    std::lock_guard<std::mutex> lock(mutex);
    std::wstring canonicalPropertyName;
    if (!findCanonicalName(propertyName, propertiesNames, canonicalPropertyName)) {
        return false;
    }
    if (canonicalPropertyName == L"AveragePeriod") {
        res = ArrayOf::doubleScalar(averagePeriod);
    } else if (canonicalPropertyName == L"BusyMode") {
        res = ArrayOf::characterArray(busyMode);
    } else if (canonicalPropertyName == L"ErrorFcn") {
        res = errorFunc;
    } else if (canonicalPropertyName == L"ExecutionMode") {
        res = ArrayOf::characterArray(executionMode);
    } else if (canonicalPropertyName == L"InstantPeriod") {
        res = ArrayOf::doubleScalar(instantPeriod);
    } else if (canonicalPropertyName == L"Name") {
        res = ArrayOf::characterArray(name);
    } else if (canonicalPropertyName == L"ObjectVisibility") {
        res = ArrayOf::characterArray(objectVisibility);
    } else if (canonicalPropertyName == L"Period") {
        res = ArrayOf::doubleScalar(period);
    } else if (canonicalPropertyName == L"Running") {
        res = ArrayOf::characterArray(running ? "on" : "off");
    } else if (canonicalPropertyName == L"StartDelay") {
        res = ArrayOf::doubleScalar(startDelay);
    } else if (canonicalPropertyName == L"StartFcn") {
        res = startFunc;
    } else if (canonicalPropertyName == L"StopFcn") {
        res = stopFunc;
    } else if (canonicalPropertyName == L"Tag") {
        res = ArrayOf::characterArray(tag);
    } else if (canonicalPropertyName == L"TasksExecuted") {
        res = ArrayOf::doubleScalar(tasksExecuted);
    } else if (canonicalPropertyName == L"TasksToExecute") {
        res = ArrayOf::doubleScalar(tasksToExecute);
    } else if (canonicalPropertyName == L"TimerFcn") {
        res = timerFunc;
    } else if (canonicalPropertyName == L"Type") {
        res = ArrayOf::characterArray(NLS_HANDLE_TIMER_CATEGORY_STR);
    } else if (canonicalPropertyName == L"UserData") {
        res = userData;
    } else {
        return false;
    }
    return true;
}
//=============================================================================
bool
TimerObject::setTextProperty(std::wstring& destination, const ArrayOf& propertyValue)
{
    if (!(propertyValue.isRowVectorCharacterArray() || propertyValue.isScalarStringArray())) {
        return false;
    }
    destination = propertyValue.asWideString();
    return true;
}
//=============================================================================
bool
TimerObject::setCallbackProperty(
    ArrayOf& destination, const ArrayOf& propertyValue, std::wstring& errorMessage)
{
    if (!isCallbackValid(propertyValue)) {
        errorMessage = resolveMessageTemplate(L"Nelson:time:callbackCharacterVectorString",
            L"Callback must be a character vector, string scalar, function handle, cell array, or "
            L"empty.");
        return false;
    }
    destination = propertyValue;
    destination.ensureSingleOwner();
    if (destination.isEmpty()) {
        destination = emptyText();
    }
    return true;
}
//=============================================================================
static bool
findCanonicalName(
    const std::wstring& value, const std::vector<std::wstring>& allowed, std::wstring& canonical)
{
    std::wstring normalizedValue = StringHelpers::to_lower_copy(value);
    std::wstring prefixMatch;
    size_t matchCount = 0;
    for (const std::wstring& candidate : allowed) {
        std::wstring normalizedCandidate = StringHelpers::to_lower_copy(candidate);
        if (normalizedValue == normalizedCandidate) {
            canonical = candidate;
            return true;
        }
        if (normalizedCandidate.rfind(normalizedValue, 0) == 0) {
            prefixMatch = candidate;
            ++matchCount;
        }
    }
    if (matchCount == 1) {
        canonical = prefixMatch;
        return true;
    }
    return false;
}
//=============================================================================
bool
TimerObject::set(
    const std::wstring& propertyName, const ArrayOf& propertyValue, std::wstring& errorMessage)
{
    std::lock_guard<std::mutex> lock(mutex);
    std::wstring canonicalPropertyName;
    if (!findCanonicalName(propertyName, propertiesNames, canonicalPropertyName)
        || !isWritableProperty(canonicalPropertyName)) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:time:cannotSetReadOnly", L"Cannot set a read-only property.");
        return false;
    }
    if (running
        && (canonicalPropertyName == L"BusyMode" || canonicalPropertyName == L"ExecutionMode"
            || canonicalPropertyName == L"Period" || canonicalPropertyName == L"StartDelay")) {
        errorMessage = resolveMessageTemplate(L"Nelson:time:cannotSetPropertyWhile",
            L"Cannot set this property while timer is running.");
        return false;
    }
    if (canonicalPropertyName == L"Period") {
        double value = propertyValue.asDoubleScalar();
        if (!std::isfinite(value) || value <= 0.001) {
            errorMessage = resolveMessageTemplate(
                L"Nelson:time:periodGreaterThan", L"Period must be greater than 0.001.");
            return false;
        }
        period = std::round(value * 1000.) / 1000.;
    } else if (canonicalPropertyName == L"StartDelay") {
        double value = propertyValue.asDoubleScalar();
        if (!std::isfinite(value) || value < 0.) {
            errorMessage = resolveMessageTemplate(L"Nelson:time:startdelayGreaterThanEqual",
                L"StartDelay must be greater than or equal to zero.");
            return false;
        }
        startDelay = std::round(value * 1000.) / 1000.;
    } else if (canonicalPropertyName == L"TasksToExecute") {
        double value = propertyValue.asDoubleScalar();
        if (!std::isinf(value)) {
            if (!std::isfinite(value)
                || value >= static_cast<double>(std::numeric_limits<int>::max())) {
                errorMessage = resolveMessageTemplate(L"Nelson:time:taskstoexecuteLessThanMaximum",
                    L"TasksToExecute must be less than the maximum integer value.");
                return false;
            }
            value = std::floor(value);
        }
        if (value <= 0.) {
            errorMessage = resolveMessageTemplate(L"Nelson:time:taskstoexecuteGreaterThanZero",
                L"TasksToExecute must be greater than zero.");
            return false;
        }
        tasksToExecute = value;
        if (running && executionMode != L"singleShot") {
            tasksLimitThisRun = tasksToExecute;
        }
    } else if (canonicalPropertyName == L"BusyMode") {
        std::wstring value = propertyValue.asWideString();
        if (!findCanonicalName(value, { L"drop", L"error", L"queue" }, busyMode)) {
            errorMessage = formatI18nMessage(
                Nelson::resolveMessageTemplate(L"Nelson:time:invalid", L"Invalid %s."),
                L"BusyMode value");
            return false;
        }
    } else if (canonicalPropertyName == L"ExecutionMode") {
        std::wstring value = propertyValue.asWideString();
        if (!findCanonicalName(value,
                { L"singleShot", L"fixedRate", L"fixedDelay", L"fixedSpacing" }, executionMode)) {
            errorMessage = formatI18nMessage(
                Nelson::resolveMessageTemplate(L"Nelson:time:invalid", L"Invalid %s."),
                L"ExecutionMode value");
            return false;
        }
    } else if (canonicalPropertyName == L"Name" && !setTextProperty(name, propertyValue)) {
        errorMessage = formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:validators:mustBeTextScalar",
                L"%s must be a character vector or string scalar."),
            L"Name");
        return false;
    } else if (canonicalPropertyName == L"Tag" && !setTextProperty(tag, propertyValue)) {
        errorMessage = formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:validators:mustBeTextScalar",
                L"%s must be a character vector or string scalar."),
            L"Tag");
        return false;
    } else if (canonicalPropertyName == L"ObjectVisibility") {
        std::wstring value = propertyValue.asWideString();
        if (!findCanonicalName(value, { L"on", L"off" }, objectVisibility)) {
            errorMessage = resolveMessageTemplate(
                L"Nelson:time:objectvisibilityOff", L"ObjectVisibility must be 'on' or 'off'.");
            return false;
        }
    } else if (canonicalPropertyName == L"UserData") {
        userData = propertyValue;
        userData.ensureSingleOwner();
    } else if (canonicalPropertyName == L"TimerFcn") {
        return setCallbackProperty(timerFunc, propertyValue, errorMessage);
    } else if (canonicalPropertyName == L"StartFcn") {
        return setCallbackProperty(startFunc, propertyValue, errorMessage);
    } else if (canonicalPropertyName == L"StopFcn") {
        return setCallbackProperty(stopFunc, propertyValue, errorMessage);
    } else if (canonicalPropertyName == L"ErrorFcn") {
        return setCallbackProperty(errorFunc, propertyValue, errorMessage);
    }
    return true;
}
//=============================================================================
ArrayOf
TimerObject::makeEvent(const std::wstring& eventType)
{
    ArrayOfVector dataValues;
    dataValues << ArrayOf::doubleScalar(Now());
    ArrayOf data = ArrayOf::structScalar(stringVector { "time" }, dataValues);
    ArrayOfVector eventValues;
    eventValues << ArrayOf::characterArray(eventType);
    eventValues << data;
    return ArrayOf::structScalar(stringVector { "Type", "Data" }, eventValues);
}
//=============================================================================
ArrayOf
TimerObject::callbackToInvokeArray(const ArrayOf& callbackArrayOf, const std::wstring& eventType)
{
    size_t extraCount = 0;
    const ArrayOf* inputElements = nullptr;
    ArrayOf functionHandle = callbackArrayOf;
    if (callbackArrayOf.isCell()) {
        extraCount = callbackArrayOf.elementCount() - 1;
        inputElements = static_cast<const ArrayOf*>(callbackArrayOf.dataPointer());
        functionHandle = inputElements[0];
    }
    size_t nbElements = 3 + extraCount;
    auto* elements = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements));
    elements[0] = functionHandle;
    nelson_handle handleId = HandleManager::getInstance()->findByPointerValue(this);
    elements[1] = handleId != 0 ? ArrayOf::handle(handleId) : ArrayOf::handle(this);
    elements[2] = makeEvent(eventType);
    for (size_t k = 0; k < extraCount; ++k) {
        elements[3 + k] = inputElements[1 + k];
    }
    return ArrayOf(NLS_CELL_ARRAY, Dimensions(1, nbElements), elements);
}
//=============================================================================
void
TimerObject::handleCallbackFailure(const std::wstring& eventType)
{
    if (eventType == L"ErrorFcn" || eventType == L"StopFcn") {
        return;
    }
    bool shouldRunStop = false;
    {
        std::lock_guard<std::mutex> lock(mutex);
        shouldRunStop = running;
        running = false;
        stopRequested = true;
        fixedRatePendingCallbacks = 0;
        fixedRateErrorPending = false;
    }
    condition.notify_all();
    enqueueCallback(errorFunc, L"ErrorFcn", false);
    if (shouldRunStop) {
        enqueueCallback(stopFunc, L"StopFcn", false);
    }
}
//=============================================================================
void
TimerObject::enqueueCallback(const ArrayOf& callbackArrayOf, const std::wstring& eventType,
    bool notifyFailure, bool notifyCompletion)
{
    if (isEmptyCallback(callbackArrayOf)) {
        return;
    }
    nelson_handle handleId = HandleManager::getInstance()->findByPointerValue(this);
    std::function<void()> failureHandler;
    if (notifyFailure) {
        failureHandler = [handleId, eventType]() {
            HandleRef ref = HandleManager::getInstance()->getHandleRef(handleId);
            if (!ref || !ref->isValid()) {
                return;
            }
            auto* object = dynamic_cast<TimerObject*>(ref->object());
            if (object != nullptr) {
                object->handleCallbackFailure(eventType);
            }
        };
    }
    std::function<void(bool)> completionHandler;
    if (notifyCompletion) {
        completionHandler = [handleId](bool) {
            HandleRef ref = HandleManager::getInstance()->getHandleRef(handleId);
            if (!ref || !ref->isValid()) {
                return;
            }
            auto* object = dynamic_cast<TimerObject*>(ref->object());
            if (object != nullptr) {
                object->handleTimerCallbackComplete();
            }
        };
    }
    if (callbackArrayOf.isRowVectorCharacterArray() || callbackArrayOf.isScalarStringArray()) {
        EventQueue::getInstance()->add(
            EventCallback(callbackArrayOf, failureHandler, completionHandler));
    } else {
        EventQueue::getInstance()->add(EventCallback(
            callbackToInvokeArray(callbackArrayOf, eventType), failureHandler, completionHandler));
    }
}
//=============================================================================
bool
TimerObject::enqueueTimerCallback()
{
    ArrayOf callbackArrayOf;
    {
        std::lock_guard<std::mutex> lock(mutex);
        if (!running || stopRequested || tasksExecuted >= tasksLimitThisRun
            || isEmptyCallback(timerFunc)) {
            return false;
        }
        callbackArrayOf = timerFunc;
        ++timerCallbacksInQueue;
        auto current = std::chrono::steady_clock::now();
        if (hasLastExecutionClock) {
            std::chrono::duration<double> elapsed = current - lastExecutionClock;
            instantPeriod = elapsed.count();
            totalPeriod += instantPeriod;
            averagePeriod = totalPeriod / tasksExecuted;
        }
        lastExecutionClock = current;
        hasLastExecutionClock = true;
        ++tasksExecuted;
    }
    condition.notify_all();
    enqueueCallback(callbackArrayOf, L"TimerFcn", true, true);
    return true;
}
//=============================================================================
void
TimerObject::handleTimerCallbackComplete()
{
    int callbacksToQueue = 0;
    bool queueError = false;
    {
        std::lock_guard<std::mutex> lock(mutex);
        if (timerCallbacksInQueue > 0) {
            --timerCallbacksInQueue;
        }
        if (fixedRateErrorPending && timerCallbacksInQueue == 0) {
            fixedRateErrorPending = false;
            fixedRatePendingCallbacks = 0;
            running = false;
            stopRequested = true;
            queueError = true;
        } else if (running && !stopRequested && executionMode == L"fixedRate"
            && timerCallbacksInQueue == 0 && fixedRatePendingCallbacks > 0) {
            double remaining = tasksLimitThisRun - tasksExecuted;
            if (std::isinf(remaining)) {
                callbacksToQueue = fixedRatePendingCallbacks;
            } else {
                callbacksToQueue = std::min(fixedRatePendingCallbacks,
                    static_cast<int>(std::max(0., std::floor(remaining))));
            }
            fixedRatePendingCallbacks -= callbacksToQueue;
        }
    }
    condition.notify_all();
    if (queueError) {
        enqueueCallback(errorFunc, L"ErrorFcn", false);
        enqueueCallback(stopFunc, L"StopFcn", false);
        return;
    }
    for (int k = 0; k < callbacksToQueue; ++k) {
        if (!enqueueTimerCallback()) {
            break;
        }
    }
}
//=============================================================================
bool
TimerObject::start(double delaySeconds, std::wstring& errorMessage)
{
    joinWorker();
    {
        std::lock_guard<std::mutex> lock(mutex);
        if (running) {
            errorMessage = Nelson::resolveMessageTemplate(
                L"Nelson:time:timerAlreadyRunning", L"Timer is already running.");
            return false;
        }
        if (isEmptyCallback(timerFunc)) {
            errorMessage = resolveMessageTemplate(L"Nelson:time:timerfcnDefinedBeforeStarting",
                L"TimerFcn must be defined before starting a timer.");
            return false;
        }
        tasksExecuted = 0;
        averagePeriod = std::nan("");
        instantPeriod = std::nan("");
        hasLastExecutionClock = false;
        totalPeriod = 0.;
        tasksLimitThisRun = executionMode == L"singleShot" ? 1 : tasksToExecute;
        timerCallbacksInQueue = 0;
        fixedRatePendingCallbacks = 0;
        fixedRateErrorPending = false;
        stopRequested = false;
        running = true;
    }
    enqueueCallback(startFunc, L"StartFcn");
#ifdef __EMSCRIPTEN__
    // Single-threaded WebAssembly: no worker thread. Arm the cooperative clock
    // and let pumpActiveTimers() (driven by the shared drain) fire the ticks.
    {
        std::lock_guard<std::mutex> lock(mutex);
        wasmNextDue = std::chrono::steady_clock::now()
            + std::chrono::duration_cast<std::chrono::steady_clock::duration>(
                std::chrono::duration<double>(delaySeconds));
    }
    ensureWasmTimerPumpHook();
#else
    worker = std::thread(&TimerObject::workerLoop, this, delaySeconds);
#endif
    return true;
}
//=============================================================================
bool
TimerObject::waitForStopOrTimeout(double seconds)
{
    std::unique_lock<std::mutex> lock(mutex);
    if (seconds <= 0.) {
        return stopRequested;
    }
    auto duration = std::chrono::duration<double>(seconds);
    condition.wait_for(lock, duration, [this]() { return stopRequested; });
    return stopRequested;
}
//=============================================================================
void
TimerObject::workerLoop(double delaySeconds)
{
    if (waitForStopOrTimeout(delaySeconds)) {
        finishRun(false);
        return;
    }
    std::wstring mode;
    {
        std::lock_guard<std::mutex> lock(mutex);
        mode = executionMode;
    }
    if (mode == L"fixedRate") {
        workerLoopFixedRate();
    } else {
        workerLoopSerial();
    }
}
//=============================================================================
void
TimerObject::workerLoopSerial()
{
    while (enqueueTimerCallback()) {
        waitForTimerCallbacksToComplete();
        {
            std::lock_guard<std::mutex> lock(mutex);
            if (stopRequested || tasksExecuted >= tasksLimitThisRun) {
                break;
            }
        }
        if (waitForStopOrTimeout(period)) {
            break;
        }
    }
    finishRun(true);
}
//=============================================================================
void
TimerObject::workerLoopFixedRate()
{
    enqueueTimerCallback();
    while (true) {
        {
            std::lock_guard<std::mutex> lock(mutex);
            if (stopRequested || tasksExecuted >= tasksLimitThisRun) {
                break;
            }
        }
        if (waitForStopOrTimeout(period)) {
            break;
        }
        bool hasTimerCallbackInQueue = false;
        {
            std::lock_guard<std::mutex> lock(mutex);
            hasTimerCallbackInQueue = timerCallbacksInQueue > 0;
        }
        if (hasTimerCallbackInQueue) {
            handleFixedRateConflict();
        } else {
            enqueueTimerCallback();
        }
    }
    waitForTimerCallbacksToComplete();
    finishRun(true);
}
//=============================================================================
void
TimerObject::handleFixedRateConflict()
{
    bool wakeWorker = false;
    {
        std::lock_guard<std::mutex> lock(mutex);
        if (!running || stopRequested || tasksExecuted >= tasksLimitThisRun) {
            return;
        }
        double remaining = tasksLimitThisRun - tasksExecuted - fixedRatePendingCallbacks;
        if (remaining <= 0) {
            return;
        }
        if (busyMode == L"drop") {
            fixedRatePendingCallbacks = std::max(fixedRatePendingCallbacks, 1);
        } else if (busyMode == L"queue") {
            ++fixedRatePendingCallbacks;
        } else if (busyMode == L"error") {
            fixedRatePendingCallbacks = 0;
            fixedRateErrorPending = true;
            stopRequested = true;
            wakeWorker = true;
        }
    }
    if (wakeWorker) {
        condition.notify_all();
    }
}
//=============================================================================
void
TimerObject::waitForTimerCallbacksToComplete()
{
    std::unique_lock<std::mutex> lock(mutex);
    condition.wait(lock, [this]() {
        return timerCallbacksInQueue == 0 || (stopRequested && !fixedRateErrorPending);
    });
}
//=============================================================================
void
TimerObject::finishRun(bool runStopCallback)
{
    bool shouldRunStop = false;
    {
        std::lock_guard<std::mutex> lock(mutex);
        shouldRunStop = running && runStopCallback;
        running = false;
        stopRequested = true;
    }
    condition.notify_all();
    if (shouldRunStop) {
        enqueueCallback(stopFunc, L"StopFcn");
    }
}
//=============================================================================
#ifdef __EMSCRIPTEN__
void
TimerObject::pumpActiveTimers()
{
    // Snapshot the live timers (retained handles keep them alive), then service
    // each without holding the registry lock. Single-threaded, so no timer can
    // be deleted while we iterate the snapshot.
    std::vector<TimerObject*> active;
    {
        std::lock_guard<std::mutex> lock(retainedTimersMutex);
        active.reserve(retainedTimers.size());
        for (const HandleRef& ref : retainedTimers) {
            if (ref && ref->isValid()) {
                auto* object = dynamic_cast<TimerObject*>(ref->object());
                if (object != nullptr) {
                    active.push_back(object);
                }
            }
        }
    }
    const auto now = std::chrono::steady_clock::now();
    for (TimerObject* object : active) {
        object->wasmServiceTick(now);
    }
}
//=============================================================================
void
TimerObject::wasmServiceTick(const std::chrono::steady_clock::time_point& now)
{
    bool isRunning = false;
    bool stopped = false;
    bool fixedRate = false;
    int inQueue = 0;
    double execCount = 0.;
    double limit = 0.;
    double periodSeconds = 0.;
    std::chrono::steady_clock::time_point due;
    {
        std::lock_guard<std::mutex> lock(mutex);
        isRunning = running;
        stopped = stopRequested;
        fixedRate = (executionMode == L"fixedRate");
        inQueue = timerCallbacksInQueue;
        execCount = static_cast<double>(tasksExecuted);
        limit = tasksLimitThisRun;
        periodSeconds = period;
        due = wasmNextDue;
    }
    if (!isRunning || stopped) {
        return;
    }
    if (execCount >= limit && inQueue == 0) {
        // Every scheduled task has run and drained: end the run (fires StopFcn).
        finishRun(true);
        return;
    }
    if (now < due) {
        return;
    }
    const auto periodDuration = std::chrono::duration_cast<std::chrono::steady_clock::duration>(
        std::chrono::duration<double>(periodSeconds));
    if (fixedRate) {
        if (inQueue > 0) {
            handleFixedRateConflict();
        } else {
            enqueueTimerCallback();
        }
        std::lock_guard<std::mutex> lock(mutex);
        wasmNextDue += periodDuration;
        if (wasmNextDue < now) {
            wasmNextDue = now + periodDuration;
        }
    } else {
        // Serial / fixedSpacing / singleShot: only one callback in flight; the
        // next tick is scheduled a full period after this one starts.
        if (inQueue > 0) {
            return;
        }
        if (enqueueTimerCallback()) {
            std::lock_guard<std::mutex> lock(mutex);
            wasmNextDue = now + periodDuration;
        } else {
            finishRun(true);
        }
    }
}
#endif
//=============================================================================
void
TimerObject::stop(bool runStopCallback)
{
    bool shouldRunStop = false;
    {
        std::lock_guard<std::mutex> lock(mutex);
        shouldRunStop = running && runStopCallback;
        stopRequested = true;
        running = false;
    }
    condition.notify_all();
    joinWorker();
    if (shouldRunStop) {
        enqueueCallback(stopFunc, L"StopFcn");
    }
}
//=============================================================================
void
TimerObject::joinWorker()
{
    if (worker.joinable()) {
        if (std::this_thread::get_id() == worker.get_id()) {
            worker.detach();
        } else {
            worker.join();
        }
    }
}
//=============================================================================
void
TimerObject::wait(Evaluator* eval)
{
    bool keepWaiting = true;
    while (keepWaiting) {
        processPendingCallbacksAndTimers(eval);
        std::unique_lock<std::mutex> lock(mutex);
        keepWaiting = running;
        if (keepWaiting) {
            condition.wait_for(lock, std::chrono::milliseconds(1));
        }
    }
    joinWorker();
    for (int k = 0; k < 8; ++k) {
        if (EventQueue::getInstance()->isEmpty()) {
            break;
        }
        processPendingCallbacksAndTimers(eval);
    }
}
//=============================================================================
ArrayOf
TimerObject::toStruct()
{
    ArrayOfVector values;
    for (const std::wstring& propertyName : propertiesNames) {
        ArrayOf value;
        get(propertyName, value);
        values << value;
    }
    return ArrayOf::structScalar(propertiesNames, values);
}
//=============================================================================
std::wstring
TimerObject::getObjectVisibility()
{
    std::lock_guard<std::mutex> lock(mutex);
    return objectVisibility;
}
//=============================================================================
bool
TimerObject::disp(Interface* io)
{
    if (!io) {
        return false;
    }
    std::lock_guard<std::mutex> lock(mutex);
    io->outputMessage(L"\n");
    io->outputMessage(L"   Timer Object: " + name + L"\n\n");
    io->outputMessage(L"   Timer Settings\n");
    io->outputMessage(L"      ExecutionMode: " + executionMode + L"\n");
    io->outputMessage(L"             Period: " + std::to_wstring(period) + L"\n");
    io->outputMessage(L"           BusyMode: " + busyMode + L"\n");
    io->outputMessage(L"            Running: " + std::wstring(running ? L"on" : L"off") + L"\n");
    io->outputMessage(L"   Callbacks\n");
    io->outputMessage(L"           TimerFcn: " + callbackSummary(timerFunc) + L"\n");
    io->outputMessage(L"           ErrorFcn: " + callbackSummary(errorFunc) + L"\n");
    io->outputMessage(L"           StartFcn: " + callbackSummary(startFunc) + L"\n");
    io->outputMessage(L"            StopFcn: " + callbackSummary(stopFunc) + L"\n");
    std::wstring expanded;
    appendTimerPropertyLine(expanded, L"AveragePeriod", std::to_wstring(averagePeriod));
    appendTimerPropertyLine(expanded, L"BusyMode", busyMode);
    appendTimerPropertyLine(expanded, L"ErrorFcn", callbackSummary(errorFunc));
    appendTimerPropertyLine(expanded, L"ExecutionMode", executionMode);
    appendTimerPropertyLine(expanded, L"InstantPeriod", std::to_wstring(instantPeriod));
    appendTimerPropertyLine(expanded, L"Name", L"'" + name + L"'");
    appendTimerPropertyLine(expanded, L"ObjectVisibility", objectVisibility);
    appendTimerPropertyLine(expanded, L"Period", std::to_wstring(period));
    appendTimerPropertyLine(expanded, L"Running", running ? L"on" : L"off");
    appendTimerPropertyLine(expanded, L"StartDelay", std::to_wstring(startDelay));
    appendTimerPropertyLine(expanded, L"StartFcn", callbackSummary(startFunc));
    appendTimerPropertyLine(expanded, L"StopFcn", callbackSummary(stopFunc));
    appendTimerPropertyLine(expanded, L"Tag", L"'" + tag + L"'");
    appendTimerPropertyLine(expanded, L"TasksExecuted", std::to_wstring(tasksExecuted));
    appendTimerPropertyLine(expanded, L"TasksToExecute", std::to_wstring(tasksToExecute));
    appendTimerPropertyLine(expanded, L"TimerFcn", callbackSummary(timerFunc));
    appendTimerPropertyLine(expanded, L"Type", L"timer");
    appendTimerPropertyLine(expanded, L"UserData", userDataSummary(userData));
    io->outputExpandableText(_W("Show all properties"), expanded, L"");
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
