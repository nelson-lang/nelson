//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "FutureObject.hpp"
#include "BackgroundPoolObject.hpp"
#include "characters_encoding.hpp"
#include "MException.hpp"
#include "TimeHelpers.hpp"
#include "NelsonConfiguration.hpp"
#include "ParallelEvaluator.hpp"
#include "HandleManager.hpp"
#include "FutureObjectHelpers.hpp"
#include "i18n.hpp"
#include "WaitFutures.hpp"
#include <condition_variable>
#include <mutex>
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static size_t counterIDs = 0;
static std::mutex futureStateConditionMutex;
static std::condition_variable futureStateCondition;
//=============================================================================
bool
FutureObject::isMethod(const std::wstring& methodName) const
{
    for (const auto& name : propertiesNames) {
        if (name == methodName) {
            return true;
        }
    }
    return false;
}
//=============================================================================
FutureObject::FutureObject(const std::wstring& functionName)
{
    counterIDs++;
    this->ID = counterIDs;
    this->evaluateInterface = nullptr;
    this->creationDateTime = getEpoch();
    this->propertiesNames
        = { L"ID", L"Function", L"CreateDateTime", L"StartDateTime", L"FinishDateTime",
              L"RunningDuration", L"State", L"Error", L"Diary", L"Read", L"Predecessors" };
    this->functionName = functionName;
    this->setState(THREAD_STATE::QUEUED);
    this->_wasRead = false;
    this->_exception = NelsonException();
    this->_result.clear();
    this->_predecessors.clear();
    this->_type = "Future";
}
//=============================================================================
void
FutureObject::displayOnOneLineEmpty(Interface* io, size_t index)
{
    std::wstring finishedDateTime = std::wstring(24, L' ');
    std::wstring message = fmt::format(L"   {:<4d}   {:<4d}   {:<10}   {:<15}   {:<30}   {:<30}\n",
        index, -1, L"unavailable", finishedDateTime, L"[]", L"");
    io->outputMessage(message);
}
//=============================================================================
void
FutureObject::displayOnOneLine(Interface* io, size_t index)
{
    if (io) {
        std::wstring finishedDateTime = std::wstring(24, L' ');
        if (this->getEpochEndDateTime() > 0) {
            finishedDateTime = epochToDateString(this->getEpochEndDateTime());
        }
        std::wstring errorString = L"none";
        if (this->state == THREAD_STATE::FINISHED) {
            if (!_exception.message().empty()) {
                errorString = _exception.message();
            }
        }
        std::wstring message = fmt::format(
            L"   {:<4d}   {:<4d}   {:<10}   {:<15}   {:<30}   {:<30}\n", index, this->getID(),
            this->getStateAsString(), finishedDateTime, this->functionName, errorString);
        io->outputMessage(message);
    }
}
//=============================================================================
void
FutureObject::display(Interface* io)
{
#define BLANKS_AT_BOL std::wstring(L"   ")
    if (io) {
        io->outputMessage(BLANKS_AT_BOL + L"ID: " + std::to_wstring(this->getID()) + L"\n");
        io->outputMessage(BLANKS_AT_BOL + L"Function: " + this->functionName + L"\n");
        std::wstring stateString = getStateAsString();
        std::wstring errorString = L"none";
        if (state == THREAD_STATE::FINISHED) {
            stateString = _wasRead ? stateString + L" (read)" : stateString + L" (unread)";
        }
        if (this->state == THREAD_STATE::FINISHED) {
            if (!_exception.message().empty()) {
                errorString = _exception.message();
            }
        }
        io->outputMessage(
            BLANKS_AT_BOL + L"CreateDateTime: " + epochToDateString(creationDateTime) + L"\n");

        std::wstring strStart = L"";
        if (startDateTime > 0) {
            strStart = epochToDateString(startDateTime);
        }
        io->outputMessage(BLANKS_AT_BOL + L"StartDateTime: " + strStart + L"\n");
        io->outputMessage(BLANKS_AT_BOL + L"RunningDuration: "
            + milliSecondsToDHMSMsString(getRunningDuration()) + L"\n");
        io->outputMessage(BLANKS_AT_BOL + L"State: " + stateString + L"\n");
        io->outputMessage(BLANKS_AT_BOL + L"Error: " + errorString + L"\n");
    }
}
//=============================================================================
FutureObject::~FutureObject()
{
    cancel();
    if (evaluateInterface) {
        delete evaluateInterface;
        evaluateInterface = nullptr;
    }
    setState(THREAD_STATE::UNAVAILABLE);
    _result.clear();
    _predecessors.clear();
    creationDateTime = 0;
    startDateTime = 0;
    endDateTime = 0;
    runningDuration = 0;
    asNelsonHandle = 0;
    functionName.clear();
    _wasRead = false;
}
//=============================================================================
size_t
FutureObject::getID() const
{
    return this->ID;
}
//=============================================================================
uint64
FutureObject::getEpochCreateDateTime() const
{
    return creationDateTime;
}
//=============================================================================
uint64
FutureObject::getEpochStartDateTime() const
{
    return startDateTime;
}
//=============================================================================
uint64
FutureObject::getEpochEndDateTime() const
{
    return endDateTime;
}
//=============================================================================
uint64
FutureObject::getRunningDuration() const
{
    switch (this->state) {
    case THREAD_STATE::UNAVAILABLE:
    case THREAD_STATE::FINISHED:
    default: {
        if (startDateTime < endDateTime) {
            return endDateTime - startDateTime;
        }
        return 0;
    } break;
    case THREAD_STATE::RUNNING: {
        uint64 currentTime = getEpoch();
        if (startDateTime < currentTime) {
            return currentTime - startDateTime;
        }
        return 0;
    } break;
    case THREAD_STATE::QUEUED: {
        return 0;
    } break;
    }
    return 0;
}
//=============================================================================
std::wstring
FutureObject::getStateAsString() const
{
    std::wstring result;
    switch (this->state) {
    case THREAD_STATE::FINISHED: {
        result = L"finished";
    } break;
    case THREAD_STATE::RUNNING: {
        result = L"running";
    } break;
    case THREAD_STATE::QUEUED: {
        result = L"queued";
    } break;
    case THREAD_STATE::UNAVAILABLE: {
        result = L"unavailable";
    } break;
    default: {
        result = L"unknown";
    } break;
    }
    return result;
}
//=============================================================================
std::wstring
FutureObject::getDiary() const
{
    if (evaluateInterface) {
        return evaluateInterface->getOutputBuffer();
    }
    return L"";
}
//=============================================================================
bool
FutureObject::get(const std::wstring& propertyName, ArrayOf& result)
{
    if (propertyName == L"ID") {
        result = ArrayOf::doubleScalar((double)this->getID());
        return true;
    }
    if (propertyName == L"Function") {
        result = ArrayOf::characterArray(this->functionName);
        return true;
    }
    if (propertyName == L"CreateDateTime") {
        result = ArrayOf::doubleScalar((double)this->getEpochCreateDateTime());
        return true;
    }
    if (propertyName == L"StartDateTime") {
        result = ArrayOf::doubleScalar((double)this->getEpochStartDateTime());
        return true;
    }
    if (propertyName == L"RunningDuration") {
        result = ArrayOf::doubleScalar((double)this->getRunningDuration());
        return true;
    }
    if (propertyName == L"FinishDateTime") {
        result = ArrayOf::doubleScalar((double)this->getEpochEndDateTime());
        return true;
    }
    if (propertyName == L"State") {
        result = ArrayOf::characterArray(getStateAsString());
        return true;
    }
    if (propertyName == L"Error") {
        switch (this->state) {
        case THREAD_STATE::FINISHED: {
            result = NelsonExceptionToArrayOf(_exception);
        } break;
        case THREAD_STATE::RUNNING:
        case THREAD_STATE::QUEUED:
        default: {
            NelsonException e;
            result = NelsonExceptionToArrayOf(e);
        } break;
        }
        return true;
    }
    if (propertyName == L"Diary") {
        result = ArrayOf::characterArray(getDiary());
        return true;
    }
    if (propertyName == L"Read") {
        result = ArrayOf::logicalScalar(wasRead());
        return true;
    }
    if (propertyName == L"Predecessors") {
        result = FuturesToArrayOf(_predecessors);
        return true;
    }

    return false;
}
//=============================================================================
bool
FutureObject::cancel()
{
    FutureStateGuardWrite guard(stateMutex);
    if (this->state == THREAD_STATE::FINISHED || this->state == THREAD_STATE::UNAVAILABLE) {
        return false;
    }
    NelsonConfiguration::getInstance()->setInterruptPending(true, this->getID());
    _exception = NelsonException(
        Nelson::resolveMessageTemplate(
            L"Nelson:parallel:executionFutureCancelled", L"Execution of the future was cancelled."),
        L"parallel:fevalqueue:ExecutionCancelled");
    if (this->state == THREAD_STATE::QUEUED) {
        this->endDateTime = getEpoch();
        this->setState(THREAD_STATE::FINISHED);
    }
    return true;
}
//=============================================================================
void
FutureObject::setState(THREAD_STATE newState)
{
    this->state = newState;
    notifyStateChanged();
}
//=============================================================================
void
FutureObject::notifyStateChanged()
{
    futureStateCondition.notify_all();
}
//=============================================================================
void
FutureObject::waitForAnyStateChange(std::chrono::milliseconds timeout)
{
    std::unique_lock<std::mutex> lock(futureStateConditionMutex);
    futureStateCondition.wait_for(lock, timeout);
}
//=============================================================================
void
FutureObject::evaluateFunction(
    FunctionDef* fptr, int nLhs, const ArrayOfVector& argIn, bool changeState)
{
    {
        FutureStateGuardWrite guard(stateMutex);
        _result = ArrayOfVector();
        this->_nLhs = nLhs;
        if (this->state == THREAD_STATE::FINISHED
            || (NelsonConfiguration::getInstance()->getInterruptPending(ID))) {
            setState(THREAD_STATE::FINISHED);
            endDateTime = (uint64)getEpoch();
            return;
        }
        try {
            evaluateInterface = new EvaluateInterface();
        } catch (std::bad_alloc&) {
            setState(THREAD_STATE::FINISHED);
            _exception
                = std::wstring(L"Memory allocation error... You may have run out of memory!");
            endDateTime = (uint64)getEpoch();
            return;
        }
        setState(THREAD_STATE::RUNNING);
    }

    std::shared_ptr<Evaluator> evaluator;
    try {
        evaluator = ParallelEvaluator::create(evaluateInterface, ID);
    } catch (NelsonException& e) {
        FutureStateGuardWrite guard(stateMutex);
        _exception = e;
        endDateTime = (uint64)getEpoch();
        if (changeState) {
            setState(THREAD_STATE::FINISHED);
        }
        return;
    } catch (const std::exception& e) {
        FutureStateGuardWrite guard(stateMutex);
        _exception = NelsonException(e.what());
        endDateTime = (uint64)getEpoch();
        if (changeState) {
            setState(THREAD_STATE::FINISHED);
        }
        return;
    } catch (...) {
        FutureStateGuardWrite guard(stateMutex);
        _exception = NelsonException("Unhandled exception while creating the parallel evaluator.");
        endDateTime = (uint64)getEpoch();
        if (changeState) {
            setState(THREAD_STATE::FINISHED);
        }
        return;
    }
    if (evaluator == nullptr) {
        {
            FutureStateGuardWrite guard(stateMutex);
            if (changeState) {
                setState(THREAD_STATE::FINISHED);
            }
            _exception = NelsonException("Cannot create evaluator.");
            endDateTime = (uint64)getEpoch();
        }
        return;
    }

    if (NelsonConfiguration::getInstance()->getInterruptPending(evaluator->getID())) {
        ParallelEvaluator::destroy(evaluator, true);
        {
            FutureStateGuardWrite guard(stateMutex);
            _exception = NelsonException("Interrupted");
            setState(THREAD_STATE::FINISHED);
            endDateTime = (uint64)getEpoch();
        }
        return;
    }
    {
        FutureStateGuardWrite guard(stateMutex);
        startDateTime = getEpoch();
        endDateTime = (uint64)0;
    }
    // Evaluate into locals without holding stateMutex: user code may re-enter state queries
    // and deadlock. _result and _exception are published in the write-guarded block below.
    ArrayOfVector localResult;
    NelsonException localException;
    bool hasException = false;
    try {
        localResult = fptr->evaluateFunction(evaluator.get(), argIn, nLhs);
    } catch (NelsonException& e) {
        localException = e;
        hasException = true;
    } catch (const std::exception& e) {
        localException = NelsonException(e.what());
        hasException = true;
    } catch (...) {
        localException = NelsonException("Unhandled exception while evaluating the future.");
        hasException = true;
    }
    bool wasInterrupted
        = NelsonConfiguration::getInstance()->getInterruptPending(evaluator->getID());
    ParallelEvaluator::destroy(evaluator, false);
    {
        FutureStateGuardWrite guard(stateMutex);
        if (hasException) {
            _exception = localException;
        } else {
            _result = localResult;
        }
        if (wasInterrupted) {
            _exception = NelsonException(
                Nelson::resolveMessageTemplate(L"Nelson:parallel:executionFutureCancelled",
                    L"Execution of the future was cancelled."),
                L"parallel:fevalqueue:ExecutionCancelled");
        }
        endDateTime = (uint64)getEpoch();
        if (changeState) {
            setState(THREAD_STATE::FINISHED);
        }
    }
}
//=============================================================================
int
FutureObject::getNumberOfLhs()
{
    return this->_nLhs;
}
//=============================================================================
ArrayOfVector
FutureObject::getResult(bool changeReadState)
{
    ArrayOfVector result;
    {
        FutureStateGuardRead guard(stateMutex);
        result = _result;
    }
    if (changeReadState) {
        FutureStateGuardWrite guard(stateMutex);
        _wasRead = true;
    }
    return result;
}
//=============================================================================
void
FutureObject::setResult(const ArrayOfVector& result)
{
    FutureStateGuardWrite guard(stateMutex);
    _result = result;
}
//=============================================================================
NelsonException
FutureObject::getException(bool changeReadState)
{
    NelsonException NelsonException;
    {
        FutureStateGuardRead guard(stateMutex);
        NelsonException = _exception;
    }
    if (changeReadState) {
        FutureStateGuardWrite guard(stateMutex);
        _wasRead = true;
    }
    return NelsonException;
}
//=============================================================================
void
FutureObject::setException(const NelsonException& e)
{
    FutureStateGuardWrite guard(stateMutex);
    _exception = e;
}
//=============================================================================
bool
FutureObject::wasRead()
{
    FutureStateGuardRead guard(stateMutex);
    return _wasRead;
}
//=============================================================================
void
FutureObject::setPredecessors(const std::vector<FutureObject*>& futures)
{
    _predecessors = futures;
}
//=============================================================================
std::vector<FutureObject*>
FutureObject::getPredecessors()
{
    return _predecessors;
}
//=============================================================================
std::string
FutureObject::getType()
{
    return _type;
}
//=============================================================================
void
FutureObject::setType(const std::string& futureType)
{
    _type = futureType;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
