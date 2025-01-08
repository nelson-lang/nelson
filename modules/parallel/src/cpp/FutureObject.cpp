//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
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
#include "PredefinedErrorMessages.hpp"
#include "WaitFutures.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static size_t counterIDs = 0;
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
    this->state = THREAD_STATE::QUEUED;
    this->_wasRead = false;
    this->_exception = Exception();
    this->_result.clear();
    this->_predecessors.clear();
    this->_type = "Future";
}
//=============================================================================
void
FutureObject::displayOnOneLineEmpty(Interface* io, size_t index)
{
    std::wstring finishedDateTime = std::wstring(24, L' ');
    std::wstring message = fmt::sprintf(_W("   %-4d   %-4d   %-10s   %-15s   %-30s   %-30s\n"),
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
            if (!_exception.getMessage().empty()) {
                errorString = _exception.getMessage();
            }
        }
        std::wstring message = fmt::sprintf(_W("   %-4d   %-4d   %-10s   %-15s   %-30s   %-30s\n"),
            index, this->getID(), this->getStateAsString(), finishedDateTime, this->functionName,
            errorString);
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
            if (!_exception.getMessage().empty()) {
                errorString = _exception.getMessage();
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
    state = THREAD_STATE::UNAVAILABLE;
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
        result = ArrayOf::doubleConstructor((double)this->getID());
        return true;
    }
    if (propertyName == L"Function") {
        result = ArrayOf::functionHandleConstructor(this->functionName, L"");
        return true;
    }
    if (propertyName == L"CreateDateTime") {
        result = ArrayOf::doubleConstructor((double)this->getEpochCreateDateTime());
        return true;
    }
    if (propertyName == L"StartDateTime") {
        result = ArrayOf::doubleConstructor((double)this->getEpochStartDateTime());
        return true;
    }
    if (propertyName == L"RunningDuration") {
        result = ArrayOf::doubleConstructor((double)this->getRunningDuration());
        return true;
    }
    if (propertyName == L"FinishDateTime") {
        result = ArrayOf::doubleConstructor((double)this->getEpochEndDateTime());
        return true;
    }
    if (propertyName == L"State") {
        result = ArrayOf::characterArrayConstructor(getStateAsString());
        return true;
    }
    if (propertyName == L"Error") {
        switch (this->state) {
        case THREAD_STATE::FINISHED: {
            result = ExceptionToArrayOf(_exception);
        } break;
        case THREAD_STATE::RUNNING:
        case THREAD_STATE::QUEUED:
        default: {
            Exception e;
            result = ExceptionToArrayOf(e);
        } break;
        }
        return true;
    }
    if (propertyName == L"Diary") {
        result = ArrayOf::characterArrayConstructor(getDiary());
        return true;
    }
    if (propertyName == L"Read") {
        result = ArrayOf::logicalConstructor(wasRead());
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
    FutureStateGuard guard(stateMutex);
    NelsonConfiguration::getInstance()->setInterruptPending(true, this->getID());
    this->endDateTime = getEpoch();
    this->state = THREAD_STATE::FINISHED;
    _exception = Exception(
        _W("Execution of the future was cancelled."), L"parallel:fevalqueue:ExecutionCancelled");
    return true;
}
//=============================================================================
void
FutureObject::evaluateFunction(
    FunctionDef* fptr, int nLhs, const ArrayOfVector& argIn, bool changeState)
{
    FutureStateGuard guard(stateMutex);
    _result = ArrayOfVector();
    this->_nLhs = nLhs;
    if (this->state == THREAD_STATE::FINISHED
        || (NelsonConfiguration::getInstance()->getInterruptPending(ID))) {
        state = THREAD_STATE::FINISHED;
        endDateTime = (uint64)getEpoch();
        return;
    }
    try {
        evaluateInterface = new EvaluateInterface();
    } catch (std::bad_alloc&) {
        state = THREAD_STATE::FINISHED;
        _exception = ERROR_MEMORY_ALLOCATION;
        endDateTime = (uint64)getEpoch();
        return;
    }
    state = THREAD_STATE::RUNNING;

    auto evaluator = ParallelEvaluator::create(evaluateInterface, ID);
    if (evaluator == nullptr) {
        if (changeState) {
            state = THREAD_STATE::FINISHED;
        }
        _exception = Exception("Cannot create evaluator.");
        endDateTime = (uint64)getEpoch();
        return;
    }

    if (NelsonConfiguration::getInstance()->getInterruptPending(evaluator->getID())) {
        ParallelEvaluator::destroy(evaluator, true);
        _exception = Exception("Interrupted");
        state = THREAD_STATE::FINISHED;
        endDateTime = (uint64)getEpoch();
        return;
    }
    startDateTime = getEpoch();
    endDateTime = (uint64)0;
    try {
        _result = fptr->evaluateFunction(evaluator.get(), argIn, nLhs);
    } catch (Exception& e) {
        _exception = e;
    }
    if (changeState) {
        state = THREAD_STATE::FINISHED;
    }
    if (NelsonConfiguration::getInstance()->getInterruptPending(evaluator->getID())) {
        state = THREAD_STATE::FINISHED;
        _exception = Exception(_W("Execution of the future was cancelled."),
            L"parallel:fevalqueue:ExecutionCancelled");
    }
    ParallelEvaluator::destroy(evaluator, false);
    endDateTime = (uint64)getEpoch();
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
    if (changeReadState) {
        _wasRead = true;
    }
    return _result;
}
//=============================================================================
void
FutureObject::setResult(const ArrayOfVector& result)
{
    _result = result;
}
//=============================================================================
Exception
FutureObject::getException(bool changeReadState)
{
    if (changeReadState) {
        _wasRead = true;
    }
    return _exception;
}
//=============================================================================
void
FutureObject::setException(const Exception& e)
{
    _exception = e;
}
//=============================================================================
bool
FutureObject::wasRead()
{
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
