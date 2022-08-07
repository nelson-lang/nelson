//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "FevalFutureObject.hpp"
#include "BackgroundPoolObject.hpp"
#include "characters_encoding.hpp"
#include "MException.hpp"
#include "TimeHelpers.hpp"
#include "NelsonConfiguration.hpp"
#include "ParallelEvaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static size_t counterIDs = 0;
//=============================================================================
bool
FevalFutureObject::isMethod(const std::wstring& methodName)
{
    for (auto name : propertiesNames) {
        if (name == methodName) {
            return true;
        }
    }
    return false;
}
//=============================================================================
FevalFutureObject::FevalFutureObject(const std::wstring& functionName)
    : HandleGenericObject(std::wstring(FEVALFUTURE_CATEGORY_STR), this, false)
{
    counterIDs++;
    this->ID = counterIDs;
    this->evaluateInterface = nullptr;
    this->creationDateTime = getEpoch();
    this->propertiesNames = { L"ID", L"Function", L"CreateDateTime", L"StartDateTime",
        L"FinishDateTime", L"RunningDuration", L"State", L"Error", L"Diary" };
    this->functionName = functionName;
    this->state = THREAD_STATE::QUEUED;
    this->wasReaded = false;
    this->_exception = Exception();
    this->_result.clear();
}
//=============================================================================
void
FevalFutureObject::displayOnOneLineEmpty(Interface* io, size_t index)
{
    std::wstring finishedDateTime = std::wstring(24, L' ');
    std::wstring message = fmt::sprintf(_W("   %-4d   %-4d   %-10s   %-15s   %-30s   %-30s\n"),
        index, -1, L"unavailable", finishedDateTime, L"[]", L"");
    io->outputMessage(message);
}
//=============================================================================
void
FevalFutureObject::displayOnOneLine(Interface* io, size_t index)
{
    if (io) {
        std::wstring finishedDateTime = std::wstring(24, L' ');
        if (this->getEpochEndDateTime() > 0) {
            finishedDateTime = epochToDateString(this->getEpochEndDateTime());
        }
        std::wstring errorString = L"none";
        if (this->state == THREAD_STATE::FINISHED || this->state == THREAD_STATE::FAILED) {
            if (!_exception.getMessage().empty()) {
                errorString = _exception.getMessage();
            }
        }
        std::wstring message = fmt::sprintf(_W("   %-4d   %-4d   %-10s   %-15s   %-30s   %-30s\n"),
            index, this->getID(), this->getStateAsString(), finishedDateTime,
            L"@" + this->functionName, errorString);
        io->outputMessage(message);
    }
}
//=============================================================================
void
FevalFutureObject::display(Interface* io)
{
#define BLANKS_AT_BOL std::wstring(L"   ")
    if (io) {
        io->outputMessage(BLANKS_AT_BOL + L"ID: " + std::to_wstring(this->getID()) + L"\n");
        io->outputMessage(BLANKS_AT_BOL + L"Function: " + L"@" + this->functionName + L"\n");
        std::wstring stateString = getStateAsString();
        std::wstring errorString = L"none";
        if (state == THREAD_STATE::FINISHED) {
            stateString = wasReaded ? stateString + L" (read)" : stateString + L" (unread)";
        }
        if (this->state == THREAD_STATE::FINISHED || this->state == THREAD_STATE::FAILED) {
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
FevalFutureObject::~FevalFutureObject()
{
    cancel();
    if (evaluateInterface) {
        delete evaluateInterface;
        evaluateInterface = nullptr;
    }
    state = THREAD_STATE::UNAVAILABLE;
    creationDateTime = 0;
    startDateTime = 0;
    endDateTime = 0;
    runningDuration = 0;
    asNelsonHandle = 0;
    functionName.clear();
    wasReaded = false;
}
//=============================================================================
size_t
FevalFutureObject::getID()
{
    return this->ID;
}
//=============================================================================
uint64
FevalFutureObject::getEpochCreateDateTime()
{
    return creationDateTime;
}
//=============================================================================
uint64
FevalFutureObject::getEpochStartDateTime()
{
    return startDateTime;
}
//=============================================================================
uint64
FevalFutureObject::getEpochEndDateTime()
{
    return endDateTime;
}
//=============================================================================
uint64
FevalFutureObject::getRunningDuration()
{
    switch (this->state) {
    case THREAD_STATE::UNAVAILABLE:
    case THREAD_STATE::FINISHED:
    case THREAD_STATE::FAILED:
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
FevalFutureObject::getStateAsString()
{
    std::wstring result;
    switch (this->state) {
    case THREAD_STATE::FINISHED: {
        result = L"finished";
    } break;
    case THREAD_STATE::FAILED: {
        result = L"failed";
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
FevalFutureObject::getDiary()
{
    if (evaluateInterface) {
        return evaluateInterface->getOutputBuffer();
    }
    return L"";
}
//=============================================================================
bool
FevalFutureObject::get(const std::wstring& propertyName, ArrayOf& result)
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
        case THREAD_STATE::FINISHED:
        case THREAD_STATE::FAILED: {
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

    return false;
}
//=============================================================================
bool
FevalFutureObject::cancel(size_t timeoutSeconds)
{
    NelsonConfiguration::getInstance()->setInterruptPending(true, this->getID());
    this->endDateTime = getEpoch();
    if (this->state == THREAD_STATE::QUEUED) {
        this->state = THREAD_STATE::FINISHED;
        _result.clear();
        _exception = Exception(_W("Execution of the future was cancelled."),
            L"parallel:fevalqueue:ExecutionCancelled");
        return true;
    }

    std::chrono::nanoseconds begin_time
        = std::chrono::high_resolution_clock::now().time_since_epoch();

    while (this->state == THREAD_STATE::RUNNING) {
        std::this_thread::sleep_for(std::chrono::milliseconds(uint64(1)));
        std::chrono::nanoseconds current_time
            = std::chrono::high_resolution_clock::now().time_since_epoch();
        std::chrono::nanoseconds difftime = (current_time - begin_time);
        if (difftime.count() > int64(timeoutSeconds * 1e9)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
void
FevalFutureObject::evaluateFunction(FunctionDef* fptr, int nLhs, const ArrayOfVector& argIn)
{
    this->_nLhs = nLhs;
    try {
        evaluateInterface = new EvaluateInterface();
    } catch (std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
    state = THREAD_STATE::RUNNING;

    Evaluator* evaluator = createParallelEvaluator(evaluateInterface, ID);
    if (evaluator == nullptr) {
        state = THREAD_STATE::FINISHED;
        _exception = Exception("Cannot create evaluator.");
        endDateTime = (uint64)getEpoch();
        return;
    }

    if (NelsonConfiguration::getInstance()->getInterruptPending(evaluator->getID())) {
        evaluator = deleteParallelEvaluator(evaluator, true);
        _exception = Exception("Interrupted");
        state = THREAD_STATE::FINISHED;
        endDateTime = (uint64)getEpoch();
        return;
    }
    startDateTime = getEpoch();
    endDateTime = (uint64)0;
    try {
        _result = fptr->evaluateFunction(evaluator, argIn, nLhs);
        state = THREAD_STATE::FINISHED;
    } catch (Exception& e) {
        _exception = e;
        state = THREAD_STATE::FAILED;
    }
    if (NelsonConfiguration::getInstance()->getInterruptPending(evaluator->getID())) {
        state = THREAD_STATE::FINISHED;
        _exception = Exception(_W("Execution of the future was cancelled."),
            L"parallel:fevalqueue:ExecutionCancelled");
    }
    evaluator = deleteParallelEvaluator(evaluator, true);
    endDateTime = (uint64)getEpoch();
}
//=============================================================================
ArrayOfVector
FevalFutureObject::getResult()
{
    wasReaded = true;
    return _result;
}
//=============================================================================
Exception
FevalFutureObject::getException()
{
    wasReaded = true;
    return _exception;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
