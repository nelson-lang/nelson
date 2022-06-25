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
//=============================================================================
namespace Nelson {
//=============================================================================
static size_t _ID = 0;
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
    creationDateTime = getEpoch();
    propertiesNames = { L"ID", L"Function", L"CreateDateTime", L"StartDateTime", L"FinishDateTime",
        L"RunningDuration", L"State", L"Error" };
    this->functionName = functionName;
    state = THREAD_STATE::UNAVAILABLE;
    _ID++;
    this->ID = _ID;
    wasReaded = false;
    content = std::make_tuple<ArrayOfVector, Exception>(ArrayOfVector(), Exception());
}
//=============================================================================
void
FevalFutureObject::setFuture(std::future<std::tuple<ArrayOfVector, Exception>> f)
{
    this->future = std::move(f);
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
        std::wstring errorString = L"";
        if (this->state == THREAD_STATE::FAILED) {
            read();
            Exception e = std::get<1>(content);
            errorString = e.getMessage();
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
        io->outputMessage(BLANKS_AT_BOL + L"ID: " + std::to_wstring(this->ID) + L"\n");
        io->outputMessage(BLANKS_AT_BOL + L"Function: " + L"@" + this->functionName + L"\n");
        std::wstring stateString = getStateAsString();
        std::wstring errorString = L"none";
        if (state == THREAD_STATE::FINISHED) {
            stateString = wasReaded ? stateString + L" (read)" : stateString + L" (unread)";
        }
        if (state == THREAD_STATE::FAILED) {
            read();
            Exception e = std::get<1>(content);
            errorString = e.getMessage();
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

    state = THREAD_STATE::UNAVAILABLE;
    creationDateTime = 0;
    startDateTime = 0;
    endDateTime = 0;
    runningDuration = 0;
    asNelsonHandle = 0;
    ID = 0;
    functionName.clear();
    wasReaded = false;
}
//=============================================================================
std::tuple<ArrayOfVector, Exception>
FevalFutureObject::get(bool& valid)
{
    valid = false;
    if (state == THREAD_STATE::FINISHED) {
        valid = true;
        if (!wasReaded) {
            valid = read();
        }
    }
    return content;
}
//=============================================================================
size_t
FevalFutureObject::getID()
{
    return ID;
}
//=============================================================================
bool
FevalFutureObject::read()
{
    if (future.valid()) {
        content = future.get();
        wasReaded = true;
        return true;
    }
    return false;
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

bool
FevalFutureObject::get(const std::wstring& propertyName, ArrayOf& result)
{
    if (propertyName == L"ID") {
        result = ArrayOf::doubleConstructor((double)this->ID);
        return true;
    }
    if (propertyName == L"Function") {
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
            read();
            Exception e = std::get<1>(content);
            result = ExceptionToArrayOf(e);
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

    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
