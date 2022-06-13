//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
#include <ctime>
#include <boost/date_time/posix_time/posix_time_io.hpp>
#include <boost/chrono/chrono.hpp>
#include "FevalFutureObject.hpp"
#include "BackgroundPoolObject.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static size_t _ID = 0;
//=============================================================================
static uint64
getEpoch()
{
    return (uint64)std::time(nullptr);
}
//=============================================================================
static std::wstring
epochToDateString(uint64 epoch)
{
    std::time_t result = (std::time_t)epoch;
    return utf8_to_wstring(std::asctime(std::localtime(&result)));
}
//=============================================================================
static std::wstring
millisecondsToDuration(uint64 _ms)
{
    boost::chrono::milliseconds ms(_ms);
    auto secs = boost::chrono::duration_cast<boost::chrono::seconds>(ms);
    ms -= boost::chrono::duration_cast<boost::chrono::milliseconds>(secs);
    auto mins = boost::chrono::duration_cast<boost::chrono::minutes>(secs);
    secs -= boost::chrono::duration_cast<boost::chrono::seconds>(mins);
    auto hour = boost::chrono::duration_cast<boost::chrono::hours>(mins);
    mins -= boost::chrono::duration_cast<boost::chrono::minutes>(hour);

    std::stringstream ss;
    ss << hour.count() << " Hours : " << mins.count() << " Minutes : " << secs.count()
       << " Seconds : " << ms.count() << " Milliseconds";
    return utf8_to_wstring(ss.str());
}
//=============================================================================
FevalFutureObject::FevalFutureObject(const std::wstring& functionName)
    : HandleGenericObject(std::wstring(FEVALFUTURE_CATEGORY_STR), this, false)
{
    creationDateTime = getEpoch();
    propertiesNames
        = { L"ID", L"Function", L"CreateDateTime", L"StartDateTime", L"RunningDuration", L"Error" };
    this->functionName = functionName;
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
FevalFutureObject::display(Interface* io)
{
#define BLANKS_AT_BOL std::wstring(L"   ")
    if (io) {
        io->outputMessage(BLANKS_AT_BOL + L"ID: " + std::to_wstring(this->ID) + L"\n");
        io->outputMessage(BLANKS_AT_BOL + L"Function: " + L"@" + this->functionName + L"\n");
        std::wstring stateString;
        std::wstring errorString = L"none";

        switch (this->state) {
        case THREAD_STATE::FAILED: {
            stateString = L"failed";
        } break;
        case THREAD_STATE::RUNNING: {
            stateString = L"running";
        } break;
        case THREAD_STATE::FINISHED: {
            stateString = wasReaded ? L"finished (read)" : L"finished (unread)";
            read();
            Exception e = std::get<1>(content);
            errorString = e.getMessage();
        } break;
        case THREAD_STATE::QUEUED: {
            stateString = L"queued";
        } break;
        case THREAD_STATE::UNAVAILABLE: {
            stateString = L"unavailable";
        } break;
        }
        io->outputMessage(
            BLANKS_AT_BOL + L"CreateDateTime: " + epochToDateString(creationDateTime));

        std::wstring strStart = L"\n";
        if (startDateTime > 0) {
            strStart = epochToDateString(startDateTime);
        }
        io->outputMessage(BLANKS_AT_BOL + L"StartDateTime: " + strStart);
        io->outputMessage(BLANKS_AT_BOL + L"RunningDuration: "
            + millisecondsToDuration(getRunningDuration()) + L"\n");
        io->outputMessage(BLANKS_AT_BOL + L"State: " + stateString + L"\n");
        io->outputMessage(BLANKS_AT_BOL + L"Error: " + errorString + L"\n");
    }
}
//=============================================================================
FevalFutureObject::~FevalFutureObject() { }
//=============================================================================
std::tuple<ArrayOfVector, Exception>
FevalFutureObject::get(bool& valid)
{
    valid = false;
    if (state == THREAD_STATE::FINISHED) {
        valid = true;
        if (!wasReaded) {
            wasReaded = true;
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
void
FevalFutureObject::read()
{
    if (future.valid()) {
        content = future.get();
    }
}
//=============================================================================
THREAD_STATE
FevalFutureObject::getState() { return state; }
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
    if (startDateTime < endDateTime) {
        return endDateTime - startDateTime;
    }
    return 0;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
