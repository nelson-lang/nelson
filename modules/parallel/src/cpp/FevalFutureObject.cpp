//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FevalFutureObject.hpp"
#include "BackgroundPoolObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
FevalFutureObject::FevalFutureObject(std::future<std::tuple<ArrayOfVector, Exception>> f,
    const std::wstring& functionName, size_t ID)
    : HandleGenericObject(std::wstring(FEVALFUTURE_CATEGORY_STR), this, false)
{
    state = THREAD_STATE::UNAVAILABLE;
    propertiesNames = { L"ID", L"Function", L"Error" };
    this->future = std::move(f);
    this->functionName = functionName;
    this->ID = ID;
    wasReaded = false;
    content = std::make_tuple<ArrayOfVector, Exception>(ArrayOfVector(), Exception());
}
//=============================================================================
void
FevalFutureObject::display(Interface* io)
{
#define BLANKS_AT_BOL std::wstring(L"   ")
    checkState();
    if (io) {
        io->outputMessage(BLANKS_AT_BOL + L"ID: " + std::to_wstring(this->ID) + L"\n");
        io->outputMessage(BLANKS_AT_BOL + L"Function: " + L"@" + this->functionName + L"\n");
        std::wstring stateString;
        std::wstring errorString = L"none";

        switch (state) {
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
FevalFutureObject::checkState()
{
    switch (state) {
    case THREAD_STATE::UNAVAILABLE: {
        if (BackgroundPoolObject::getInstance()->getTasksRunning()
            > BackgroundPoolObject::getInstance()->getNumberOfThreads()) {
            state = THREAD_STATE::QUEUED;
        } else {
            state = THREAD_STATE::RUNNING;
        }
    } break;
    case THREAD_STATE::QUEUED: {
        /*
        std::future_status status = future.wait_for(std::chrono::seconds(0));
        if (status == std::future_status::timeout) {
            state = THREAD_STATE::QUEUED;
        } else if (status == std::future_status::ready) {
            state = THREAD_STATE::FINISHED;
        } else {
            state = THREAD_STATE::RUNNING;
        }
        */
    } break;
    case THREAD_STATE::RUNNING: {
        bool isReady = future.wait_for(std::chrono::seconds(0)) == std::future_status::ready;
        if (isReady) {
            if (future.valid()) {
                state = THREAD_STATE::FINISHED;
            }
        }
    } break;
    case THREAD_STATE::FAILED: {
    } break;
    case THREAD_STATE::FINISHED: {
    } break;
    default: {
    } break;
    }
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
FevalFutureObject::getState()
{
    checkState();
    return state;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
