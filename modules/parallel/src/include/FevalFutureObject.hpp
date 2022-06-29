//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <future>
#include <tuple>
#include "nlsParallel_exports.h"
#include "HandleGenericObject.hpp"
#include "Types.hpp"
#include "ArrayOf.hpp"
#include "Interface.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define FEVALFUTURE_CATEGORY_STR L"FevalFuture"
//=============================================================================
enum THREAD_STATE
{
    QUEUED,
    RUNNING,
    FINISHED,
    FAILED,
    UNAVAILABLE,

};
//=============================================================================
class NLSPARALLEL_IMPEXP FevalFutureObject : public HandleGenericObject
{
public:
    FevalFutureObject(const std::wstring& functionName);
    ~FevalFutureObject() override;

    bool
    isMethod(const std::wstring& methodName) override;

    void
    setFuture(std::future<std::tuple<ArrayOfVector, Exception>> f);

    void
    display(Interface* io);
    void
    displayOnOneLine(Interface* io, size_t index);

    static void
    displayOnOneLineEmpty(Interface* io, size_t index);

    std::tuple<ArrayOfVector, Exception>
    get(bool& valid);

    size_t
    getID();

    std::wstring
    getStateAsString();

    uint64
    getEpochCreateDateTime();

    uint64
    getEpochStartDateTime();

    uint64
    getEpochEndDateTime();

    uint64
    getRunningDuration();

    std::atomic<THREAD_STATE> state;
    std::atomic<uint64> creationDateTime;
    std::atomic<uint64> startDateTime;
    std::atomic<uint64> endDateTime;
    std::atomic<uint64> runningDuration;

    nelson_handle asNelsonHandle;

    bool
    get(const std::wstring& propertyName, ArrayOf& result);

private:
    bool
    readContent();
    wstringVector propertiesNames;
    std::future<std::tuple<ArrayOfVector, Exception>> future;
    size_t ID;
    std::wstring functionName;
    bool wasReaded;
    std::tuple<ArrayOfVector, Exception> content;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
