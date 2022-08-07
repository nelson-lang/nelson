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
#include "Exception.hpp"
#include "Interface.hpp"
#include "EvaluateInterface.hpp"
#include "FunctionDef.hpp"
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
    display(Interface* io);
    void
    displayOnOneLine(Interface* io, size_t index);

    static void
    displayOnOneLineEmpty(Interface* io, size_t index);

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

    EvaluateInterface* evaluateInterface;
    nelson_handle asNelsonHandle;

    bool
    get(const std::wstring& propertyName, ArrayOf& result);

    std::wstring
    getDiary();

    bool
    cancel(size_t timeoutSeconds = 5);

    void
    evaluateFunction(FunctionDef* fptr, int nLhs, const ArrayOfVector& argIn);

    ArrayOfVector
    getResult();

    Exception
    getException();

private:
    size_t ID;
    wstringVector propertiesNames;
    std::wstring functionName;
    bool wasReaded;
    Exception _exception;
    ArrayOfVector _result;
    int _nLhs;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
