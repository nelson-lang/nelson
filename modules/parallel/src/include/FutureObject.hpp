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
#include <vector>
#include <atomic>
#include <shared_mutex>
#include <chrono>
#include "nlsParallel_exports.h"
#include "Types.hpp"
#include "ArrayOf.hpp"
#include "NelsonException.hpp"
#include "Interface.hpp"
#include "EvaluateInterface.hpp"
#include "FunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum THREAD_STATE
{
    QUEUED,
    RUNNING,
    FINISHED,
    UNAVAILABLE,
};
//=============================================================================
class NLSPARALLEL_IMPEXP FutureObject
{
public:
    FutureObject(const std::wstring& functionName);
    ~FutureObject();

    std::shared_mutex stateMutex;

    bool
    isMethod(const std::wstring& methodName) const;

    void
    display(Interface* io);
    void
    displayOnOneLine(Interface* io, size_t index);

    static void
    displayOnOneLineEmpty(Interface* io, size_t index);

    size_t
    getID() const;

    std::wstring
    getStateAsString() const;

    uint64
    getEpochCreateDateTime() const;

    uint64
    getEpochStartDateTime() const;

    uint64
    getEpochEndDateTime() const;

    uint64
    getRunningDuration() const;

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
    getDiary() const;

    bool
    cancel();

    void
    setState(THREAD_STATE newState);

    static void
    notifyStateChanged();

    static void
    waitForAnyStateChange(std::chrono::milliseconds timeout);

    void
    evaluateFunction(
        FunctionDef* fptr, int nLhs, const ArrayOfVector& argIn, bool changeState = true);

    ArrayOfVector
    getResult(bool changeReadState = true);

    void
    setResult(const ArrayOfVector& result);

    NelsonException
    getException(bool changeReadState = true);

    void
    setException(const NelsonException& e);

    int
    getNumberOfLhs();

    bool
    wasRead();

    void
    setPredecessors(const std::vector<FutureObject*>& futures);

    std::vector<FutureObject*>
    getPredecessors();

    std::string
    getType();

    void
    setType(const std::string& futureType);

private:
    size_t ID;
    wstringVector propertiesNames;
    std::wstring functionName;
    bool _wasRead;
    NelsonException _exception;
    ArrayOfVector _result;
    int _nLhs;
    std::string _type;
    std::vector<FutureObject*> _predecessors;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
