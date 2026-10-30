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

    // RAII marker: keeps this future alive for a worker run so the destructor waits before freeing
    class WorkerActivityGuard
    {
    public:
        explicit WorkerActivityGuard(FutureObject* owner);
        ~WorkerActivityGuard();
        WorkerActivityGuard(const WorkerActivityGuard&) = delete;
        WorkerActivityGuard& operator=(const WorkerActivityGuard&) = delete;

    private:
        FutureObject* _owner;
    };

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
    // nonzero while a worker runs this future's code; the destructor waits on it before freeing
    std::atomic<int> _workerActivityCount { 0 };
};
//=============================================================================
inline FutureObject::WorkerActivityGuard::WorkerActivityGuard(FutureObject* owner) : _owner(owner)
{
    _owner->_workerActivityCount.fetch_add(1, std::memory_order_release);
}
//=============================================================================
inline FutureObject::WorkerActivityGuard::~WorkerActivityGuard()
{
    _owner->_workerActivityCount.fetch_sub(1, std::memory_order_release);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
