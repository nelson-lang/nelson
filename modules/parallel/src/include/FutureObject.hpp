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
#include <vector>
#include <atomic>
#include "nlsParallel_exports.h"
#include "Types.hpp"
#include "ArrayOf.hpp"
#include "Exception.hpp"
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

    bool
    isMethod(const std::wstring& methodName);

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
    cancel();

    void
    evaluateFunction(
        FunctionDef* fptr, int nLhs, const ArrayOfVector& argIn, bool changeState = true);

    ArrayOfVector
    getResult(bool changeReadState = true);

    void
    setResult(const ArrayOfVector& result);

    Exception
    getException(bool changeReadState = true);

    void
    setException(const Exception& e);

    int
    getNumberOfLhs();

    bool
    wasRead();

    void
    setPredecessors(const std::vector<FutureObject*>& futures);

    std::vector<FutureObject*>
    getPredecessors();

    std::wstring
    getType();

    void
    setType(const std::wstring& futureType);

private:
    size_t ID;
    wstringVector propertiesNames;
    std::wstring functionName;
    bool _wasRead;
    Exception _exception;
    ArrayOfVector _result;
    int _nLhs;
    std::wstring _type;
    std::vector<FutureObject*> _predecessors;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
