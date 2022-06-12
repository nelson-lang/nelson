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
    FevalFutureObject(const std::wstring& functionName, size_t ID);
    ~FevalFutureObject() override;
    void
    setFuture(std::future<std::tuple<ArrayOfVector, Exception>> f);

    void
    display(Interface* io);

    std::tuple<ArrayOfVector, Exception>
    get(bool& valid);

    size_t
    getID();

    THREAD_STATE
    getState();

    std::atomic<THREAD_STATE>*
    getStatePtr();


private:
    void
    read();
    wstringVector propertiesNames;
    std::future<std::tuple<ArrayOfVector, Exception>> future;
    size_t ID;
    std::wstring functionName;
    std::atomic<THREAD_STATE> state;
    bool wasReaded;
    std::tuple<ArrayOfVector, Exception> content;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
