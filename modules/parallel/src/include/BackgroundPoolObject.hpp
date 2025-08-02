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
#include <BS_thread_pool.hpp>
#include "nlsParallel_exports.h"
#include "HandleGenericObject.hpp"
#include "Types.hpp"
#include "ArrayOf.hpp"
#include "FevalFutureObject.hpp"
#include "FunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSPARALLEL_IMPEXP BackgroundPoolObject : public HandleGenericObject
{
public:
    static BackgroundPoolObject*
    getInstance();
    void
    destroy();

    bool
    get(const std::wstring& propertyName, ArrayOf& result);

    void
    display(Interface* io);

    ArrayOf
    feval(FunctionDef* fptr, int nLhs, const ArrayOfVector& argIn);

    size_t
    getTasksQueued();
    size_t
    getTasksRunning();
    size_t
    getNumberOfThreads();

    wstringVector
    fieldnames();

    wstringVector
    getProperties() override;
    wstringVector
    getMethods() override;

    void
    resetThreadPool();

    static bool
    isInitialized();

private:
    BackgroundPoolObject();
    ~BackgroundPoolObject() override;

    static BackgroundPoolObject* m_pInstance;

    wstringVector propertiesNames;

    BS::pause_thread_pool* threadPool = nullptr;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
