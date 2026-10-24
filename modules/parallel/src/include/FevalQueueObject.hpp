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
#include <future>
#include <memory>
#include <vector>
#include "nlsParallel_exports.h"
#include "HandleGenericObject.hpp"
#include "HandleState.hpp"
#include "Types.hpp"
#include "ArrayOf.hpp"
#include "Interface.hpp"
#include "NelsonException.hpp"
#include "FevalFutureObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSPARALLEL_IMPEXP FevalQueueObject : public HandleGenericObject
{
public:
    static FevalQueueObject*
    getInstance();

    void
    destroy();

    static bool
    isInitialized();

    void
    add(const HandleRef& fevalFutureObject);

    bool
    isMethod(const std::wstring& methodName) override;

    void
    display(Interface* io);

    bool
    get(const std::wstring& propertyName, ArrayOf& result);

    bool
    useSharedHandleOwnership() const override
    {
        return false;
    }

    void
    reset();

    void
    cleanupFinishedFutures();

    void
    cancelAll();

private:
    FevalQueueObject();
    ~FevalQueueObject() override;

    static FevalQueueObject* m_pInstance;

    std::vector<std::weak_ptr<HandleState>> fEvalQueue;

    wstringVector propertiesNames;

    std::vector<nelson_handle>
    searchThreadsByState(THREAD_STATE stateDesired);

    ArrayOf
    getThreadsByState(THREAD_STATE stateDesired);

    void
    refreshQueue();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
