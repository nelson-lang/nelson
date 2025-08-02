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
#include <vector>
#include "nlsParallel_exports.h"
#include "HandleGenericObject.hpp"
#include "Types.hpp"
#include "ArrayOf.hpp"
#include "Interface.hpp"
#include "Exception.hpp"
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

    void
    add(FevalFutureObject* fevalFutureObject);

    bool
    isMethod(const std::wstring& methodName) override;

    void
    display(Interface* io);

    bool
    get(const std::wstring& propertyName, ArrayOf& result);

    void
    reset();

    void
    cancelAll();

private:
    FevalQueueObject();
    ~FevalQueueObject() override;

    static FevalQueueObject* m_pInstance;

    std::vector<FevalFutureObject*> fEvalQueue;

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
