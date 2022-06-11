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
#define FEVALQUEUE_CATEGORY_STR L"FevalQueue"
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

    size_t
    getQueueLength();

    void
    display(Interface* io);

    std::vector<FevalFutureObject*>
    getQueue();

private:
    FevalQueueObject();
    ~FevalQueueObject() override;

    static FevalQueueObject* m_pInstance;

    std::vector<FevalFutureObject*> fEvalQueue;
    wstringVector propertiesNames;

};
//=============================================================================
} // namespace Nelson
//=============================================================================
