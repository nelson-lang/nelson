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
#include <QtCore/QObject>
#include "nlsQml_engine_exports.h"
#include "QObjectHandleObject.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
QObjectHandleObject*
QObjectHandleObjectAllocator(QObject* qobj)
{
    QObjectHandleObject* qObjectHandle = nullptr;
    try {
        qObjectHandle = new QObjectHandleObject(qobj);
    } catch (const std::bad_alloc&) {
        qObjectHandle = nullptr;
        Error(ERROR_MEMORY_ALLOCATION);
    }
    return qObjectHandle;
}
//=============================================================================
}
//=============================================================================
