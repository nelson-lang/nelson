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
#include <string>
#include "HandleGenericObject.hpp"
#include "nlsQml_engine_exports.h"
//=============================================================================
#define QOBJECT_PROPERTY_PARENT_STR "parent"
#define QOBJECT_PROPERTY_CHILDREN_STR "children"
#define QOBJECT_PROPERTY_CLASSNAME_STR "className"
//=============================================================================
namespace Nelson {
//=============================================================================
class QObjectHandleObject : public HandleGenericObject
{
public:
    QObjectHandleObject(void* _ptr)
        : HandleGenericObject(NLS_HANDLE_QOBJECT_CATEGORY_STR, _ptr, false)
    {
    }
    ~QObjectHandleObject() override = default;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
