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
#include "HandleGenericObject.hpp"
#include "nlsQml_engine_exports.h"
#include <string>
//=============================================================================
#define QOBJECT_CATEGORY_STR L"QObject"
#define QOBJECT_PROPERTY_PARENT_STR "parent"
#define QOBJECT_PROPERTY_CHILDREN_STR "children"
#define QOBJECT_PROPERTY_CLASSNAME_STR "className"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSQML_ENGINE_IMPEXP QmlHandleObject : public HandleGenericObject
{
public:
    QmlHandleObject(void* _ptr);
    ~QmlHandleObject();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
