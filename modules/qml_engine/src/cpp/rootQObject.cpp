//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "rootQObject.hpp"
#include "HandleManager.hpp"
#include "MainGuiObject.hpp"
#include "QObjectHandleObject.hpp"
#include <QtGui/QWindow>
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
rootQObject()
{
    ArrayOf res;
    QWindow* parent = (QWindow*)GetMainGuiObject();
    if (parent) {
        QObjectHandleObject* qmlHandle = nullptr;
        try {
            qmlHandle = new QObjectHandleObject(parent);
        } catch (const std::bad_alloc&) {
            qmlHandle = nullptr;
            Error(ERROR_MEMORY_ALLOCATION);
        }
        res = ArrayOf::handleConstructor(qmlHandle);
    } else {
        res = ArrayOf::emptyConstructor(0, 0);
        res.promoteType(NLS_HANDLE);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
