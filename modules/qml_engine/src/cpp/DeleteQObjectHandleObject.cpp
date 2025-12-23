//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DeleteQObjectHandleObject.hpp"
#include "HandleManager.hpp"
#include "MainGuiObject.hpp"
#include "QObjectHandleObject.hpp"
#include <QtGui/QWindow>
#include <QtQml/QQmlComponent>
#include <QtQml/QQmlEngine>
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
DeleteQObjectHandleObject(const ArrayOf& A)
{
    auto deleter = [](QObjectHandleObject* qmlhandleobj) {
        void* ptr = qmlhandleobj->getPointer();
        if (ptr) {
            QObject* qobj = static_cast<QObject*>(ptr);
            QObject* qobjMainWindow = static_cast<QObject*>(GetMainGuiObject());
            if (qobj == qobjMainWindow) {
                qmlhandleobj->setPointer(nullptr);
            } else {
                int idx = qobj->metaObject()->indexOfProperty("visible");
                if (idx != -1) {
                    qobj->setProperty("visible", QVariant(false));
                }
                if (qobj->isWindowType()) {
                    QWindow* w = static_cast<QWindow*>(qobj);
                    w->destroy();
                } else {
                    qobj->deleteLater();
                }
                qmlhandleobj->setPointer(nullptr);
            }
        }
        delete qmlhandleobj;
    };

    return DeleteHandleObjects<QObjectHandleObject>(A, NLS_HANDLE_QOBJECT_CATEGORY_STR,
        _W("QObject handle expected."), _W("QObject scalar handle expected."), deleter);
}
//=============================================================================
}
//=============================================================================
