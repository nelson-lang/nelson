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
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
DeleteQObjectHandleObject(const ArrayOf& A)
{
    auto deleter = [](HandleGenericObject* qmlhandleobj) {
        qmlhandleobj->onDelete(nullptr);
        void* ptr = qmlhandleobj->pointer();
        if (ptr) {
            QObject* qobj = static_cast<QObject*>(ptr);
            QObject* qobjMainWindow = static_cast<QObject*>(GetMainGuiObject());
            if (qobj == qobjMainWindow) {
                qmlhandleobj->setPointer(nullptr);
            } else {
                int idx = qobj->metaObject()->indexOfProperty("visible");
                if (idx != -1 || qobj->property("visible").isValid()) {
                    qobj->setProperty("visible", QVariant(false));
                    if (qobj->property("visible").isValid()) {
                        auto* objectHandle = dynamic_cast<QObjectHandleObject*>(qmlhandleobj);
                        if (objectHandle != nullptr) {
                            objectHandle->propertyChanged(L"visible");
                        }
                    }
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

    return DeleteHandleObjects<HandleGenericObject>(A, NLS_HANDLE_QOBJECT_CATEGORY_STR,
        Nelson::resolveMessageTemplate(L"Nelson:QObject:notHandle", L"QObject handle expected."),
        Nelson::resolveMessageTemplate(
            L"Nelson:qml_engine:qobjectScalarHandle", L"QObject scalar handle expected."),
        deleter);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
