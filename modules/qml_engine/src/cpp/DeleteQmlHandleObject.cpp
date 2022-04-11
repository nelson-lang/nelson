//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DeleteQmlHandleObject.hpp"
#include "HandleManager.hpp"
#include "MainGuiObject.hpp"
#include "QmlHandleObject.hpp"
#include <QtGui/QWindow>
#include <QtQml/QQmlComponent>
#include <QtQml/QQmlEngine>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
DeleteQmlHandleObject(const ArrayOf& A)
{
    bool res = false;
    if (A.isHandle()) {
        if (!A.isEmpty()) {
            Dimensions dims = A.getDimensions();
            nelson_handle* qp = (nelson_handle*)A.getDataPointer();
            indexType elementCount = dims.getElementCount();
            for (indexType k = 0; k < elementCount; k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj) {
                    if (hlObj->getCategory() != QOBJECT_CATEGORY_STR) {
                        Error(_W("QObject handle expected."));
                    }
                    QmlHandleObject* qmlhandleobj = (QmlHandleObject*)hlObj;
                    void* ptr = qmlhandleobj->getPointer();
                    if (ptr) {
                        QObject* qobj = (QObject*)ptr;
                        QObject* qobjMainWindow = (QObject*)GetMainGuiObject();
                        if (qobj == qobjMainWindow) {
                            qmlhandleobj->setPointer(nullptr);
                        } else {
                            if (qobj->isWindowType()) {
                                QWindow* w = static_cast<QWindow*>(qobj);
                                w->destroy();
                            } else {
                                qobj->deleteLater();
                            }
                        }
                        qmlhandleobj->setPointer(nullptr);
                    }
                    delete qmlhandleobj;
                    HandleManager::getInstance()->removeHandle(hl);
                    res = true;
                }
            }
        } else {
            Error(_W("QObject scalar handle expected."));
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
