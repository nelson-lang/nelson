//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
DeleteQmlHandleObject(ArrayOf A)
{
    bool res = false;
    if (A.isHandle()) {
        if (!A.isEmpty()) {
            Dimensions dims = A.getDimensions();
            nelson_handle* qp = (nelson_handle*)A.getDataPointer();
            for (indexType k = 0; k < dims.getElementCount(); k++) {
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
                                delete qobj;
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