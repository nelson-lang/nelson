//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "iswidgettypeQObject.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleManager.hpp"
#include "QObjectHandleObject.hpp"
#include <QtQml/QQmlComponent>
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
iswidgettypeQObject(const ArrayOf& A)
{
    if (!A.isHandle()) {
        raiseError(L"Nelson:qml:ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED",
            ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED, 1, NLS_HANDLE_STR);
    }
    std::string className;
    ClassName(A, className);
    if (className != NLS_HANDLE_QOBJECT_CATEGORY_STR) {
        raiseError(L"Nelson:qml:ERROR_QOBJECT_HANDLE_EXPECTED", ERROR_QOBJECT_HANDLE_EXPECTED);
    }
    ArrayOf res;
    Dimensions dimsA = A.getDimensions();
    nelson_handle* qp = (nelson_handle*)A.getDataPointer();
    if (qp) {
        logical* resArray = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false);
        indexType elementCount = dimsA.getElementCount();
        for (indexType k = 0; k < elementCount; k++) {
            nelson_handle hl = qp[k];
            HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
            if (hlObj != nullptr) {
                if (hlObj->getPointer()) {
                    QObjectHandleObject* qmlHandle = (QObjectHandleObject*)hlObj;
                    void* ptr = qmlHandle->getPointer();
                    if (ptr == nullptr) {
                        resArray[k] = false;
                    } else {
                        QObject* qobj = (QObject*)ptr;
                        resArray[k] = qobj->isWidgetType();
                    }
                } else {
                    resArray[k] = false;
                }
            } else {
                resArray[k] = false;
            }
        }
        res = ArrayOf(NLS_LOGICAL, dimsA, resArray);
    } else {
        res = ArrayOf::emptyConstructor(dimsA);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
