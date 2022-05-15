//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "iswindowtypeQObject.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "QObjectHandleObject.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
iswindowtypeQObject(const ArrayOf& A)
{
    if (!A.isHandle()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
    }
    std::wstring className;
    ClassName(A, className);
    if (className != QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
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
                        resArray[k] = qobj->isWindowType();
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
