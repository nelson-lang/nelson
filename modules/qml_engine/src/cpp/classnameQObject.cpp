//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "classnameQObject.hpp"
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
classnameQObject(const ArrayOf& A)
{
    if (!A.isHandle()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
    }
    std::string className;
    ClassName(A, className);
    if (className != NLS_HANDLE_QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    ArrayOf res;
    Dimensions dimsA = A.getDimensions();
    nelson_handle* qp = (nelson_handle*)A.getDataPointer();
    if (qp) {
        stringVector names;
        indexType elementCount = dimsA.getElementCount();
        for (indexType k = 0; k < elementCount; k++) {
            nelson_handle hl = qp[k];
            HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
            if (hlObj != nullptr) {
                if (hlObj->getPointer()) {
                    QObjectHandleObject* qmlHandle = (QObjectHandleObject*)hlObj;
                    void* ptr = qmlHandle->getPointer();
                    if (ptr == nullptr) {
                        names.push_back("");
                    } else {
                        QObject* qobj = (QObject*)ptr;
                        std::string name = std::string(qobj->metaObject()->className());
                        names.push_back(name);
                    }
                } else {
                    names.push_back("");
                }
            } else {
                names.push_back("");
            }
        }
        if (names.size() == 1) {
            res = ArrayOf::characterArrayConstructor(names[0]);
        } else {
            res = ArrayOf::toCellArrayOfCharacterColumnVectors(names);
        }
    } else {
        res = ArrayOf::emptyConstructor(dimsA);
        res.promoteType(NLS_CELL_ARRAY);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
