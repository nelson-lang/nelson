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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
#include "QObjectHandleObject.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
classnameQObject(const ArrayOf& A)
{
    if (!A.isHandle()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    std::string className;
    ClassName(A, className);
    if (className != NLS_HANDLE_QOBJECT_CATEGORY_STR) {
        Error(L"Nelson:QObject:notHandle", L"QObject handle expected.");
    }
    ArrayOf res;
    Dimensions dimsA = A.dimensions();
    HandleRef* qp = (HandleRef*)A.dataPointer();
    if (qp) {
        stringVector names;
        indexType elementCount = dimsA.elementCount();
        for (indexType k = 0; k < elementCount; k++) {
            HandleGenericObject* hlObj = qp[k] ? qp[k]->object() : nullptr;
            if (hlObj != nullptr) {
                if (hlObj->pointer()) {
                    QObjectHandleObject* qmlHandle = (QObjectHandleObject*)hlObj;
                    void* ptr = qmlHandle->pointer();
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
            res = ArrayOf::characterArray(names[0]);
        } else {
            res = ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(names);
        }
    } else {
        res = ArrayOf::empty(dimsA);
        res.promoteType(NLS_CELL_ARRAY);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
