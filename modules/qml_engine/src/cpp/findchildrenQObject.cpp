//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "findchildrenQObject.hpp"
#include "HandleManager.hpp"
#include "QStringConverter.hpp"
#include "QObjectHandleObject.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
findchildrenQObject(const ArrayOf& H, const std::wstring& fieldname, bool bRecursively)
{
    ArrayOf res = ArrayOf::emptyConstructor(0, 0);
    res.promoteType(NLS_HANDLE);
    HandleGenericObject* hlObj = H.getContentAsHandleScalar();
    if (hlObj->getCategory() != QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    QObjectHandleObject* qmlhandleobj = (QObjectHandleObject*)hlObj;
    void* ptr = qmlhandleobj->getPointer();
    if (ptr == nullptr) {
        Error(_W("QObject valid handle expected."));
    }
    QObject* qobj = (QObject*)ptr;
    Qt::FindChildOption option = Qt::FindDirectChildrenOnly;
    if (bRecursively) {
        option = Qt::FindChildrenRecursively;
    }
    QList<QObject*> qobjfound = qobj->findChildren<QObject*>(wstringToQString(fieldname), option);
    if (qobjfound.size() > 0) {
        Dimensions dims(1, qobjfound.size());
        nelson_handle* nh = (nelson_handle*)ArrayOf::allocateArrayOf(
            NLS_HANDLE, qobjfound.size(), stringVector(), false);
        for (int k = 0; k < qobjfound.size(); k++) {
            nelson_handle nh_found = HandleManager::getInstance()->findByPointerValue(qobjfound[k]);
            if (nh_found != -1) {
                nh[k] = nh_found;
            } else {
                QObjectHandleObject* qmlHandle = nullptr;
                try {
                    qmlHandle = new QObjectHandleObject(qobjfound[k]);
                } catch (const std::bad_alloc&) {
                    qmlHandle = nullptr;
                    Error(ERROR_MEMORY_ALLOCATION);
                }
                nh[k] = HandleManager::getInstance()->addHandle(qmlHandle);
            }
        }
        res = ArrayOf(NLS_HANDLE, dims, (void*)nh);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
