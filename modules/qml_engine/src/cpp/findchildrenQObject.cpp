//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "findchildrenQObject.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
#include "QStringConverter.hpp"
#include "QObjectHandleObject.hpp"
#include <QtQml/QQmlComponent>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
findchildrenQObject(const ArrayOf& H, const std::wstring& fieldname, bool bRecursively)
{
    ArrayOf res = ArrayOf::empty(0, 0);
    res.promoteType(NLS_HANDLE);
    HandleGenericObject* hlObj = H.asHandleScalar();
    if (hlObj->category() != NLS_HANDLE_QOBJECT_CATEGORY_STR) {
        Error(L"Nelson:QObject:notHandle", L"QObject handle expected.");
    }
    QObjectHandleObject* qmlhandleobj = static_cast<QObjectHandleObject*>(hlObj);
    void* ptr = qmlhandleobj->pointer();
    if (ptr == nullptr) {
        Error(L"Nelson:QObject:invalidHandle", L"QObject valid handle expected.");
    }
    QObject* qobj = static_cast<QObject*>(ptr);
    Qt::FindChildOption option = Qt::FindDirectChildrenOnly;
    if (bRecursively) {
        option = Qt::FindChildrenRecursively;
    }
    QList<QObject*> qobjfound = qobj->findChildren<QObject*>(wstringToQString(fieldname), option);
    if (qobjfound.size() > 0) {
        Dimensions dims(1, qobjfound.size());
        HandleRef* nh = (HandleRef*)ArrayOf::allocateArrayOf(
            NLS_HANDLE, qobjfound.size(), stringVector(), false);
        for (int k = 0; k < qobjfound.size(); k++) {
            nelson_handle nh_found = HandleManager::getInstance()->findByPointerValue(qobjfound[k]);
            if (nh_found != -1) {
                nh[k] = HandleManager::getInstance()->getHandleRef(nh_found);
            } else {
                QObjectHandleObject* qmlHandle = nullptr;
                try {
                    qmlHandle = new QObjectHandleObject(qobjfound[k]);
                } catch (const std::bad_alloc&) {
                    qmlHandle = nullptr;
                    Error(L"Nelson:outOfMemory",
                        L"Memory allocation error... You may have run out of memory!");
                }
                nh[k] = HandleManager::getInstance()->addHandleRef(qmlHandle);
            }
        }
        res = ArrayOf(NLS_HANDLE, dims, (void*)nh);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
