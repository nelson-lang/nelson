//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GetQObjectHandleObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
#include "QVariantArrayOf.hpp"
#include "QObjectHandleObject.hpp"
#include "characters_encoding.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
GetQObjectHandleObject(const ArrayOf& A, const std::wstring& propertyName)
{
    ArrayOf res;
    HandleGenericObject* hlObj = A.asHandleScalar();
    if (hlObj->category() != NLS_HANDLE_QOBJECT_CATEGORY_STR) {
        Error(L"Nelson:QObject:notHandle", L"QObject handle expected.");
    }
    QObjectHandleObject* qmlhandleobj = (QObjectHandleObject*)hlObj;
    void* ptr = qmlhandleobj->pointer();
    if (ptr == nullptr) {
        Error(L"Nelson:QObject:invalidHandle", L"QObject valid handle expected.");
    }
    QObject* qobj = (QObject*)ptr;
    if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_PARENT_STR)) {
        QObject* qparent = qobj->parent();
        if (qparent) {
            nelson_handle nh_found = HandleManager::getInstance()->findByPointerValue(qparent);
            if (nh_found != -1) {
                res = ArrayOf::handle(nh_found);
            } else {
                QObjectHandleObject* qmlHandle = nullptr;
                try {
                    qmlHandle = new QObjectHandleObject(qparent);
                } catch (const std::bad_alloc&) {
                    qmlHandle = nullptr;
                    Error(L"Nelson:outOfMemory",
                        L"Memory allocation error... You may have run out of memory!");
                }
                res = ArrayOf::handle(qmlHandle);
            }
        } else {
            Error(L"Nelson:QObject:noParent", L"No parent.");
        }
    } else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CLASSNAME_STR)) {
        std::string name = std::string(qobj->metaObject()->className());
        res = ArrayOf::characterArray(name);
    } else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CHILDREN_STR)) {
        QObjectList children = qobj->children();
        int nbChildren = children.size();
        if (nbChildren == 0) {
            Dimensions dims(0, 0);
            res = ArrayOf::empty(dims);
            res.promoteType(NLS_HANDLE);
        } else {
            Dimensions dims(1, nbChildren);
            HandleRef* nh = (HandleRef*)ArrayOf::allocateArrayOf(
                NLS_HANDLE, nbChildren, stringVector(), false);
            for (int k = 0; k < nbChildren; k++) {
                nelson_handle nh_found
                    = HandleManager::getInstance()->findByPointerValue(children[k]);
                if (nh_found != -1) {
                    nh[k] = HandleManager::getInstance()->getHandleRef(nh_found);
                } else {
                    QObjectHandleObject* qmlHandle = nullptr;
                    try {
                        qmlHandle = new QObjectHandleObject(children[k]);
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
    } else {
        QVariant propertyValue = qobj->property(wstring_to_utf8(propertyName).c_str());
        res = QVariantToArrayOf(propertyValue);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
