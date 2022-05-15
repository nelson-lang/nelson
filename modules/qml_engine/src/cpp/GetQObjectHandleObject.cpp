//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GetQObjectHandleObject.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
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
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    QObjectHandleObject* qmlhandleobj = (QObjectHandleObject*)hlObj;
    void* ptr = qmlhandleobj->getPointer();
    if (ptr == nullptr) {
        Error(_W("QObject valid handle expected."));
    }
    QObject* qobj = (QObject*)ptr;
    if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_PARENT_STR)) {
        QObject* qparent = qobj->parent();
        if (qparent) {
            nelson_handle nh_found = HandleManager::getInstance()->findByPointerValue(qparent);
            if (nh_found != -1) {
                res = ArrayOf::handleConstructor(nh_found);
            } else {
                QObjectHandleObject* qmlHandle = nullptr;
                try {
                    qmlHandle = new QObjectHandleObject(qparent);
                } catch (const std::bad_alloc&) {
                    qmlHandle = nullptr;
                    Error(ERROR_MEMORY_ALLOCATION);
                }
                res = ArrayOf::handleConstructor(qmlHandle);
            }
        } else {
            Error(_W("No parent."));
        }
    } else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CLASSNAME_STR)) {
        std::string name = std::string(qobj->metaObject()->className());
        res = ArrayOf::characterArrayConstructor(name);
    } else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CHILDREN_STR)) {
        QObjectList childs = qobj->children();
        int nbChilds = childs.size();
        if (nbChilds == 0) {
            Dimensions dims(0, 0);
            res = ArrayOf::emptyConstructor(dims);
            res.promoteType(NLS_HANDLE);
        } else {
            Dimensions dims(1, nbChilds);
            nelson_handle* nh = (nelson_handle*)ArrayOf::allocateArrayOf(
                NLS_HANDLE, nbChilds, stringVector(), false);
            for (int k = 0; k < nbChilds; k++) {
                nelson_handle nh_found
                    = HandleManager::getInstance()->findByPointerValue(childs[k]);
                if (nh_found != -1) {
                    nh[k] = nh_found;
                } else {
                    QObjectHandleObject* qmlHandle = nullptr;
                    try {
                        qmlHandle = new QObjectHandleObject(childs[k]);
                    } catch (const std::bad_alloc&) {
                        qmlHandle = nullptr;
                        Error(ERROR_MEMORY_ALLOCATION);
                    }
                    nh[k] = HandleManager::getInstance()->addHandle(qmlHandle);
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
}
//=============================================================================
