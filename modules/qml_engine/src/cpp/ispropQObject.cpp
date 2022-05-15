//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ispropQObject.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "QObjectHandleObject.hpp"
#include "characters_encoding.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ispropQObject(QObjectHandleObject* qmlHandle, const std::wstring& propertyName)
{
    void* ptr = qmlHandle->getPointer();
    if (ptr == nullptr) {
        Error(_W("QObject valid handle expected."));
    }
    QObject* qobj = (QObject*)ptr;
    const QMetaObject* meta = qobj->metaObject();
    std::string upropertyName = wstring_to_utf8(propertyName);
    for (int i = 0; i < meta->propertyCount(); i++) {
        QMetaProperty property = meta->property(i);
        const char* name = property.name();
        std::string str = std::string(name);
        if (str == upropertyName) {
            return true;
        }
    }
    QList<QByteArray> names = qobj->dynamicPropertyNames();
    for (auto& k : names) {
        std::string name = std::string(k);
        if (name == upropertyName) {
            return true;
        }
    }
    if (upropertyName == QOBJECT_PROPERTY_PARENT_STR) {
        QObject* parent = qobj->parent();
        if (parent) {
            return true;
        }
    }
    if (upropertyName == QOBJECT_PROPERTY_CHILDREN_STR) {
        QObjectList childs = qobj->children();
        int s = childs.size();
        if (s > 0) {
            return true;
        }
    }
    return false;
}
//=============================================================================
ArrayOf
ispropQObject(const ArrayOf& A, const std::wstring& propertyName)
{
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    QObjectHandleObject* qmlhandleobj = (QObjectHandleObject*)hlObj;
    bool res = ispropQObject(qmlhandleobj, propertyName);
    return ArrayOf::logicalConstructor(res);
}
//=============================================================================
}
//=============================================================================
