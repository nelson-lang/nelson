//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UndefineDynamicProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "QStringConverter.hpp"
#include "QVariantArrayOf.hpp"
#include "QObjectHandleObject.hpp"
#include "characters_encoding.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
void
UndefineDynamicProperty(const ArrayOf& A, const std::wstring& propertyName)
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
        Error(L"Nelson:QObject:readOnlyProperty", L"'%s' can not modified.", L"parent");
    } else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CHILDREN_STR)) {
        Error(L"Nelson:QObject:readOnlyProperty", L"'%s' can not modified.", L"children");
    } else {
        bool isDynamicProperty = false;
        QList<QByteArray> names = qobj->dynamicPropertyNames();
        std::string upropertyname = wstring_to_utf8(propertyName);
        for (auto& k : names) {
            std::string name = std::string(k);
            if (name == upropertyname) {
                isDynamicProperty = true;
                break;
            }
        }
        if (isDynamicProperty) {
            QVariant undefined = QVariant();
            qobj->setProperty(upropertyname.c_str(), undefined);
        } else {
            Error(L"Nelson:QObject:readOnlyProperty", L"'%s' can not modified.", propertyName);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
