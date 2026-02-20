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
#include "PredefinedErrorMessages.hpp"
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
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != NLS_HANDLE_QOBJECT_CATEGORY_STR) {
        raiseError2(L"nelson:validators:mustBeTypeAtPosition", 1,
            utf8_to_wstring(NLS_HANDLE_QOBJECT_CATEGORY_STR));
    }
    QObjectHandleObject* qmlhandleobj = (QObjectHandleObject*)hlObj;
    void* ptr = qmlhandleobj->getPointer();
    if (ptr == nullptr) {
        raiseError2(L"nelson:validators:mustBeTypeAtPosition", 1,
            utf8_to_wstring(NLS_HANDLE_QOBJECT_CATEGORY_STR));
    }
    QObject* qobj = (QObject*)ptr;
    if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_PARENT_STR)) {
        raiseError(
            L"Nelson:qml_engine:ERROR_PARENT_CANNOT_BE_MODIFIED", ERROR_PARENT_CANNOT_BE_MODIFIED);
    } else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CHILDREN_STR)) {
        raiseError(L"Nelson:qml_engine:ERROR_CHILDREN_CANNOT_BE_MODIFIED",
            ERROR_CHILDREN_CANNOT_BE_MODIFIED);
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
            raiseError(L"Nelson:qml_engine:ERROR_PROPERTY_CANNOT_BE_MODIFIED",
                ERROR_PROPERTY_CANNOT_BE_MODIFIED, utf8_to_wstring(upropertyname));
        }
    }
}
//=============================================================================
}
//=============================================================================
