//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fieldnamesQObjectHandleObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleManager.hpp"
#include "QStringConverter.hpp"
#include "QVariantArrayOf.hpp"
#include "characters_encoding.hpp"
#include <QtQml/QQmlComponent>
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
void
fieldnamesQObjectHandleObject(const ArrayOf& A, bool fullList, wstringVector& fieldnames)
{
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != NLS_HANDLE_QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    QObjectHandleObject* qmlhandleobj = static_cast<QObjectHandleObject*>(hlObj);
    fieldnamesQObjectHandleObject(qmlhandleobj, fullList, fieldnames);
}
//=============================================================================
void
fieldnamesQObjectHandleObject(
    QObjectHandleObject* qmlHandle, bool fullList, wstringVector& fieldnames)
{
    void* ptr = qmlHandle->getPointer();
    fieldnames.clear();
    if (ptr == nullptr) {
        Error(_W("QObject valid handle expected."));
    }
    QObject* qobj = (QObject*)ptr;
    const QMetaObject* meta = qobj->metaObject();
    stringVector allFields;
    for (int i = 0; i < meta->propertyCount(); i++) {
        QMetaProperty property = meta->property(i);
        const char* name = property.name();
        if (std::find(allFields.begin(), allFields.end(), name) == allFields.end()) {
            allFields.push_back(name);
        }
    }
    QList<QByteArray> names = qobj->dynamicPropertyNames();
    for (auto& k : names) {
        std::string name = std::string(k);
        if (std::find(allFields.begin(), allFields.end(), name) == allFields.end()) {
            allFields.push_back(name);
        }
    }
    if (std::find(allFields.begin(), allFields.end(), QOBJECT_PROPERTY_PARENT_STR)
        == allFields.end()) {
        QObject* parent = qobj->parent();
        if (parent) {
            allFields.push_back(std::string(QOBJECT_PROPERTY_PARENT_STR));
        }
    }
    if (std::find(allFields.begin(), allFields.end(), QOBJECT_PROPERTY_CHILDREN_STR)
        == allFields.end()) {
        QObjectList childs = qobj->children();
        int s = childs.size();
        if (s > 0) {
            allFields.push_back(std::string(QOBJECT_PROPERTY_CHILDREN_STR));
        }
    }
    if (std::find(allFields.begin(), allFields.end(), QOBJECT_PROPERTY_CLASSNAME_STR)
        == allFields.end()) {
        allFields.push_back(std::string(QOBJECT_PROPERTY_CLASSNAME_STR));
    }
    if (!allFields.empty()) {
        std::sort(allFields.begin(), allFields.end());
    }
    if (fullList) {
        for (const std::string& allField : allFields) {
            fieldnames.push_back(utf8_to_wstring(allField));
        }
    } else {
        for (const std::string& allField : allFields) {
            if (allField == QOBJECT_PROPERTY_PARENT_STR || allField == QOBJECT_PROPERTY_CHILDREN_STR
                || allField == QOBJECT_PROPERTY_CLASSNAME_STR) {
                fieldnames.push_back(utf8_to_wstring(allField));
            } else {
                QVariant propertyValue = qobj->property(allField.c_str());
                if (propertyValue.isValid()) {
                    if (canBeConvertedToArrayOf(propertyValue)) {
                        fieldnames.push_back(utf8_to_wstring(allField));
                    } else {
                        if (propertyValue.canConvert<QObject*>()) {
                            fieldnames.push_back(utf8_to_wstring(allField));
                        }
                    }
                }
            }
        }
    }
}
}
//=============================================================================
