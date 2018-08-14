//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "fieldnamesQmlHandleObject.hpp"
#include "Error.hpp"
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
fieldnamesQmlHandleObject(ArrayOf A, bool fullList, wstringVector& fieldnames)
{
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    QmlHandleObject* qmlhandleobj = (QmlHandleObject*)hlObj;
    fieldnamesQmlHandleObject(qmlhandleobj, fullList, fieldnames);
}
//=============================================================================
void
fieldnamesQmlHandleObject(QmlHandleObject* qmlHandle, bool fullList, wstringVector& fieldnames)
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
    for (int k = 0; k < names.size(); k++) {
        std::string name = std::string(names[k]);
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
    std::sort(allFields.begin(), allFields.end());
    if (fullList) {
        for (size_t k = 0; k < allFields.size(); k++) {
            fieldnames.push_back(utf8_to_wstring(allFields[k]));
        }
    } else {
        for (size_t k = 0; k < allFields.size(); k++) {
            if (allFields[k] == QOBJECT_PROPERTY_PARENT_STR
                || allFields[k] == QOBJECT_PROPERTY_CHILDREN_STR
                || allFields[k] == QOBJECT_PROPERTY_CLASSNAME_STR) {
                fieldnames.push_back(utf8_to_wstring(allFields[k]));
            } else {
                QVariant propertyValue = qobj->property(allFields[k].c_str());
                if (propertyValue.isValid()) {
                    if (canBeConvertedToArrayOf(propertyValue)) {
                        fieldnames.push_back(utf8_to_wstring(allFields[k]));
                    } else {
                        QObject* obj = qvariant_cast<QObject*>(propertyValue);
                        if (obj != nullptr) {
                            fieldnames.push_back(utf8_to_wstring(allFields[k]));
                        }
                    }
                }
            }
        }
    }
}
}
//=============================================================================
