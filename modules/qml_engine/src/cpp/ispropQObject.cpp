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
#include "ispropQObject.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "QmlHandleObject.hpp"
#include "characters_encoding.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ispropQObject(QmlHandleObject* qmlHandle, const std::wstring& propertyName)
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
    for (int k = 0; k < names.size(); k++) {
        std::string name = std::string(names[k]);
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
ispropQObject(ArrayOf A, const std::wstring& propertyName)
{
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    QmlHandleObject* qmlhandleobj = (QmlHandleObject*)hlObj;
    bool res = ispropQObject(qmlhandleobj, propertyName);
    return ArrayOf::logicalConstructor(res);
}
//=============================================================================
}
//=============================================================================
