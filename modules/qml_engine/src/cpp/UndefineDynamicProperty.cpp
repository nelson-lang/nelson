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
#include "UndefineDynamicProperty.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "QStringConverter.hpp"
#include "QVariantArrayOf.hpp"
#include "QmlHandleObject.hpp"
#include "characters_encoding.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
void
UndefineDynamicProperty(ArrayOf A, std::wstring propertyName)
{
    ArrayOf res;
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    QmlHandleObject* qmlhandleobj = (QmlHandleObject*)hlObj;
    void* ptr = qmlhandleobj->getPointer();
    if (ptr == nullptr) {
        Error(_W("QObject valid handle expected."));
    }
    QObject* qobj = (QObject*)ptr;
    if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_PARENT_STR)) {
        Error(_W("'parent' can not modified."));
    } else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CHILDREN_STR)) {
        Error(_W("'children' can not modified."));
    } else {
        bool isDynamicProperty = false;
        QList<QByteArray> names = qobj->dynamicPropertyNames();
        std::string upropertyname = wstring_to_utf8(propertyName);
        for (int k = 0; k < names.size(); k++) {
            std::string name = std::string(names[k]);
            if (name == upropertyname) {
                isDynamicProperty = true;
                break;
            }
        }
        if (isDynamicProperty) {
            QVariant undefined = QVariant();
            qobj->setProperty(upropertyname.c_str(), undefined);
        } else {
            Error(_W("'" + upropertyname + "'" + " can not modified."));
        }
    }
}
//=============================================================================
}
//=============================================================================
