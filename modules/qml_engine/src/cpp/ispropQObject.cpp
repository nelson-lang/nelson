//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include <QtQml/QQmlComponent>
#include "ispropQObject.hpp"
#include "Exception.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
#include "QmlHandleObject.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    bool ispropQObject(QmlHandleObject *qmlHandle, std::wstring propertyName)
    {
        void *ptr = qmlHandle->getPointer();
        if (ptr == nullptr)
        {
            throw Exception(_W("QObject valid handle expected."));
        }
        QObject *qobj = (QObject *)ptr;
        const QMetaObject *meta = qobj->metaObject();
        std::string upropertyName = wstring_to_utf8(propertyName);
        for (int i = 0; i < meta->propertyCount(); i++)
        {
            QMetaProperty property = meta->property(i);
            const char *name = property.name();
            std::string str = std::string(name);
            if (str == upropertyName)
            {
                return true;
            }
        }
        QList<QByteArray> names = qobj->dynamicPropertyNames();
        for (int k = 0; k < names.size(); k++)
        {
            std::string name = std::string(names[k]);
            if (name == upropertyName)
            {
                return true;
            }
        }
        if (upropertyName == "parent")
        {
            QObject *parent = qobj->parent();
            if (parent)
            {
                return true;
            }
        }
        if (upropertyName == "children")
        {
            QObjectList childs = qobj->children();
            int s = childs.size();
            if (s > 0)
            {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    ArrayOf ispropQObject(ArrayOf A, std::wstring propertyName)
    {
        if (!A.isHandle())
        {
            throw Exception(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
        }
        if (!A.isScalar())
        {
            throw Exception(ERROR_SIZE_SCALAR_EXPECTED);
        }
        nelson_handle *qp = (nelson_handle*)A.getDataPointer();
        if (qp == nullptr)
        {
            throw Exception(_W("QObject valid handle expected."));
        }
        nelson_handle hl = qp[0];
        HandleGenericObject *hlObj = HandleManager::getInstance()->getPointer(hl);
        if (hlObj == nullptr)
        {
            throw Exception(_W("QObject valid handle expected."));
        }
        if (hlObj->getCategory() != L"QObject")
        {
            throw Exception(_W("QObject handle expected."));
        }
        QmlHandleObject *qmlhandleobj = (QmlHandleObject *)hlObj;
        bool res = ispropQObject(qmlhandleobj, propertyName);
        return ArrayOf::logicalConstructor(res);
    }
    //=============================================================================
}
//=============================================================================
