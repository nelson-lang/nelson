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
#include <QtGui/QQuaternion>
#include <QtQuick/QQuickItem>
#include <QtGui/qcolor.h>
#include <QtGui/QVector2D>
#include <QtGui/qmatrix4x4.h>
#include <QtCore/qrect.h>
#include <QtCore/qdatetime.h>
#include <QtCore/QStringList>
#include "SetQmlHandleObject.hpp"
#include "Exception.hpp"
#include "HandleGenericObject.hpp"
#include "QmlHandleObject.hpp"
#include "characters_encoding.hpp"
#include "QStringConverter.hpp"
#include "HandleManager.hpp"
#include "QVariantArrayOf.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    void SetQmlHandleObject(ArrayOf A, std::wstring propertyName, ArrayOf B)
    {
        ArrayOf res;
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
        void *ptr = qmlhandleobj->getPointer();
        if (ptr == nullptr)
        {
            throw Exception(_W("QObject valid handle expected."));
        }
        QObject *qobj = (QObject *)ptr;
        if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_PARENT_STR))
        {
            if (!B.isHandle())
            {
                throw Exception(ERROR_WRONG_ARGUMENT_2_TYPE_HANDLE_EXPECTED);
            }
            if (!B.isScalar())
            {
                throw Exception(ERROR_SIZE_SCALAR_EXPECTED);
            }
            nelson_handle *qpParent = (nelson_handle*)B.getDataPointer();
            if (qpParent == nullptr)
            {
                throw Exception(_W("QObject valid handle expected."));
            }
            nelson_handle hlParent = qpParent[0];
            HandleGenericObject *hlObjParent = HandleManager::getInstance()->getPointer(hlParent);
            if (hlObjParent == nullptr)
            {
                throw Exception(_W("QObject valid handle expected."));
            }
            if (hlObjParent->getCategory() != L"QObject")
            {
                throw Exception(_W("QObject handle expected."));
            }
            QmlHandleObject *qmlhandleobjparent = (QmlHandleObject *)hlObjParent;
            void *ptr = qmlhandleobjparent->getPointer();
            if (ptr == nullptr)
            {
                throw Exception(_W("QObject valid handle expected."));
            }
            QObject *qobjParent = (QObject *)ptr;
            if (qobjParent == qobj)
            {
                throw Exception(_W("QObject parent egals to the child."));
            }
            qobj->setParent(qobjParent);
        }
        else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CHILDREN_STR))
        {
            throw Exception(_W("'children' can not modified."));
        }
        else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CLASSNAME_STR))
        {
            throw Exception(_W("'className' can not modified."));
        }
        else
        {
            QVariant propertyValue = qobj->property(wstring_to_utf8(propertyName).c_str());
            if (!propertyValue.isValid())
            {
                QVariant v = ArrayOfToQVariant(B);
                qobj->setProperty(wstring_to_utf8(propertyName).c_str(), v);
            }
            else
            {
                QVariant::Type qtype = propertyValue.type();
                QVariant v = ArrayOfToQVariant(B, qtype);
                if (v.isValid())
                {
                    qobj->setProperty(wstring_to_utf8(propertyName).c_str(), v);
                }
                else
                {
                    throw Exception(_W("QVariant invalid."));
                }
            }
        }
    }
    //=============================================================================
}
//=============================================================================
