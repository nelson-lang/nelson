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
#include "SetQmlHandleObject.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "QStringConverter.hpp"
#include "QVariantArrayOf.hpp"
#include "QmlHandleObject.hpp"
#include "characters_encoding.hpp"
#include <QtCore/QStringList>
#include <QtCore/qdatetime.h>
#include <QtCore/qrect.h>
#include <QtGui/QQuaternion>
#include <QtGui/QVector2D>
#include <QtGui/qcolor.h>
#include <QtGui/qmatrix4x4.h>
#include <QtQml/QQmlComponent>
#include <QtQuick/QQuickItem>
//=============================================================================
namespace Nelson {
//=============================================================================
void
SetQmlHandleObject(ArrayOf A, std::wstring propertyName, ArrayOf B)
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
        HandleGenericObject* hlObjParent = B.getContentAsHandleScalar();
        if (hlObjParent == nullptr) {
            Error(_W("QObject valid handle expected."));
        }
        if (hlObjParent->getCategory() != QOBJECT_CATEGORY_STR) {
            Error(_W("QObject handle expected."));
        }
        QmlHandleObject* qmlhandleobjparent = (QmlHandleObject*)hlObjParent;
        void* ptr = qmlhandleobjparent->getPointer();
        if (ptr == nullptr) {
            Error(_W("QObject valid handle expected."));
        }
        QObject* qobjParent = (QObject*)ptr;
        if (qobjParent == qobj) {
            Error(_W("QObject parent egals to the child."));
        }
        qobj->setParent(qobjParent);
    } else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CHILDREN_STR)) {
        Error(_W("'children' can not modified."));
    } else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CLASSNAME_STR)) {
        Error(_W("'className' can not modified."));
    } else {
        QVariant propertyValue = qobj->property(wstring_to_utf8(propertyName).c_str());
        if (!propertyValue.isValid()) {
            QVariant v = ArrayOfToQVariant(B);
            qobj->setProperty(wstring_to_utf8(propertyName).c_str(), v);
        } else {
            QVariant::Type qtype = propertyValue.type();
            QVariant v = ArrayOfToQVariant(B, qtype);
            if (v.isValid()) {
                qobj->setProperty(wstring_to_utf8(propertyName).c_str(), v);
            } else {
                Error(_W("QVariant invalid."));
            }
        }
    }
}
//=============================================================================
}
//=============================================================================
