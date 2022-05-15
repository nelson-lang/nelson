//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SetQObjectHandleObject.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "QStringConverter.hpp"
#include "QVariantArrayOf.hpp"
#include "QObjectHandleObject.hpp"
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
SetQObjectHandleObject(const ArrayOf& A, const std::wstring& propertyName, const ArrayOf& B)
{
    ArrayOf res;
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    QObjectHandleObject* qmlhandleobj = (QObjectHandleObject*)hlObj;
    void* ptr = qmlhandleobj->getPointer();
    if (ptr == nullptr) {
        Error(_W("QObject valid handle expected."));
    }
    QObject* qobj = (QObject*)ptr;
    if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_PARENT_STR)) {
        HandleGenericObject* hlObjParent = B.getContentAsHandleScalar();
        if (hlObjParent == nullptr) {
            Error(_W("QObject valid handle expected."));
        } else {
            if (hlObjParent->getCategory() != QOBJECT_CATEGORY_STR) {
                Error(_W("QObject handle expected."));
            }
        }
        QObjectHandleObject* qmlhandleobjparent = (QObjectHandleObject*)hlObjParent;
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
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
            QMetaType metatype = propertyValue.metaType();
            int id = metatype.id();
#else
            int id = QMetaType::type(propertyValue.typeName());

#endif
            QVariant v = ArrayOfToQVariant(B, id);
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
