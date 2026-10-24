//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SetQObjectHandleObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
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
#include "ForceWindowsTitleBarToDark.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
SetQObjectHandleObject(const ArrayOf& A, const std::wstring& propertyName, const ArrayOf& B)
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
        HandleGenericObject* hlObjParent = B.asHandleScalar();
        if (hlObjParent == nullptr) {
            Error(L"Nelson:QObject:invalidHandle", L"QObject valid handle expected.");
        } else {
            if (hlObjParent->category() != NLS_HANDLE_QOBJECT_CATEGORY_STR) {
                Error(L"Nelson:QObject:notHandle", L"QObject handle expected.");
            }
        }
        QObjectHandleObject* qmlhandleobjparent = (QObjectHandleObject*)hlObjParent;
        void* ptr = qmlhandleobjparent->pointer();
        if (ptr == nullptr) {
            Error(L"Nelson:QObject:invalidHandle", L"QObject valid handle expected.");
        }
        QObject* qobjParent = (QObject*)ptr;
        if (qobjParent == qobj) {
            Error(L"Nelson:QObject:invalidParent", L"QObject parent equals to the child.");
        }
        qobj->setParent(qobjParent);
    } else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CHILDREN_STR)) {
        Error(L"Nelson:QObject:readOnlyProperty", L"'%s' can not modified.", L"children");
    } else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CLASSNAME_STR)) {
        Error(L"Nelson:QObject:readOnlyProperty", L"'%s' can not modified.", L"className");
    } else {
        std::string propertyNameUtf8 = wstring_to_utf8(propertyName);
        QVariant propertyValue = qobj->property(propertyNameUtf8.c_str());
        if (!propertyValue.isValid()) {
            QVariant v = ArrayOfToQVariant(B);
            qobj->setProperty(propertyNameUtf8.c_str(), v);
            if (qobj->property(propertyNameUtf8.c_str()).isValid()) {
                qmlhandleobj->propertyChanged(propertyName);
            }
        } else {
            QMetaType metatype = propertyValue.metaType();
            int id = metatype.id();
            QVariant v = ArrayOfToQVariant(B, id);
            if (v.isValid()) {
                qobj->setProperty(propertyNameUtf8.c_str(), v);
                if (qobj->property(propertyNameUtf8.c_str()).isValid()) {
                    qmlhandleobj->propertyChanged(propertyName);
                }
#ifdef _MSC_VER
                if (qobj->isWindowType() && propertyName == L"visible") {
                    QWindow* qWindow = qobject_cast<QWindow*>(qobj);
                    forceWindowsTitleBarToDark(qWindow->winId());
                }
#endif
            } else {
                Error(L"Nelson:QObject:invalidValue", L"QVariant invalid.");
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
