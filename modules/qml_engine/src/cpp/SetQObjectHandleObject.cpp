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
#include "PredefinedErrorMessages.hpp"
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
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != NLS_HANDLE_QOBJECT_CATEGORY_STR) {
        raiseError(
            L"Nelson:qml_engine:ERROR_QOBJECT_HANDLE_EXPECTED", ERROR_QOBJECT_HANDLE_EXPECTED);
    }
    QObjectHandleObject* qmlhandleobj = (QObjectHandleObject*)hlObj;
    void* ptr = qmlhandleobj->getPointer();
    if (ptr == nullptr) {
        raiseError(L"Nelson:qml_engine:ERROR_QOBJECT_VALID_HANDLE_EXPECTED",
            ERROR_QOBJECT_VALID_HANDLE_EXPECTED);
    }
    QObject* qobj = (QObject*)ptr;
    if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_PARENT_STR)) {
        HandleGenericObject* hlObjParent = B.getContentAsHandleScalar();
        if (hlObjParent == nullptr) {
            raiseError(L"Nelson:qml_engine:ERROR_QOBJECT_VALID_HANDLE_EXPECTED",
                ERROR_QOBJECT_VALID_HANDLE_EXPECTED);
        } else {
            if (hlObjParent->getCategory() != NLS_HANDLE_QOBJECT_CATEGORY_STR) {
                raiseError(L"Nelson:qml_engine:ERROR_QOBJECT_HANDLE_EXPECTED",
                    ERROR_QOBJECT_HANDLE_EXPECTED);
            }
        }
        QObjectHandleObject* qmlhandleobjparent = (QObjectHandleObject*)hlObjParent;
        void* ptr = qmlhandleobjparent->getPointer();
        if (ptr == nullptr) {
            raiseError(L"Nelson:qml_engine:ERROR_QOBJECT_VALID_HANDLE_EXPECTED",
                ERROR_QOBJECT_VALID_HANDLE_EXPECTED);
        }
        QObject* qobjParent = (QObject*)ptr;
        if (qobjParent == qobj) {
            raiseError(L"Nelson:qml_engine:ERROR_QOBJECT_PARENT_EQUALS_CHILD",
                ERROR_QOBJECT_PARENT_EQUALS_CHILD);
        }
        qobj->setParent(qobjParent);
    } else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CHILDREN_STR)) {
        raiseError(L"Nelson:qml_engine:ERROR_CHILDREN_CANNOT_BE_MODIFIED",
            ERROR_CHILDREN_CANNOT_BE_MODIFIED);
    } else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CLASSNAME_STR)) {
        raiseError(L"Nelson:qml_engine:ERROR_CLASSNAME_CANNOT_BE_MODIFIED",
            ERROR_CLASSNAME_CANNOT_BE_MODIFIED);
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
#ifdef _MSC_VER
                if (qobj->isWindowType() && propertyName == L"visible") {
                    QWindow* qWindow = qobject_cast<QWindow*>(qobj);
                    forceWindowsTitleBarToDark(qWindow->winId());
                }
#endif
            } else {
                raiseError(L"Nelson:qml_engine:ERROR_QVARIANT_INVALID", ERROR_QVARIANT_INVALID);
            }
        }
    }
}
//=============================================================================
}
//=============================================================================
