//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ismethodQObject.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "QObjectHandleObject.hpp"
#include "characters_encoding.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ismethodQObject(QObjectHandleObject* qmlhandleobj, const std::wstring& methodname)
{
    void* ptr = qmlhandleobj->getPointer();
    if (ptr == nullptr) {
        Error(_W("QObject valid handle expected."));
    }
    QObject* qobj = (QObject*)ptr;
    const QMetaObject* metaObject = qobj->metaObject();
    std::string umethodname = wstring_to_utf8(methodname);
    for (int i = 0; i < metaObject->methodCount(); i++) {
        QMetaMethod metaMethod = metaObject->method(i);
        QMetaMethod::MethodType methodType = metaMethod.methodType();
        QByteArray name = metaMethod.name();
        std::string str = std::string(name);
        if (methodType == QMetaMethod::Method || methodType == QMetaMethod::Slot) {
            if (str == umethodname) {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
ArrayOf
ismethodQObject(const ArrayOf& A, const std::wstring& methodname)
{
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    QObjectHandleObject* qmlhandleobj = (QObjectHandleObject*)hlObj;
    bool res = ismethodQObject(qmlhandleobj, methodname);
    return ArrayOf::logicalConstructor(res);
}
//=============================================================================
}
//=============================================================================
