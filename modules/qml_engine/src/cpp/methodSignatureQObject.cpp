//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "methodSignatureQObject.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "QObjectHandleObject.hpp"
#include "characters_encoding.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
methodSignatureQObject(
    QObjectHandleObject* qmlhandleobj, const std::wstring& methodName, std::wstring& signature)
{
    void* ptr = qmlhandleobj->getPointer();
    if (ptr == nullptr) {
        Error(_W("QObject valid handle expected."));
    }
    std::string umethodName = wstring_to_utf8(methodName);
    QObject* qobj = (QObject*)ptr;
    const QMetaObject* metaObject = qobj->metaObject();
    for (int i = 0; i < metaObject->methodCount(); i++) {
        QMetaMethod metaMethod = metaObject->method(i);
        QMetaMethod::MethodType methodType = metaMethod.methodType();
        QByteArray name = metaMethod.name();
        std::string str = std::string(name);
        if (methodType == QMetaMethod::Method || methodType == QMetaMethod::Slot) {
            if (umethodName == str) {
                signature = utf8_to_wstring(std::string(metaMethod.methodSignature()));
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
ArrayOf
methodSignatureQObject(const ArrayOf& A, const std::wstring& methodName)
{
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    QObjectHandleObject* qmlhandleobj = (QObjectHandleObject*)hlObj;
    std::wstring signature;
    bool res = methodSignatureQObject(qmlhandleobj, methodName, signature);
    if (res == false) {
        Error(_W("method not found."));
    }
    return ArrayOf::characterArrayConstructor(signature);
}
//=============================================================================
}
//=============================================================================
