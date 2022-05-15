//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "methodsQObject.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "QObjectHandleObject.hpp"
#include "ToCellString.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
void
methodsQObject(QObjectHandleObject* qmlhandleobj, stringVector& methods)
{
    void* ptr = qmlhandleobj->getPointer();
    methods.clear();
    if (ptr == nullptr) {
        Error(_W("QObject valid handle expected."));
    }
    QObject* qobj = (QObject*)ptr;
    const QMetaObject* metaObject = qobj->metaObject();
    for (int i = 0; i < metaObject->methodCount(); i++) {
        QMetaMethod metaMethod = metaObject->method(i);
        QMetaMethod::MethodType methodType = metaMethod.methodType();
        QByteArray name = metaMethod.name();
        std::string str = std::string(name);
        if (methodType == QMetaMethod::Method || methodType == QMetaMethod::Slot) {
            if (std::find(methods.begin(), methods.end(), str) == methods.end()) {
                methods.push_back(str);
            }
        }
    }
    if (!methods.empty()) {
        std::sort(methods.begin(), methods.end());
    }
}
//=============================================================================
ArrayOf
methodsQObject(const ArrayOf& A)
{
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    QObjectHandleObject* qmlhandleobj = (QObjectHandleObject*)hlObj;
    stringVector methodsName;
    methodsQObject(qmlhandleobj, methodsName);
    return ToCellStringAsColumn(methodsName);
}
//=============================================================================
}
//=============================================================================
