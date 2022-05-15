//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "invokeQObject.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "QVariantArrayOf.hpp"
#include "QObjectHandleObject.hpp"
#include "characters_encoding.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
#define NB_PARAMS_MAX 10
//=============================================================================
ArrayOf
invokeQObject(const ArrayOf& A, const std::wstring& wmethodname, const ArrayOfVector& params,
    bool& haveReturnValue)
{
    ArrayOf res;
    haveReturnValue = false;
    if (params.size() > NB_PARAMS_MAX) {
        Error(_W("Only 10 input parameters expected."));
    }
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
    const QMetaObject* metaObject = qobj->metaObject();
    std::string methodname = wstring_to_utf8(wmethodname);
    QMetaMethod metaMethodFound;
    bool methodFound = false;
    for (int i = 0; i < metaObject->methodCount(); i++) {
        QMetaMethod metaMethod = metaObject->method(i);
        QMetaMethod::MethodType methodType = metaMethod.methodType();
        QByteArray name = metaMethod.name();
        std::string str = std::string(name);
        if (methodType == QMetaMethod::Method || methodType == QMetaMethod::Slot) {
            if (str == methodname) {
                metaMethodFound = metaMethod;
                methodFound = true;
                break;
            }
        }
    }
    if (methodFound) {
        QVariant qparams[NB_PARAMS_MAX];
        QGenericArgument arg[NB_PARAMS_MAX];
        for (size_t k = 0; k < params.size(); k++) {
            qparams[k] = ArrayOfToQVariant(params[k]);
            arg[k] = Q_ARG(QVariant, qparams[k]);
        }
        bool ok;
        if (metaMethodFound.returnType() == QMetaType::Void) {
            ok = metaMethodFound.invoke(qobj, Qt::DirectConnection, arg[0], arg[1], arg[2], arg[3],
                arg[4], arg[5], arg[6], arg[7], arg[8], arg[9]);
            haveReturnValue = false;
        } else {
            QVariant result;
            ok = metaMethodFound.invoke(qobj, Qt::DirectConnection, Q_RETURN_ARG(QVariant, result),
                arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9]);
            haveReturnValue = true;
            if (ok) {
                if (!result.isValid()) {
                    haveReturnValue = false;
                } else {
                    res = QVariantToArrayOf(result);
                }
            }
        }
        if (!ok) {
            Error(_W("Invalid parameters"));
        }
    } else {
        Error(_W("method not found."));
    }
    return res;
}
//=============================================================================
}
//=============================================================================
