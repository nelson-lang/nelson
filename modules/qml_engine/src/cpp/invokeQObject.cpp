//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtQml/QQmlComponent>
#include <QtCore/QVariant>
#include <QtCore/QGenericArgument>
#include <QtCore/QMetaMethod>
#include <QtCore/QObject>
#include "invokeQObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleManager.hpp"
#include "QVariantArrayOf.hpp"
#include "QObjectHandleObject.hpp"
#include "characters_encoding.hpp"
#include "PredefinedErrorMessages.hpp"
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
        raiseError(L"Nelson:qml_engine:ERROR_ONLY_10_INPUT_PARAMETERS_EXPECTED",
            ERROR_ONLY_10_INPUT_PARAMETERS_EXPECTED);
    }
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != NLS_HANDLE_QOBJECT_CATEGORY_STR) {
        raiseError2(
            L"nelson:validators:mustBeType", 1, utf8_to_wstring(NLS_HANDLE_QOBJECT_CATEGORY_STR));
    }
    QObjectHandleObject* qmlhandleobj = (QObjectHandleObject*)hlObj;
    void* ptr = qmlhandleobj->getPointer();
    if (ptr == nullptr) {
        raiseError2(
            L"nelson:validators:mustBeType", 1, utf8_to_wstring(NLS_HANDLE_QOBJECT_CATEGORY_STR));
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
            arg[k] = QGenericArgument("QVariant", static_cast<void*>(qparams[k].data()));
        }
        bool ok;
        if (metaMethodFound.returnType() == QMetaType::Void) {
            ok = metaMethodFound.invoke(qobj, Qt::DirectConnection, arg[0], arg[1], arg[2], arg[3],
                arg[4], arg[5], arg[6], arg[7], arg[8], arg[9]);
            haveReturnValue = false;
        } else {
            QVariant result;
            ok = metaMethodFound.invoke(qobj, Qt::DirectConnection,
                QGenericReturnArgument("QVariant", static_cast<void*>(result.data())), arg[0],
                arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9]);
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
            raiseError(L"Nelson:qml_engine:ERROR_INVALID_PARAMETERS", ERROR_INVALID_PARAMETERS);
        }
    } else {
        raiseError2(L"nelson:runtime:methodNotFound", wmethodname);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
