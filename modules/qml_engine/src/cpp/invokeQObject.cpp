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
#include "invokeQObject.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "QVariantArrayOf.hpp"
#include "QmlHandleObject.hpp"
#include "characters_encoding.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
#define NB_PARAMS_MAX 10
//=============================================================================
ArrayOf
invokeQObject(ArrayOf A, std::wstring wmethodname, ArrayOfVector params, bool& haveReturnValue)
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
    QmlHandleObject* qmlhandleobj = (QmlHandleObject*)hlObj;
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
