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
#include "methodsQObject.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "QmlHandleObject.hpp"
#include "ToCellString.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
void
methodsQObject(QmlHandleObject* qmlhandleobj, stringVector& methods)
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
    std::sort(methods.begin(), methods.end());
}
//=============================================================================
ArrayOf
methodsQObject(ArrayOf A)
{
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    QmlHandleObject* qmlhandleobj = (QmlHandleObject*)hlObj;
    stringVector methodsName;
    methodsQObject(qmlhandleobj, methodsName);
    return ToCellStringAsColumn(methodsName);
}
//=============================================================================
}
//=============================================================================
