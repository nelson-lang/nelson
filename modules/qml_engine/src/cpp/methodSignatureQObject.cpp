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
#include "methodSignatureQObject.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "QmlHandleObject.hpp"
#include "characters_encoding.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
methodSignatureQObject(
    QmlHandleObject* qmlhandleobj, const std::wstring& methodName, std::wstring& signature)
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
methodSignatureQObject(ArrayOf A, const std::wstring& methodName)
{
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    QmlHandleObject* qmlhandleobj = (QmlHandleObject*)hlObj;
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
