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
#include "iswidgettypeQObject.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "QmlHandleObject.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
iswidgettypeQObject(ArrayOf A)
{
    if (!A.isHandle()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
    }
    std::wstring className;
    ClassName(A, className);
    if (className != QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    ArrayOf res;
    Dimensions dimsA = A.getDimensions();
    nelson_handle* qp = (nelson_handle*)A.getDataPointer();
    if (qp) {
        logical* resArray
            = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount());
        for (size_t k = 0; k < dimsA.getElementCount(); k++) {
            nelson_handle hl = qp[k];
            HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
            if (hlObj != nullptr) {
                if (hlObj->getPointer()) {
                    QmlHandleObject* qmlHandle = (QmlHandleObject*)hlObj;
                    void* ptr = qmlHandle->getPointer();
                    if (ptr == nullptr) {
                        resArray[k] = false;
                    } else {
                        QObject* qobj = (QObject*)ptr;
                        resArray[k] = qobj->isWidgetType();
                    }
                } else {
                    resArray[k] = false;
                }
            } else {
                resArray[k] = false;
            }
        }
        res = ArrayOf(NLS_LOGICAL, dimsA, resArray);
    } else {
        res = ArrayOf::emptyConstructor(dimsA);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
