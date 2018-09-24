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
#include "GetQmlHandleObject.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "QVariantArrayOf.hpp"
#include "QmlHandleObject.hpp"
#include "characters_encoding.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
GetQmlHandleObject(ArrayOf A, std::wstring propertyName)
{
    ArrayOf res;
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
    if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_PARENT_STR)) {
        QObject* qparent = qobj->parent();
        if (qparent) {
            nelson_handle nh_found = HandleManager::getInstance()->findByPointerValue(qparent);
            if (nh_found != -1) {
                res = ArrayOf::handleConstructor(nh_found);
            } else {
                QmlHandleObject* qmlHandle = nullptr;
                try {
                    qmlHandle = new QmlHandleObject(qparent);
                } catch (const std::bad_alloc& e) {
                    e.what();
                    qmlHandle = nullptr;
                    Error(ERROR_MEMORY_ALLOCATION);
                }
                res = ArrayOf::handleConstructor(qmlHandle);
            }
        } else {
            Error(_W("No parent."));
        }
    } else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CLASSNAME_STR)) {
        std::string name = std::string(qobj->metaObject()->className());
        res = ArrayOf::characterArrayConstructor(name);
    } else if (propertyName == utf8_to_wstring(QOBJECT_PROPERTY_CHILDREN_STR)) {
        QObjectList childs = qobj->children();
        int nbChilds = childs.size();
        if (nbChilds == 0) {
            Dimensions dims(0, 0);
            res = ArrayOf::emptyConstructor(dims);
            res.promoteType(NLS_HANDLE);
        } else {
            Dimensions dims(1, nbChilds);
            nelson_handle* nh = (nelson_handle*)ArrayOf::allocateArrayOf(NLS_HANDLE, nbChilds);
            for (int k = 0; k < nbChilds; k++) {
                nelson_handle nh_found
                    = HandleManager::getInstance()->findByPointerValue(childs[k]);
                if (nh_found != -1) {
                    nh[k] = nh_found;
                } else {
                    QmlHandleObject* qmlHandle = nullptr;
                    try {
                        qmlHandle = new QmlHandleObject(childs[k]);
                    } catch (const std::bad_alloc& e) {
                        e.what();
                        qmlHandle = nullptr;
                        Error(ERROR_MEMORY_ALLOCATION);
                    }
                    nh[k] = HandleManager::getInstance()->addHandle(qmlHandle);
                }
            }
            res = ArrayOf(NLS_HANDLE, dims, (void*)nh);
        }
    } else {
        QVariant propertyValue = qobj->property(wstring_to_utf8(propertyName).c_str());
        res = QVariantToArrayOf(propertyValue);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
