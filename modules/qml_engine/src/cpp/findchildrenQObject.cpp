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
#include "findchildrenQObject.hpp"
#include "HandleManager.hpp"
#include "QStringConverter.hpp"
#include "QmlHandleObject.hpp"
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
findchildrenQObject(ArrayOf H, std::wstring fieldname, bool bRecursively)
{
    ArrayOf res = ArrayOf::emptyConstructor(0, 0);
    res.promoteType(NLS_HANDLE);
    HandleGenericObject* hlObj = H.getContentAsHandleScalar();
    if (hlObj->getCategory() != QOBJECT_CATEGORY_STR) {
        Error(_W("QObject handle expected."));
    }
    QmlHandleObject* qmlhandleobj = (QmlHandleObject*)hlObj;
    void* ptr = qmlhandleobj->getPointer();
    if (ptr == nullptr) {
        Error(_W("QObject valid handle expected."));
    }
    QObject* qobj = (QObject*)ptr;
    Qt::FindChildOption option = Qt::FindDirectChildrenOnly;
    if (bRecursively) {
        option = Qt::FindChildrenRecursively;
    }
    QList<QObject*> qobjfound = qobj->findChildren<QObject*>(wstringToQString(fieldname), option);
    if (qobjfound.size() > 0) {
        Dimensions dims(1, qobjfound.size());
        nelson_handle* nh = (nelson_handle*)ArrayOf::allocateArrayOf(NLS_HANDLE, qobjfound.size());
        for (int k = 0; k < qobjfound.size(); k++) {
            nelson_handle nh_found = HandleManager::getInstance()->findByPointerValue(qobjfound[k]);
            if (nh_found != -1) {
                nh[k] = nh_found;
            } else {
                QmlHandleObject* qmlHandle = nullptr;
                try {
                    qmlHandle = new QmlHandleObject(qobjfound[k]);
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
    return res;
}
//=============================================================================
}
//=============================================================================
