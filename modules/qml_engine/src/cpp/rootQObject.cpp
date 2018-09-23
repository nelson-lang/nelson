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
#include "rootQObject.hpp"
#include "HandleManager.hpp"
#include "MainGuiObject.hpp"
#include "QmlHandleObject.hpp"
#include <QtGui/QWindow>
#include <QtQml/QQmlComponent>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
rootQObject()
{
    ArrayOf res;
    QWindow* parent = (QWindow*)GetMainGuiObject();
    if (parent) {
        QmlHandleObject* qmlHandle = nullptr;
        try {
            qmlHandle = new QmlHandleObject(parent);
        } catch (const std::bad_alloc& e) {
            e.what();
            qmlHandle = nullptr;
            Error(ERROR_MEMORY_ALLOCATION);
        }
        res = ArrayOf::handleConstructor(qmlHandle);
    } else {
        res = ArrayOf::emptyConstructor(0, 0);
        res.promoteType(NLS_HANDLE);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
