//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include <QtQml/QQmlComponent>
#include <QtGui/QWindow>
#include "rootQObject.hpp"
#include "HandleManager.hpp"
#include "QmlHandleObject.hpp"
#include "MainGuiObject.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf rootQObject()
    {
        QWindow * parent = (QWindow *)GetMainGuiObject();
        if (parent)
        {
            QmlHandleObject * qmlHandle = nullptr;
            try
            {
                qmlHandle = new QmlHandleObject(parent);
            }
            catch (std::bad_alloc &e)
            {
                e.what();
                qmlHandle = nullptr;
                throw Exception(ERROR_MEMORY_ALLOCATION);
            }
            return ArrayOf::handleConstructor(qmlHandle);
        }
        return ArrayOf::handleConstructor(nullptr);
    }
    //=============================================================================
}
//=============================================================================
