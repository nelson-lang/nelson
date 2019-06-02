//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "GraphicObjectDelete.hpp"
#include "GOWindowManager.hpp"
#include "GOFigure.hpp"
#include "GOWindow.hpp"
#include "GORoot.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
graphicObjectDelete(GraphicObject* goPtr)
{
    if (goPtr->isType(ROOT_TYPE_STR)) {
        return false;
    }
    if (goPtr->isType(FIGURE_TYPE_STR) && (goPtr->referenceCount() != 0)) {
        auto* goFigPtr = (GOFigure*)goPtr;
        GOWindow* goWinPtr = goFigPtr->getParentWindow();
        if (goWinPtr != nullptr) {
            removeGOWindow(goWinPtr->ID());
            goWinPtr->hide();
            delete goWinPtr;
        }
        goPtr->dereference();
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
