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
#include "graphic_object_displayBuiltin.hpp"
#include "GraphicObject.hpp"
#include "GraphicObjectDisplay.hpp"
#include "GOFigure.hpp"
#include "DisplayVariableHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::graphic_object_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf paramGo = argIn[0];
    if (paramGo.getDataClass() != NLS_GO_HANDLE) {
        Error(_W("graphic_object expected."));
    }
    Dimensions dims = paramGo.getDimensions();
    auto* ptrGO = (nelson_handle*)paramGo.getDataPointer();
    std::wstring name;
    if (argIn.size() == 2) {
        name = argIn[1].getContentAsWideString();
    }
    Interface* io = eval->getInterface();
    DisplayVariableHeader(io, argIn[0], name);
    graphicObjectDisplay(io, dims, ptrGO);
    DisplayVariableFooter(io, argIn[0], name);
    return retval;
}
//=============================================================================
