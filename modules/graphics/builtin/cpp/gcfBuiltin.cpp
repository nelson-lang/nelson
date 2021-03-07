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
#include <cmath>
#include "NewFigure.hpp"
#include "gcfBuiltin.hpp"
#include "Error.hpp"
#include "GOWindowManager.hpp"
#include "GOFigure.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::gcfBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf res;
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    uint64 currentWindowsID = getCurrentGOWindowID();
    if (currentWindowsID == 0) {
        GOFigure* goPtr = Nelson::newFigure();
        res = ArrayOf::graphicObjectConstructor(goPtr);
    } else {
        GOFigure* goPtr = Nelson::newFigure(currentWindowsID);
        res = ArrayOf::graphicObjectConstructor(goPtr);
    }
    ArrayOfVector retval(1);
    retval << res;
    return retval;
}
//=============================================================================
