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
#include <math.h>
#include "NewFigure.hpp"
#include "figureBuiltin.hpp"
#include "Error.hpp"
#include "GOWindowManager.hpp"
#include "GOFigure.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::figureBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf res;
    switch (argIn.size()) {
    case 0: {
        GOFigure* goPtr = Nelson::newFigure();
        res = ArrayOf::graphicObjectConstructor(goPtr);
    } break;
    case 1: {
        ArrayOf param1 = argIn[0];
        if (!param1.isScalar()) {
            Error(_W("Scalar value expected."));
        }
        if (param1.isNumeric()) {
            double tmp;
            double dvalue = param1.getContentAsDoubleScalar();
            if (std::modf(dvalue, &tmp)) {
                Error(_W("Integer value expected."));
            }
            if (dvalue <= 0) {
                Error(_W("Integer positive value expected."));
            }
            uint64 ivalue = (uint64)dvalue;
            GOFigure* goPtr = Nelson::newFigure(ivalue);
            res = ArrayOf::graphicObjectConstructor(goPtr);
        } else if (param1.isGraphicObject()) {
            GraphicObject* go = (GraphicObject*)param1.getContentAsGraphicObjectScalar();
            if (!go || (go->referenceCount() == 0) || (go->getType() != FIGURE_TYPE_STR)) {
                Error(_W("An valid figure object expected."));
            }
            GOFigure* goFig = (GOFigure*)go;
            GOFigure* goPtr = Nelson::newFigure(goFig);
            res = ArrayOf::graphicObjectConstructor(goPtr);
        }
    } break;
    default: { } break; }
    ArrayOfVector retval;
    retval.push_back(res);
    return retval;
}
//=============================================================================
