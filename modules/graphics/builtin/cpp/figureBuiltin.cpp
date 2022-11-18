//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "NewFigure.hpp"
#include "figureBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GOWindowManager.hpp"
#include "GOFigure.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::figureBuiltin(int nLhs, const ArrayOfVector& argIn)
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
    default: {
    } break;
    }
    ArrayOfVector retval(1);
    retval << res;
    return retval;
}
//=============================================================================
