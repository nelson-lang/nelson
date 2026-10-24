//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "addpointsBuiltin.hpp"
#include "clearpointsBuiltin.hpp"
#include "getpointsBuiltin.hpp"
#include "GOAnimatedLine.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static GOAnimatedLine*
animatedLineFromArray(const ArrayOf& arg)
{
    if (!arg.isGraphicsObject() || !arg.isScalar()) {
        Error(L"Nelson:animatedline:expectedScalar",
            L"Expected a scalar animatedline graphics object.");
    }
    int64 handle = arg.asGraphicsObjectScalar();
    GraphicsObject* object = graphicsObjectFromHandle(handle, true);
    GOAnimatedLine* animatedLine = dynamic_cast<GOAnimatedLine*>(object);
    if (!animatedLine) {
        Error(L"Nelson:animatedline:expectedAnimatedLine",
            L"Expected an animatedline graphics object.");
    }
    return animatedLine;
}
//=============================================================================
static std::vector<double>
numericVectorFromArray(const ArrayOf& arg)
{
    if (!arg.isNumeric() || arg.isComplex()) {
        Error(L"Nelson:animatedline:invalidCoordinates", L"Expected real numeric coordinates.");
    }
    return arg.asDoubleVector();
}
//=============================================================================
static void
refreshParentFigure(GOAnimatedLine* animatedLine)
{
    GOFigure* fig = animatedLine ? animatedLine->getParentFigure() : nullptr;
    if (!fig) {
        return;
    }
    fig->setRenderingStateInvalid(true);
    if (!fig->isDrawLater()) {
        fig->repaint();
    }
}
//=============================================================================
ArrayOfVector
addpointsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 0);
    GOAnimatedLine* animatedLine = animatedLineFromArray(argIn[0]);
    std::vector<double> x = numericVectorFromArray(argIn[1]);
    std::vector<double> y = numericVectorFromArray(argIn[2]);
    std::vector<double> z;
    if (argIn.size() == 4) {
        z = numericVectorFromArray(argIn[3]);
    } else {
        z.assign(x.size(), 0.0);
    }
    animatedLine->addPoints(x, y, z);
    animatedLine->updateState();
    refreshParentFigure(animatedLine);
    return {};
}
//=============================================================================
ArrayOfVector
clearpointsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    GOAnimatedLine* animatedLine = animatedLineFromArray(argIn[0]);
    animatedLine->clearPoints();
    animatedLine->updateState();
    refreshParentFigure(animatedLine);
    return {};
}
//=============================================================================
ArrayOfVector
getpointsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 3);
    GOAnimatedLine* animatedLine = animatedLineFromArray(argIn[0]);
    ArrayOfVector retval;
    retval << ArrayOf::doubleRowVector(animatedLine->xData());
    if (nLhs >= 2) {
        retval << ArrayOf::doubleRowVector(animatedLine->yData());
    }
    if (nLhs >= 3) {
        retval << ArrayOf::doubleRowVector(animatedLine->zData());
    }
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
