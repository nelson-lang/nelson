//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__web_figure_size__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "WebFigureBridgeExports.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::__web_figure_size__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isScalar() || (!argIn[0].isGraphicsObject() && !argIn[0].isNumeric())) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectOrWebHandle",
            L"Expected graphics object or web graphics handle.");
    }
    const nelson_handle handle = argIn[0].isGraphicsObject()
        ? static_cast<const nelson_handle*>(argIn[0].dataPointer())[0]
        : static_cast<nelson_handle>(argIn[0].asDoubleScalar());
    if (isDeletedGraphicsObject(handle) || handle == HANDLE_ROOT_OBJECT
        || handle >= HANDLE_OFFSET_OBJECT) {
        Error(L"Nelson:graphics:expectedFigure:figureExpected", L"Figure object expected.");
    }
    const int width = static_cast<int>(argIn[1].asDoubleScalar());
    const int height = static_cast<int>(argIn[2].asDoubleScalar());
    ArrayOfVector result;
    result << ArrayOf::logicalScalar(nlsWebGuiSetFigureSize(handle, width, height));
    return result;
}
//=============================================================================
