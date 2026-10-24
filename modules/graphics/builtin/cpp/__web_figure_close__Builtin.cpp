//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__web_figure_close__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "WebFigureBridgeExports.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::__web_figure_close__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isNumeric() || !argIn[0].isScalar()) {
        Error(L"Nelson:graphics:webFigureHandleExpected", L"Web graphics handle expected.");
    }
    const auto handle = static_cast<int64_t>(argIn[0].asDoubleScalar());
    ArrayOfVector result;
    result << ArrayOf::logicalScalar(nlsWebGuiCloseFigure(handle));
    return result;
}
//=============================================================================
