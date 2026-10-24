//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__web_figure_reference__Builtin.hpp"
#include "GOAxis.hpp"
#include "GOHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
ArrayOfVector
__web_figure_reference__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isGraphicsObject() || !argIn[0].isScalar()) {
        Error(L"Nelson:graphics:expectedAxes", L"Axes object expected.");
    }
    const auto handle = static_cast<const nelson_handle*>(argIn[0].dataPointer())[0];
    auto* axis = dynamic_cast<GOAxis*>(findGraphicsObject(handle, false));
    if (axis == nullptr) {
        Error(L"Nelson:graphics:expectedAxes", L"Axes object expected.");
    }
    ArrayOfVector result;
    result << axis->navigationReferenceView(argIn[1].asLogicalScalar());
    return result;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
