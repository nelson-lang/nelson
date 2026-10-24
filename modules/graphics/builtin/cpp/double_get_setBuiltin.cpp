//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "double_get_setBuiltin.hpp"
#include "graphics_object_getBuiltin.hpp"
#include "graphics_object_setBuiltin.hpp"
#include "GOHelpers.hpp"
#include "GOFiguresManager.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <vector>
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
// Numeric graphics handles: 0 is the root object (groot) and a positive value N
// is the figure numbered N. get(0, 'ScreenSize') relies on it.
static ArrayOf
numericHandlesToGraphicsObjects(const ArrayOf& arg)
{
    ArrayOf converted(arg);
    converted.promoteType(NLS_INT64);
    const int64* values = static_cast<const int64*>(converted.dataPointer());
    const size_t count = arg.elementCount();
    std::vector<int64> handles;
    handles.reserve(count);
    for (size_t k = 0; k < count; ++k) {
        const int64 value = values[k];
        if (value == 0) {
            handles.push_back(HANDLE_ROOT_OBJECT);
            continue;
        }
        // Figures are numbered from 1 for the user but stored from 0.
        if (value > 0 && getHandleWindow(value - 1) != nullptr) {
            handles.push_back(value - 1);
            continue;
        }
        Error(L"Nelson:graphics:invalidHandle", L"Invalid handle.");
    }
    return graphicsObjectVector(handles);
}
//=============================================================================
ArrayOfVector
double_getBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.empty()) {
        Error(L"Nelson:graphics:invalidHandle", L"Invalid handle.");
    }
    ArrayOfVector forwarded(argIn);
    forwarded[0] = numericHandlesToGraphicsObjects(argIn[0]);
    return graphics_object_getBuiltin(eval, nLhs, forwarded);
}
//=============================================================================
ArrayOfVector
double_setBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.empty()) {
        Error(L"Nelson:graphics:invalidHandle", L"Invalid handle.");
    }
    ArrayOfVector forwarded(argIn);
    forwarded[0] = numericHandlesToGraphicsObjects(argIn[0]);
    return graphics_object_setBuiltin(nLhs, forwarded);
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
