//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__web_display_list__Builtin.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "WebFigureBridgeExports.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static void
appendDisplayListChunk(void* ctx, const char* data, int length)
{
    if (ctx == nullptr || data == nullptr || length <= 0) {
        return;
    }
    auto* text = static_cast<std::string*>(ctx);
    text->append(data, static_cast<size_t>(length));
}
//=============================================================================
static int64_t
scalarGraphicsHandleArgument(const ArrayOf& value)
{
    if (!value.isScalar() || (!value.isGraphicsObject() && !value.isNumeric())) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectOrWebHandle",
            L"Expected graphics object or web graphics handle.");
    }
    const nelson_handle handle = value.isGraphicsObject()
        ? static_cast<const nelson_handle*>(value.dataPointer())[0]
        : static_cast<nelson_handle>(value.asDoubleScalar());
    return static_cast<int64_t>(handle);
}
//=============================================================================
ArrayOfVector
__web_display_list__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 6);
    nargoutcheck(nLhs, 0, 1);

    if (argIn.size() != 1 && argIn.size() != 2 && argIn.size() != 4 && argIn.size() != 5
        && argIn.size() != 6) {
        Error(L"Nelson:graphics:webDisplayListInvalidArguments",
            L"One, two, four, five or six input arguments expected.");
    }

    const nelson_handle graphicsHandle
        = static_cast<nelson_handle>(scalarGraphicsHandleArgument(argIn[0]));
    if (isDeletedGraphicsObject(graphicsHandle)) {
        Error(L"Nelson:graphics:invalidGraphicsObject", L"Valid graphics object expected.");
    }
    if (graphicsHandle == HANDLE_ROOT_OBJECT || graphicsHandle >= HANDLE_OFFSET_OBJECT) {
        Error(L"Nelson:graphics:expectedFigure:figureExpected", L"Figure object expected.");
    }

    const int64_t handle = static_cast<int64_t>(graphicsHandle);
    bool compactView = false;
    bool softwareFallback = true;
    // The trailing logical controls the software fallback raster: the two-argument
    // form controls it for a full render, the six-argument form for a view change.
    // A WebGL-capable client passes false so the figure is not rasterised twice.
    if (argIn.size() == 2) {
        if (!argIn[1].isLogical()) {
            Error(L"Nelson:graphics:webDisplayListExpectedLogical", L"Logical scalar expected.");
        }
        softwareFallback = argIn[1].asLogicalScalar();
    }
    if (argIn.size() == 6) {
        if (!argIn[5].isLogical()) {
            Error(L"Nelson:graphics:webDisplayListExpectedLogical", L"Logical scalar expected.");
        }
        softwareFallback = argIn[5].asLogicalScalar();
    }
    if (argIn.size() >= 4) {
        const int viewArgOffset = argIn.size() >= 5 ? 1 : 0;
        const int64_t axesHandle = argIn.size() >= 5 ? scalarGraphicsHandleArgument(argIn[1]) : 0;
        const double azimuth = argIn[1 + viewArgOffset].asDoubleScalar();
        const double elevation = argIn[2 + viewArgOffset].asDoubleScalar();
        if (!argIn[3 + viewArgOffset].isLogical()) {
            Error(L"Nelson:graphics:webDisplayListExpectedLogical", L"Logical scalar expected.");
        }
        compactView = argIn[3 + viewArgOffset].asLogicalScalar();
        if (!nlsWebGuiSetAxesView(handle, axesHandle, azimuth, elevation)) {
            Error(L"Nelson:graphics:webDisplayListViewFailed", L"Cannot update figure view.");
        }
        if (compactView) {
            nlsWebGuiMarkFigureViewOnlyFrame(handle);
        }
        if (nLhs == 0) {
            return {};
        }
    }

    std::string json;
    const bool rendered = compactView
        ? nlsWebGuiRenderFigureAnimationFrame(handle, appendDisplayListChunk, &json)
        : softwareFallback
        ? nlsWebGuiRenderFigureDisplayList(handle, appendDisplayListChunk, &json)
        : nlsWebGuiRenderFigureDisplayListNoFallback(handle, appendDisplayListChunk, &json);
    if (!rendered) {
        Error(L"Nelson:graphics:webDisplayListRenderFailed", L"Cannot render web display list.");
    }

    ArrayOfVector retval;
    retval << ArrayOf::characterArray(utf8_to_wstring(json));
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
