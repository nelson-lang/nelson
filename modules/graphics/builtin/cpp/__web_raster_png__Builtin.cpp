//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__web_raster_png__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "WebFigureBridgeExports.hpp"
#include <string>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static void
appendPngChunk(void* context, const char* data, int length)
{
    if (context == nullptr || data == nullptr || length <= 0) {
        return;
    }
    static_cast<std::string*>(context)->append(data, static_cast<size_t>(length));
}
//=============================================================================
ArrayOfVector
__web_raster_png__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    if (!argIn[0].isGraphicsObject()) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObject",
            L"Expected graphics object.");
    }
    const auto* object = static_cast<const nelson_handle*>(argIn[0].dataPointer());
    if (isDeletedGraphicsObject(object[0]) || object[0] == HANDLE_ROOT_OBJECT
        || object[0] >= HANDLE_OFFSET_OBJECT) {
        Error(L"Nelson:graphics:expectedFigure:validFigureExpected",
            L"Valid figure object expected.");
    }

    std::string base64;
    if (!nlsWebGuiRenderFigurePngBase64(static_cast<int64_t>(object[0]), appendPngChunk, &base64)) {
        Error(L"Nelson:graphics:webRasterRenderFailed", L"Cannot render figure as PNG.");
    }

    ArrayOfVector result;
    result << ArrayOf::characterArray(std::wstring(base64.begin(), base64.end()));
    return result;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
