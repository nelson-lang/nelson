//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__web_figure_scene__Builtin.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "WebFigureBridgeExports.hpp"
#include "FileSystemWrapper.hpp"
#include <filesystem>
#include <fstream>
#include <string>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static void
appendSceneChunk(void* ctx, const char* data, int length)
{
    if (ctx == nullptr || data == nullptr || length <= 0) {
        return;
    }
    static_cast<std::string*>(ctx)->append(data, static_cast<size_t>(length));
}
//=============================================================================
// Hidden engine primitive behind nelson.graphics.internal.saveFigureScene. It
// writes a figure web scene as a self-contained JSON document so a replayer with
// no live engine can render it.
ArrayOfVector
__web_figure_scene__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 0);

    if (!argIn[0].isScalar() || (!argIn[0].isGraphicsObject() && !argIn[0].isNumeric())) {
        Error(L"Nelson:graphics:expectedFigure:figureExpected", L"Figure object expected.");
    }
    const nelson_handle graphicsHandle = argIn[0].isGraphicsObject()
        ? static_cast<const nelson_handle*>(argIn[0].dataPointer())[0]
        : static_cast<nelson_handle>(argIn[0].asDoubleScalar());
    if (isDeletedGraphicsObject(graphicsHandle)) {
        Error(L"Nelson:graphics:invalidGraphicsObject", L"Valid graphics object expected.");
    }
    if (graphicsHandle == HANDLE_ROOT_OBJECT || graphicsHandle >= HANDLE_OFFSET_OBJECT) {
        Error(L"Nelson:graphics:expectedFigure:figureExpected", L"Figure object expected.");
    }

    const std::wstring filename = argIn[1].asWideString();
    const FileSystemWrapper::Path path(filename);
    if (!path.parent_path().empty() && !path.parent_path().is_directory()) {
        Error(L"Nelson:graphics:parentDirectoryDoesNotExist", L"Parent directory does not exist: %s",
            path.parent_path().generic_wstring());
    }

    std::string json;
    if (!nlsWebGuiRenderFigureScene(
            static_cast<int64_t>(graphicsHandle), appendSceneChunk, &json)) {
        Error(L"Nelson:graphics:sceneExportFailed", L"Cannot export figure scene.");
    }

    std::ofstream out(std::filesystem::path(filename), std::ios::binary | std::ios::trunc);
    if (!out.is_open()) {
        Error(L"Nelson:graphics:cannotWriteFile", L"Cannot write file: %s", filename);
    }
    out.write(json.data(), static_cast<std::streamsize>(json.size()));
    out.close();
    if (!out || !path.is_regular_file()) {
        Error(L"Nelson:graphics:cannotWriteFile", L"Cannot write file: %s", filename);
    }
    return {};
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
