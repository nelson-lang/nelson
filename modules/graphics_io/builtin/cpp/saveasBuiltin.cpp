//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "saveasBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ExportGraphics.hpp"
#include "GOHelpers.hpp"
#include "GOFiguresManager.hpp"
#include "FileSystemWrapper.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
GraphicsIoGateway::saveasBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 0);

    GOFigureHost* f = nullptr;
    if (argIn[0].isGraphicsObject()) {
        int64 handle = argIn[0].asGraphicsObjectScalar();
        f = findGOWindows(handle);
    } else {
        int64 fignum = argIn[0].asInt64Scalar();
        if ((fignum <= 0) || (fignum > MAX_FIGS)) {
            Error("Nelson:saveas:invalidFigure",
                "figure number is out of range - it must be between 1 and 2147483647.");
        }
        f = getHandleWindow(fignum);
    }
    if (!f) {
        Error(L"Nelson:saveas:invalidHandle", L"Invalid handle.");
    }
    std::wstring filename = argIn[1].asWideString();
    FileSystemWrapper::Path p(filename);
    if (!p.parent_path().is_directory()) {
        Error(L"Nelson:saveas:cannotWriteFile", L"Parent directory does not exist: %s",
            p.parent_path().generic_wstring());
    }
    IMAGE_FORMAT formatForced;
    if (p.has_extension()) {
        std::wstring pathExtension = p.extension().wstring();
        pathExtension.erase(0, 1);
        if (argIn.size() == 3) {
            std::wstring param3 = argIn[2].asWideString();
            if (!isSupportedImageFormatExtension(param3)) {
                Error(L"Nelson:saveas:badFormat", L"Unsupported format:%s", param3);
            }
            formatForced = getExportImageFormatFromString(param3);
        } else {
            if (!isSupportedImageFormatExtension(pathExtension)) {
                Error(L"Nelson:saveas:badFormat", L"Unsupported format:%s", pathExtension);
            }
            formatForced = getExportImageFormatFromString(pathExtension);
        }
    } else {
        if (argIn.size() == 3) {
            std::wstring param3 = argIn[2].asWideString();
            if (!isSupportedImageFormatExtension(param3)) {
                Error(L"Nelson:saveas:badFormat", L"Unsupported format:%s", param3);
            }
            formatForced = getExportImageFormatFromString(param3);
        } else {
            formatForced = IMAGE_FORMAT::PNG_EXPORT;
        }
        filename = filename + L"." + getExportImageFormatAsString(formatForced);
        p = filename;
    }
    bool saved = ExportGraphics(f, filename, formatForced);
    if (!saved) {
        Error(L"Nelson:saveas:saveFailed", L"Impossible to save image.");
    }
    if (!p.is_regular_file()) {
        Error(L"Nelson:saveas:filename", L"Impossible to save image.");
    }
    return retval;
}
//=============================================================================
ArrayOfVector
GraphicsIoGateway::webExportGraphicsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 0);
    const auto handle = argIn[0].asGraphicsObjectScalar();
    auto* figure = findGOWindows(handle);
    const auto filename = argIn[1].asWideString();
    const FileSystemWrapper::Path path(filename);
    const auto format = getExportImageFormatFromString(path.extension().wstring());
    if (!figure || !path.parent_path().is_directory()
        || (format != PNG_EXPORT && format != SVG_EXPORT && format != PDF_EXPORT)) {
        Error(L"Nelson:graphics:invalidExport", L"Valid figure and PNG, SVG or PDF path expected.");
    }
    if (!ExportGraphicsAtScale(figure, filename, format, argIn[2].asDoubleScalar())) {
        Error(L"Nelson:graphics:exportFailed", L"Cannot export figure.");
    }
    return {};
}
//=============================================================================
