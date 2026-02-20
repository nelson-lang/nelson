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

    GOWindow* f = nullptr;
    if (argIn[0].isGraphicsObject()) {
        int64 handle = argIn[0].getContentAsGraphicsObjectScalar();
        f = findGOWindows(handle);
    } else {
        int64 fignum = argIn[0].getContentAsInteger64Scalar();
        if ((fignum <= 0) || (fignum > MAX_FIGS)) {
            raiseError(L"Nelson:graphics_io:ERROR_FIGURE_NUMBER_IS_OUT_OF_RANGE_IT_MUST_BE_BETWEEN_"
                       L"1_AND_2147483647",
                ERROR_FIGURE_NUMBER_IS_OUT_OF_RANGE_IT_MUST_BE_BETWEEN_1_AND_2147483647);
        }
        f = getHandleWindow(fignum);
    }
    if (!f) {
        raiseError2(_E("nelson:validators:invalidNelsonHandle"));
    }
    std::wstring filename = argIn[1].getContentAsWideString();
    FileSystemWrapper::Path p(filename);
    if (!p.parent_path().is_directory()) {
        raiseError(L"Nelson:graphics_io:ERROR_PARENT_DIRECTORY_DOES_NOT_EXIST",
            ERROR_PARENT_DIRECTORY_DOES_NOT_EXIST, p.parent_path().generic_wstring());
    }
    IMAGE_FORMAT formatForced;
    if (p.has_extension()) {
        std::wstring pathExtension = p.extension().wstring();
        pathExtension.erase(0, 1);
        if (argIn.size() == 3) {
            std::wstring param3 = argIn[2].getContentAsWideString();
            if (!isSupportedImageFormatExtension(param3)) {
                raiseError(L"Nelson:graphics_io:ERROR_UNSUPPORTED_FORMAT", ERROR_UNSUPPORTED_FORMAT,
                    param3);
            }
            formatForced = getExportImageFormatFromString(param3);
        } else {
            if (!isSupportedImageFormatExtension(pathExtension)) {
                raiseError(L"Nelson:graphics_io:ERROR_UNSUPPORTED_FORMAT", ERROR_UNSUPPORTED_FORMAT,
                    pathExtension);
            }
            formatForced = getExportImageFormatFromString(pathExtension);
        }
    } else {
        if (argIn.size() == 3) {
            std::wstring param3 = argIn[2].getContentAsWideString();
            if (!isSupportedImageFormatExtension(param3)) {
                raiseError(L"Nelson:graphics_io:ERROR_UNSUPPORTED_FORMAT", ERROR_UNSUPPORTED_FORMAT,
                    param3);
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
        raiseError(
            L"Nelson:graphics_io:ERROR_IMPOSSIBLE_TO_SAVE_IMAGE", ERROR_IMPOSSIBLE_TO_SAVE_IMAGE);
    }
    if (!p.is_regular_file()) {
        raiseError(
            L"Nelson:graphics_io:ERROR_IMPOSSIBLE_TO_SAVE_IMAGE", ERROR_IMPOSSIBLE_TO_SAVE_IMAGE);
    }
    return retval;
}
//=============================================================================
