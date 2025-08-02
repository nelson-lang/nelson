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
            Error(_("figure number is out of range - it must be between 1 and 2147483647."));
        }
        f = getHandleWindow(fignum);
    }
    if (!f) {
        Error(_W("Invalid handle."));
    }
    std::wstring filename = argIn[1].getContentAsWideString();
    FileSystemWrapper::Path p(filename);
    if (!p.parent_path().is_directory()) {
        Error(_W("Parent directory does not exist: ") + p.parent_path().generic_wstring());
    }
    IMAGE_FORMAT formatForced;
    if (p.has_extension()) {
        std::wstring pathExtension = p.extension().wstring();
        pathExtension.erase(0, 1);
        if (argIn.size() == 3) {
            std::wstring param3 = argIn[2].getContentAsWideString();
            if (!isSupportedImageFormatExtension(param3)) {
                Error(_W("Unsupported format:") + param3, L"Nelson:saveas:badFormat");
            }
            formatForced = getExportImageFormatFromString(param3);
        } else {
            if (!isSupportedImageFormatExtension(pathExtension)) {
                Error(_W("Unsupported format:") + pathExtension, L"Nelson:saveas:badFormat");
            }
            formatForced = getExportImageFormatFromString(pathExtension);
        }
    } else {
        if (argIn.size() == 3) {
            std::wstring param3 = argIn[2].getContentAsWideString();
            if (!isSupportedImageFormatExtension(param3)) {
                Error(_W("Unsupported format:") + param3, L"Nelson:saveas:badFormat");
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
        Error(_W("Impossible to save image."), L"Nelson:saveas:badFormat");
    }
    if (!p.is_regular_file()) {
        Error(_W("Impossible to save image."), L"Nelson:saveas:filename");
    }
    return retval;
}
//=============================================================================
