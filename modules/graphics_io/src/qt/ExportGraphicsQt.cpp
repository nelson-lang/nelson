//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ExportGraphics.hpp"
#include "GraphicsIoQtAdapter.hpp"
#include "GraphicsQtModule.hpp"
#include "GOWindow.hpp"
#include "QStringConverter.hpp"
#include <QtCore/QDir>
#include <QtCore/QFileInfo>
#include <QtWidgets/QFileDialog>
#include <filesystem>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    bool
    exportWithDesktopDialog(GOFigureHost* host)
    {
        auto* window = dynamic_cast<GOWindow*>(host);
        if (!window) {
            return false;
        }
        const QString filters
            = "PNG (*.png);;JPEG (*.jpg);;GIF (*.gif);;WebP (*.webp);;TIFF (*.tiff);;"
              "BMP (*.bmp);;TGA (*.tga);;PBM (*.pbm);;PGM (*.pgm);;PPM (*.ppm);;"
              "PNM (*.pnm);;PCX (*.pcx);;SVG (*.svg);;PDF (*.pdf)";
        QString filename
            = QFileDialog::getSaveFileName(window, "Export Image", QDir::homePath(), filters);
        if (filename.isEmpty()) {
            return false;
        }
        std::wstring output = QStringTowstring(filename);
        std::filesystem::path path(output);
        IMAGE_FORMAT format = path.has_extension()
            ? getExportImageFormatFromString(path.extension().wstring())
            : PNG_EXPORT;
        if (!path.has_extension()) {
            output += L".png";
        }
        return format != ERROR_EXPORT && ExportGraphics(host, output, format);
    }
    //=============================================================================
    struct ProviderRegistration
    {
        ProviderRegistration() { setExportGraphicsDialogProvider(&exportWithDesktopDialog); }
        ~ProviderRegistration() { setExportGraphicsDialogProvider(nullptr); }
    } registration;
    //=============================================================================
} // namespace
//=============================================================================
} // namespace Nelson
//=============================================================================
extern "C" NLS_GRAPHICS_IO_QT_EXPORT void
initializeGraphicsIoQtAdapter()
{
    Nelson::initializeGraphicsQtBackend();
}
//=============================================================================
