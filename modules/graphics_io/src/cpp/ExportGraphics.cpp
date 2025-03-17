//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtPrintSupport/QPrinter>
#include <QtGui/QImage>
#include <QtGui/QImageWriter>
#include <QtSvg/QtSvg>
#include <QtWidgets/QApplication>
#include <QtWidgets/QFileDialog>
#include <unordered_map>
#include "nlsBuildConfig.h"
#include "ExportGraphics.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "RenderQt.hpp"
#include "QStringConverter.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOColorProperty.hpp"
#include "FileSystemWrapper.hpp"
#include "QtTranslation.hpp"
#include "Nelson_VERSION.h"
#if WITH_GIF
#include "qgifimage.h"
#endif
#if WITH_TIFF
#include "TiffFileHandler.hpp"
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
#define PNG_EXTENSION L"png"
#if WITH_GIF
#define GIF_EXTENSION L"gif"
#endif
#define JPG_EXTENSION L"jpg"
#define PDF_EXTENSION L"pdf"
#define SVG_EXTENSION L"svg"
#define TIF_EXTENSION L"tif"
//=============================================================================
static std::unordered_map<std::wstring, IMAGE_FORMAT> formats = {
    { PNG_EXTENSION, PNG_EXPORT },
    { JPG_EXTENSION, JPG_EXPORT },
#if WITH_GIF
    { GIF_EXTENSION, GIF_EXPORT },
#endif
#if WITH_TIFF
    { TIF_EXTENSION, TIF_EXPORT },
#endif

    { PDF_EXTENSION, PDF_EXPORT },
    { SVG_EXTENSION, SVG_EXPORT },
};
//=============================================================================
bool
isSupportedImageFormatExtension(const std::wstring& extension)
{
    for (auto ex : formats) {
        if (StringHelpers::iequals(extension, ex.first)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
std::wstring
getExportImageFormatAsString(IMAGE_FORMAT exportFormat)
{
    for (auto ex : formats) {
        if (exportFormat == ex.second) {
            return ex.first;
        }
    }
    return {};
}
//=============================================================================
IMAGE_FORMAT
getExportImageFormatFromString(const std::wstring& extension)
{
    for (auto ex : formats) {
        if (StringHelpers::iequals(extension, ex.first)) {
            return ex.second;
        }
    }
    return ERROR_EXPORT;
}
//=============================================================================
bool
ExportGraphics(GOWindow* f, const std::wstring& filename, IMAGE_FORMAT exportFormat)
{
    bool result = false;
    if (!f) {
        return result;
    }
    GOFigure* hf = f->getGOFigure();
    GOColorProperty* color
        = static_cast<GOColorProperty*>(hf->findProperty(GO_COLOR_PROPERTY_NAME_STR));
    double cr = color->at(0);
    double cg = color->at(1);
    double cb = color->at(2);
    hf->setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
    hf->updateState();
    switch (exportFormat) {
    case PDF_EXPORT: {
        QPrinter printer(QPrinter::ScreenResolution);
        printer.setOutputFormat(QPrinter::PdfFormat);
        printer.setOutputFileName(wstringToQString(filename));
        QPainter painter;
        painter.begin(&printer);
        const auto pageLayout = printer.pageLayout();
        const auto pageRect = pageLayout.paintRectPixels(printer.resolution());
        const auto paperRect = pageLayout.fullRectPixels(printer.resolution());
        double xscale = pageRect.width() / double(f->width());
        double yscale = pageRect.height() / double(f->height());
        double scale = qMin(xscale, yscale);
        painter.translate(
            pageRect.x() + paperRect.width() / 2., pageRect.y() + paperRect.height() / 2.);
        painter.scale(scale, scale);
        painter.translate(-f->width() / 2., -f->height() / 2.);
        RenderQt gc(&painter, 0, 0, f->width(), f->height(), L"PDF");
        hf->paintMe(gc);
        result = true;
    } break;
    case PNG_EXPORT:
    case JPG_EXPORT: {
        QPixmap pxmap(f->getMainQWigdet()->grab());
        QImage img(pxmap.toImage());
        result = img.save(wstringToQString(filename),
            wstring_to_utf8(getExportImageFormatAsString(exportFormat)).c_str());
    } break;
    case SVG_EXPORT: {
        QSvgGenerator gen;
        gen.setDescription(QString(""));
        gen.setTitle(QString(""));
        gen.setFileName(wstringToQString(filename));
        gen.setSize(QSize(f->width(), f->height()));
        QPainter pnt(&gen);
        RenderQt gc(&pnt, 0, 0, f->width(), f->height(), L"SVG");
        hf->paintMe(gc);
        result = true;
    } break;
#if WITH_TIFF
    case TIF_EXPORT: {
        QPixmap pxmap(f->getMainQWigdet()->grab());
        QImage img(pxmap.toImage());
        result = TiffFileHandler::writeTiff(wstringToQString(filename), img);
    } break;
#endif
#if WITH_GIF
    case GIF_EXPORT: {
        QPixmap pxmap(f->getMainQWigdet()->grab());
        QImage img(pxmap.toImage());
        QGifImage gif(QSize(img.width(), img.height()));
        gif.addFrame(img);
        result = gif.save(wstringToQString(filename));
    } break;
#endif
    case ERROR_EXPORT:
    default: {
        result = false;
    } break;
    }
    hf->setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, cr, cg, cb);
    hf->updateState();
    return result;
}
//=============================================================================
NLSGRAPHICS_IO_IMPEXP bool
ExportGraphicsGUI(GOWindow* f)
{
#define PREFERED_DIRECTORY_EXPORT_IMAGE "preferedDirectoryExportImage"
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);

    QString currentDir = QDir::currentPath();
    if (settings.contains(PREFERED_DIRECTORY_EXPORT_IMAGE)) {
        currentDir = settings.value(PREFERED_DIRECTORY_EXPORT_IMAGE).toString();
        QFileInfo fileinfo(currentDir);
        if (!fileinfo.isDir()) {
            currentDir = QDir::homePath();
        }
    } else {
        currentDir = QDir::homePath();
    }
    QString defaultFilePath = currentDir + "/image";
    QString exportTypeMessage = TR("Export Image to ...");
    QString supportedFormats = TR("PNG File") + " (*.png)" + ";;" + TR("JPG File") + " (*.jpg)"
        + ";;" + TR("SVG File") + " (*.svg)" + ";;" + TR("PDF File") + " (*.pdf)";
#if WITH_GIF
    supportedFormats += ";;" + TR("GIF File") + " (*.gif)";
#endif
#if WITH_TIFF
    supportedFormats += ";;" + TR("TIFF File") + " (*.tif)";
#endif
    QString filePath
        = QFileDialog::getSaveFileName(f, exportTypeMessage, defaultFilePath, supportedFormats);

    if (filePath.isEmpty()) {
        return false;
    }
    settings.setValue(PREFERED_DIRECTORY_EXPORT_IMAGE, QFileInfo(filePath).absolutePath());

    IMAGE_FORMAT formatForced = IMAGE_FORMAT::PNG_EXPORT;
    std::wstring filename = QStringTowstring(filePath);
    FileSystemWrapper::Path p(filename);
    if (p.has_extension()) {
        std::wstring pathExtension = p.extension().wstring();
        pathExtension.erase(0, 1);
        if (!isSupportedImageFormatExtension(pathExtension)) {
            return false;
        }
        formatForced = getExportImageFormatFromString(pathExtension);
    } else {
        formatForced = IMAGE_FORMAT::PNG_EXPORT;
        filename = filename + L"." + getExportImageFormatAsString(formatForced);
        p = filename;
    }
    return ExportGraphics(f, filename, formatForced);
}
//=============================================================================
}
//=============================================================================
bool
ExportGraphicsGUI(void* f)
{
    return Nelson::ExportGraphicsGUI((Nelson::GOWindow*)f);
}
//=============================================================================
