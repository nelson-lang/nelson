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
#include "RasterCodec.hpp"
#include "RasterAtomicFile.hpp"
#include "RenderWeb.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "GOFiguresManager.hpp"
#include "GOColorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <cmath>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <unordered_map>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    ExportGraphicsDialogProvider dialogProvider = nullptr;
    //=============================================================================
    const std::unordered_map<std::wstring, IMAGE_FORMAT>&
    exportFormats()
    {
        static const std::unordered_map<std::wstring, IMAGE_FORMAT> formats = {
            { L"png", PNG_EXPORT },
            { L"jpg", JPG_EXPORT },
            { L"jpeg", JPG_EXPORT },
            { L"jfif", JPG_EXPORT },
            { L"gif", GIF_EXPORT },
            { L"tif", TIF_EXPORT },
            { L"tiff", TIF_EXPORT },
            { L"pdf", PDF_EXPORT },
            { L"svg", SVG_EXPORT },
            { L"webp", WEBP_EXPORT },
            { L"bmp", BMP_EXPORT },
            { L"dib", BMP_EXPORT },
            { L"tga", TGA_EXPORT },
            { L"pbm", PBM_EXPORT },
            { L"pgm", PGM_EXPORT },
            { L"ppm", PPM_EXPORT },
            { L"pnm", PNM_EXPORT },
            { L"pcx", PCX_EXPORT },
        };
        return formats;
    }
    //=============================================================================
    bool
    rasterExport(const std::vector<WebDrawPrimitive>& primitives,
        const std::vector<WebUIControl>& controls, int width, int height,
        const std::wstring& filename, const std::wstring& format)
    {
        std::vector<unsigned char> rgb;
        if (!rasterizeWebPrimitivesAndControls(primitives, controls, width, height, rgb)) {
            return false;
        }
        RasterImage image;
        image.width = width;
        image.height = height;
        image.format = RasterPixelFormat::RGB8;
        image.pixels = std::move(rgb);
        std::string error;
        return writeRasterImageAtomic(filename, wstring_to_utf8(format), image, 75, error);
    }
    //=============================================================================
    bool
    isUiFigureLike(GOFigure* figure)
    {
        return figure
            && figure->stringCheck(GO_MENU_BAR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)
            && figure->stringCheck(GO_TOOL_BAR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)
            && figure->stringCheck(GO_NUMBER_TITLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)
            && figure->stringCheck(GO_INTEGER_HANDLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    }
    //=============================================================================
    // Counts filled faces. A z-buffered surface, an image or a colorbar ramp is not
    // counted: those export as one inline <image> or a few rects.
    size_t
    filledPolygonCount(const std::vector<WebDrawPrimitive>& primitives)
    {
        size_t count = 0;
        for (const WebDrawPrimitive& primitive : primitives) {
            if (primitive.kind == "polygon") {
                ++count;
            } else if (primitive.kind == "points" && !primitive.pointColors.empty()) {
                // One filled vector shape per marker.
                count += primitive.pointRadii.size();
            }
        }
        return count;
    }
    //=============================================================================
    // Above this many filled faces (dense contour, translucent surface) a vector
    // <polygon> per face bloats the SVG, so the figure is exported as one raster.
    constexpr size_t maxVectorFilledPolygons = 2000;
    //=============================================================================
    bool
    embeddedRasterSvg(const std::vector<WebDrawPrimitive>& primitives,
        const std::vector<WebUIControl>& controls, int width, int height, double displayScale,
        const std::wstring& filename)
    {
        std::string png;
        if (!rasterizeWebPrimitivesAndControlsToPngBase64(
                primitives, controls, width, height, png)) {
            return false;
        }
        std::ofstream output(std::filesystem::path(filename), std::ios::binary | std::ios::trunc);
        if (!output) {
            return false;
        }
        // The viewBox keeps the export resolution, so the raster is displayed
        // downscaled to the logical size, never upscaled.
        const double declaredWidth = width * displayScale;
        const double declaredHeight = height * displayScale;
        output << "<svg width=\"" << declaredWidth << "\" height=\"" << declaredHeight
               << "\" viewBox=\"0 0 " << width << " " << height
               << "\" xmlns=\"http://www.w3.org/2000/svg\">\n"
               << "<image x=\"0\" y=\"0\" width=\"" << width << "\" height=\"" << height
               << "\" href=\"data:image/png;base64," << png << "\"/>\n</svg>\n";
        return output.good();
    }
    //=============================================================================
    // uicontrols have no vector representation, so the whole page is rasterized
    // and embedded as a single image.
    bool
    embeddedRasterPdf(const std::vector<WebDrawPrimitive>& primitives,
        const std::vector<WebUIControl>& controls, int width, int height,
        const std::wstring& filename)
    {
        std::string png;
        if (!rasterizeWebPrimitivesAndControlsToPngBase64(
                primitives, controls, width, height, png)) {
            return false;
        }
        WebDrawPrimitive image;
        image.kind = "image";
        image.x = 0;
        image.y = 0;
        image.w = width;
        image.h = height;
        image.image = "data:image/png;base64," + png;
        return webPrimitivesToPdfFile({ image }, width, height, filename);
    }
    //=============================================================================
    template <class Writer>
    bool
    atomicVectorExport(const std::wstring& filename, Writer writer)
    {
        const std::filesystem::path target(filename);
        const std::filesystem::path temporary = rasterTemporaryPath(target);
        std::error_code ec;
        std::filesystem::remove(temporary, ec);
        if (!writer(temporary.wstring())) {
            std::filesystem::remove(temporary, ec);
            return false;
        }
        std::string error;
        return replaceFileAtomically(temporary, target, error);
    }
    //=============================================================================
} // namespace
//=============================================================================
void
setExportGraphicsDialogProvider(ExportGraphicsDialogProvider provider)
{
    dialogProvider = provider;
}
//=============================================================================
bool
isSupportedImageFormatExtension(const std::wstring& extension)
{
    std::wstring normalized = extension;
    if (!normalized.empty() && normalized.front() == L'.') {
        normalized.erase(normalized.begin());
    }
    std::transform(normalized.begin(), normalized.end(), normalized.begin(), ::towlower);
    return exportFormats().find(normalized) != exportFormats().end();
}
//=============================================================================
std::wstring
getExportImageFormatAsString(IMAGE_FORMAT format)
{
    switch (format) {
    case PNG_EXPORT:
        return L"png";
    case JPG_EXPORT:
        return L"jpg";
    case GIF_EXPORT:
        return L"gif";
    case TIF_EXPORT:
        return L"tiff";
    case PDF_EXPORT:
        return L"pdf";
    case SVG_EXPORT:
        return L"svg";
    case WEBP_EXPORT:
        return L"webp";
    case BMP_EXPORT:
        return L"bmp";
    case TGA_EXPORT:
        return L"tga";
    case PBM_EXPORT:
        return L"pbm";
    case PGM_EXPORT:
        return L"pgm";
    case PPM_EXPORT:
        return L"ppm";
    case PNM_EXPORT:
        return L"pnm";
    case PCX_EXPORT:
        return L"pcx";
    default:
        return {};
    }
}
//=============================================================================
IMAGE_FORMAT
getExportImageFormatFromString(const std::wstring& extension)
{
    std::wstring normalized = extension;
    if (!normalized.empty() && normalized.front() == L'.') {
        normalized.erase(normalized.begin());
    }
    std::transform(normalized.begin(), normalized.end(), normalized.begin(), ::towlower);
    auto found = exportFormats().find(normalized);
    return found == exportFormats().end() ? ERROR_EXPORT : found->second;
}
//=============================================================================
bool
ExportGraphics(GOFigureHost* host, const std::wstring& filename, IMAGE_FORMAT format)
{
    return ExportGraphicsAtScale(host, filename, format, 150.0 / 96.0);
}
//=============================================================================
bool
ExportGraphicsAtScale(
    GOFigureHost* host, const std::wstring& filename, IMAGE_FORMAT format, double exportScale)
{
    GOFigure* figure = host ? host->hostFigure() : nullptr;
    if (!figure) {
        return false;
    }
    if (!std::isfinite(exportScale) || exportScale <= 0 || exportScale > 4) {
        return false;
    }
    const int layoutWidth = figure->getLayoutWidth();
    const int layoutHeight = figure->getLayoutHeight();
    const int width = static_cast<int>(std::ceil(layoutWidth * exportScale));
    const int height = static_cast<int>(std::ceil(layoutHeight * exportScale));
    if (width <= 0 || height <= 0) {
        return false;
    }
    const int previousWidth = figure->getWidth();
    const int previousHeight = figure->getHeight();
    const double previousScale = figure->getRenderScale();
    // Record at the logical size (scale 1.0) so layout and text metrics stay
    // device-independent. The export DPI is applied later by scaling the primitives.
    figure->setRenderingSize(layoutWidth, layoutHeight, 1.0);
    // With InvertHardcopy on (the default), the background is whitened only when
    // the figure Color is still the default one. An explicitly set Color is kept.
    // The Color is swapped around the paint pass only and restored after it.
    GOColorProperty* figureColor
        = static_cast<GOColorProperty*>(figure->findProperty(GO_COLOR_PROPERTY_NAME_STR, false));
    const bool invertHardcopy
        = figure->stringCheck(GO_INVERT_HARDCOPY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    const bool figureColorIsUserSet = figure->hasChanged(GO_COLOR_PROPERTY_NAME_STR);
    std::vector<double> previousColor;
    const bool overrideBackground = invertHardcopy && figureColor && !figureColorIsUserSet;
    if (overrideBackground) {
        previousColor = figureColor->data();
        figureColor->data({ 1.0, 1.0, 1.0 });
    }
    RenderWeb renderer(0, 0, layoutWidth, layoutHeight);
    figure->paintMe(renderer);
    renderer.flush3d();
    if (overrideBackground) {
        figureColor->data(previousColor);
    }
    figure->setRenderingSize(previousWidth, previousHeight, previousScale);
    const std::vector<WebDrawPrimitive> primitives
        = scalePrimitivesToSurface(renderer.primitives(), exportScale);
    const std::vector<WebUIControl> controls
        = scaleControlsToSurface(renderer.controls(), exportScale);
    if (format == SVG_EXPORT) {
        // uicontrols and uifigures, or too many filled faces, use the raster path.
        const bool rasterFallback = isUiFigureLike(figure) || !controls.empty()
            || filledPolygonCount(primitives) > maxVectorFilledPolygons;
        // Inverse of exportScale: internal pixels back to the logical size.
        const double displayScale = 1.0 / exportScale;
        return atomicVectorExport(filename, [&](const std::wstring& temporary) {
            return rasterFallback
                ? embeddedRasterSvg(primitives, controls, width, height, displayScale, temporary)
                : webPrimitivesToSvgFile(primitives, width, height, temporary, displayScale);
        });
    }
    if (format == PDF_EXPORT) {
        return atomicVectorExport(filename, [&](const std::wstring& temporary) {
            return controls.empty()
                ? webPrimitivesToPdfFile(primitives, width, height, temporary)
                : embeddedRasterPdf(primitives, controls, width, height, temporary);
        });
    }
    const std::wstring rasterFormat = getExportImageFormatAsString(format);
    return !rasterFormat.empty()
        && rasterExport(primitives, controls, width, height, filename, rasterFormat);
}
//=============================================================================
bool
ExportGraphicsGUI(GOFigureHost* host)
{
    return dialogProvider != nullptr && dialogProvider(host);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
extern "C" NLSGRAPHICS_IO_IMPEXP bool
ExportGraphicsGUI(void* host)
{
    return Nelson::ExportGraphicsGUI(static_cast<Nelson::GOFigureHost*>(host));
}
//=============================================================================
extern "C" NLSGRAPHICS_IO_IMPEXP bool
nlsWebGuiSaveFigure(int64_t handle, const char* path)
{
    if (!path) {
        return false;
    }
    Nelson::GOFigureHost* host = Nelson::getFigure(static_cast<Nelson::int64>(handle));
    if (!host) {
        return false;
    }
    std::wstring filename = Nelson::utf8_to_wstring(path);
    Nelson::FileSystemWrapper::Path output(filename);
    if (output.has_parent_path() && !output.parent_path().is_directory()) {
        return false;
    }
    Nelson::IMAGE_FORMAT format = Nelson::PNG_EXPORT;
    if (output.has_extension()) {
        format = Nelson::getExportImageFormatFromString(output.extension().wstring());
        if (format == Nelson::ERROR_EXPORT) {
            return false;
        }
    } else {
        filename += L".png";
    }
    return Nelson::ExportGraphics(host, filename, format);
}
//=============================================================================
