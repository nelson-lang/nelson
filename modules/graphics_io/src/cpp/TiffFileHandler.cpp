//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "TiffFileHandler.hpp"
#if WITH_TIFF
#include <tiffio.h>
#include <QtCore/QDebug>
#endif
//=============================================================================
#ifdef _MSC_VER
#pragma comment(lib, "tiff.lib")
#endif
//=============================================================================
QImage
TiffFileHandler::readTiff(const QString& filePath)
{
#if WITH_TIFF
    TIFF* tif = TIFFOpen(filePath.toUtf8().constData(), "r");
    if (!tif) {
        qDebug() << "Failed to open TIFF file for reading:" << filePath;
        return QImage();
    }

    uint32_t width, height;
    TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &width);
    TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &height);

    QImage image(width, height, QImage::Format_RGB888);

    uint32_t* raster = (uint32_t*)_TIFFmalloc(width * height * sizeof(uint32_t));
    if (!raster) {
        qDebug() << "Memory allocation failed for TIFF image.";
        TIFFClose(tif);
        return QImage();
    }

    if (TIFFReadRGBAImage(tif, width, height, raster, 0)) {
        for (uint32_t y = 0; y < height; y++) {
            for (uint32_t x = 0; x < width; x++) {
                uint32_t pixel
                    = raster[(height - y - 1) * width + x]; // TIFF stores images bottom-up
                image.setPixelColor(
                    x, y, QColor(TIFFGetR(pixel), TIFFGetG(pixel), TIFFGetB(pixel)));
            }
        }
    } else {
        qDebug() << "Failed to read TIFF image data.";
        image = QImage(); // Return empty image on failure
    }

    _TIFFfree(raster);
    TIFFClose(tif);
    return image;
#else
    return QImage();
#endif
}
//=============================================================================
bool
TiffFileHandler::writeTiff(const QString& filePath, const QImage& image)
{
#if WITH_TIFF

    TIFF* tif = TIFFOpen(filePath.toUtf8().constData(), "w");
    if (!tif) {
        qDebug() << "Failed to open TIFF file for writing:" << filePath;
        return false;
    }

    uint32_t width = image.width();
    uint32_t height = image.height();

    TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, width);
    TIFFSetField(tif, TIFFTAG_IMAGELENGTH, height);
    TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, 3);
    TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, 8);
    TIFFSetField(tif, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
    TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
    TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);

    uint8_t* buf = (uint8_t*)_TIFFmalloc(width * height * 3);
    if (!buf) {
        qDebug() << "Memory allocation failed for TIFF write.";
        TIFFClose(tif);
        return false;
    }

    for (uint32_t y = 0; y < height; y++) {
        for (uint32_t x = 0; x < width; x++) {
            QColor color = image.pixelColor(x, y);
            uint32_t index = (y * width + x) * 3;
            buf[index] = color.red();
            buf[index + 1] = color.green();
            buf[index + 2] = color.blue();
        }
    }

    for (uint32_t row = 0; row < height; row++) {
        if (TIFFWriteScanline(tif, &buf[row * width * 3], row, 0) < 0) {
            qDebug() << "Failed to write TIFF scanline.";
            _TIFFfree(buf);
            TIFFClose(tif);
            return false;
        }
    }

    _TIFFfree(buf);
    TIFFClose(tif);
    return true;
#else
    return false;
#endif
}
//=============================================================================
