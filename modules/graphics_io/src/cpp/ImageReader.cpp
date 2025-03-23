//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtGui/QImage>
#include "ImageReader.hpp"
#include "nlsGraphics_exports.h"
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FileSystemWrapper.hpp"
#include "QStringConverter.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "PcxFileHandler.hpp"
#include "TiffFileHandler.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOfVector
imageReaderRGB32(QImage image, int nLhs);
static ArrayOfVector
imageReaderGrayScale16(QImage image, int nLhs);
static ArrayOfVector
imageReaderARGB32(QImage image, int nLhs);
static ArrayOfVector
imageReaderIndexed8(QImage image, int nLhs);
//=============================================================================
ArrayOfVector
imageReader(const std::wstring& filename, int nLhs)
{
    ArrayOfVector results = {};
    FileSystemWrapper::Path path(filename);
    if (!path.is_regular_file()) {
        Error(_W("A valid filename expected."));
    }
    QImage image(wstringToQString(filename));
    if (image.isNull()) {
        image = PcxFileHandler::loadPCX(wstringToQString(filename));
    }
    if (image.isNull()) {
        QString errorMessage;
        image = TiffFileHandler::readTiff(wstringToQString(filename), errorMessage);
    }
    if (image.isNull()) {
        Error(_W("Impossible read image file."));
    }
    auto imageFormat = image.format();
    switch (imageFormat) {
    case QImage::Format_Indexed8: {
        results = imageReaderIndexed8(image, nLhs);
    } break;
    case QImage::Format_RGB32: {
        results = imageReaderRGB32(image, nLhs);
    } break;
    case QImage::Format_ARGB32: {
        results = imageReaderARGB32(image, nLhs);
    } break;
    case QImage::Format_Grayscale16: {
        results = imageReaderGrayScale16(image, nLhs);
    } break;
    default: {
        image = image.convertToFormat(QImage::Format_RGB32);
        results = imageReaderRGB32(image, nLhs);
    } break;
    case QImage::Format_Invalid: {
        Error(_W("Unsupported file image format."));
    } break;
    }
    return results;
}
//=============================================================================
ArrayOfVector
imageReaderRGB32(QImage image, int nLhs)
{
    ArrayOfVector results;
    results.reserve(nLhs + 1);
    std::vector<indexType> d;
    d.push_back(image.height());
    d.push_back(image.width());
    d.push_back(3);
    Dimensions dims(d);
    uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, dims.getElementCount());
    ArrayOf A = ArrayOf(NLS_UINT8, dims, ptr);
    ompIndexType imageCounter = (ompIndexType)image.height() * (ompIndexType)image.width();
    OMP_PARALLEL_FOR_LOOP(imageCounter)
    for (ompIndexType idx = 0; idx < imageCounter; idx++) {
        indexType row = idx % image.height();
        indexType col = idx / image.height();

        QRgb* p = (QRgb*)image.scanLine((int)row);
        indexType ndx = row + col * image.height();

        ptr[ndx] = qRed(p[col]);
        ptr[ndx + 1 * imageCounter] = qGreen(p[col]);
        ptr[ndx + 2 * imageCounter] = qBlue(p[col]);
    }
    results << A;
    if (nLhs > 1) {
        // no colormap
        results << ArrayOf::emptyConstructor(0, 0);
    }
    if (nLhs > 2) {
        // no transparency
        results << ArrayOf::emptyConstructor(0, 0);
    }
    return results;
}
//=============================================================================
ArrayOfVector
imageReaderARGB32(QImage image, int nLhs)
{
    ArrayOfVector results = {};
    results.reserve(nLhs + 1);

    std::vector<indexType> d;
    d.push_back(image.height());
    d.push_back(image.width());
    d.push_back(3);
    Dimensions dims(d);
    uint8* ptrA = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, dims.getElementCount());
    ArrayOf A = ArrayOf(NLS_UINT8, dims, ptrA);

    Dimensions dimsTransparency(image.height(), image.width());
    uint8* ptrTransparency
        = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, dimsTransparency.getElementCount());
    ArrayOf transparency = ArrayOf(NLS_UINT8, dimsTransparency, ptrTransparency);

    ompIndexType imageCounter = (ompIndexType)image.height() * (ompIndexType)image.width();
    OMP_PARALLEL_FOR_LOOP(imageCounter)
    for (ompIndexType idx = 0; idx < imageCounter; idx++) {
        indexType row = idx % image.height();
        indexType col = idx / image.height();

        QRgb* p = (QRgb*)image.scanLine((int)row);
        indexType ndx = row + col * image.height();

        ptrA[ndx] = qRed(p[col]);
        ptrA[ndx + 1 * imageCounter] = qGreen(p[col]);
        ptrA[ndx + 2 * imageCounter] = qBlue(p[col]);
        ptrTransparency[ndx] = qAlpha(p[col]);
    }
    results << A;
    if (nLhs > 1) {
        // no colormap
        results << ArrayOf::emptyConstructor(0, 0);
    }
    if (nLhs > 2) {
        // transparency
        results << transparency;
    }
    return results;
}
//=============================================================================
ArrayOfVector
imageReaderIndexed8(QImage image, int nLhs)
{
    ArrayOfVector results = {};
    results.reserve(nLhs + 1);

    Dimensions dimsA(image.height(), image.width());
    uint8* ptrA = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, dimsA.getElementCount());
    ArrayOf A = ArrayOf(NLS_UINT8, dimsA, ptrA);

    ompIndexType imageCounter = image.height() * image.width();
    OMP_PARALLEL_FOR_LOOP(imageCounter)
    for (int idx = 0; idx < imageCounter; idx++) {
        int row = idx % image.height();
        int col = idx / image.height();
        uchar* p = image.scanLine(row);
        ptrA[row + col * image.height()] = p[col];
    }
    results << A;
    if (nLhs > 1) {
        // colormap
        QVector<QRgb> colorTable(image.colorTable());
        int numcol = colorTable.size();
        Dimensions dimsColorTable(numcol, 3);
        double* ptrColorTable
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsColorTable.getElementCount());
        ArrayOf colormap = ArrayOf(NLS_DOUBLE, dimsColorTable, ptrColorTable);
        OMP_PARALLEL_FOR_LOOP(numcol)
        for (int i = 0; i < numcol; i++) {
            QColor c(colorTable[i]);
            ptrColorTable[i] = (double)c.redF();
            ptrColorTable[i + numcol] = (double)c.greenF();
            ptrColorTable[i + 2 * numcol] = (double)c.blueF();
        }
        results << colormap;
    }
    if (nLhs > 2) {
        // transparency
        QImage alpha = image.convertToFormat(QImage::Format_Alpha8);
        uint8* ptrTransparency
            = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, dimsA.getElementCount());
        ArrayOf transparency = ArrayOf(NLS_UINT8, dimsA, ptrTransparency);
        ompIndexType alphaImageCounter = alpha.height() * alpha.width();
        OMP_PARALLEL_FOR_LOOP(alphaImageCounter)
        for (int idx = 0; idx < alphaImageCounter; idx++) {
            int row = idx % alpha.height();
            int col = idx / alpha.height();

            uchar* p = alpha.scanLine(row);
            ptrTransparency[row + col * image.height()] = p[col];
        }
        results << transparency;
    }
    return results;
}
//=============================================================================
ArrayOfVector
imageReaderGrayScale16(QImage image, int nLhs)
{
    ArrayOfVector results = {};
    results.reserve(nLhs + 1);

    Dimensions dimsA(image.height(), image.width());
    uint16* ptrA = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, dimsA.getElementCount());
    ArrayOf A = ArrayOf(NLS_UINT16, dimsA, ptrA);
    ompIndexType imageCounter = image.height() * image.width();
    OMP_PARALLEL_FOR_LOOP(imageCounter)
    for (int idx = 0; idx < imageCounter; idx++) {
        int row = idx % image.height();
        int col = idx / image.height();

        QRgb* p = (QRgb*)image.scanLine(row);
        int ndx = row + col * image.height();
        ptrA[ndx] = quint16(p[col]);
    }
    results << A;
    if (nLhs > 1) {
        // no colormap
        results << ArrayOf::emptyConstructor(0, 0);
    }
    if (nLhs > 2) {
        // transparency
        results << ArrayOf::emptyConstructor(0, 0);
    }
    return results;
}
//=============================================================================
}
//=============================================================================
