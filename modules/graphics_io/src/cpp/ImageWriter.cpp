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
#include <QtGui/QImageWriter>
#include "ImageWriter.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "RealPart.hpp"
#include "QStringConverter.hpp"
#include "StringHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#if WITH_GIF
#include "qgifimage.h"
#endif
#if WITH_TIFF
#include "TiffFileHandler.hpp"
#endif
#include "PcxFileHandler.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isSupportedFormat(const std::wstring& format);
//=============================================================================
static QImage
imwriteRGB32(const ArrayOf& A);
//=============================================================================
static QImage
imwriteRGBA32(const ArrayOf& A, const ArrayOf& alphaMap);
//=============================================================================
static QImage
imwriteIndexed8(const ArrayOf& A, const ArrayOf& colorMap, const ArrayOf& alphaMap);
//=============================================================================
static ArrayOf
convertImageToUint(const ArrayOf& image);
//=============================================================================
static void
setTextInfo(QImageWriter& qimagewriter, const std::map<std::wstring, wstringVector>& nameValue);
//=============================================================================
void
writePcx(const std::wstring& filename, const ArrayOf& A, const ArrayOf& colorMap,
    const ArrayOf& alphaMap, int quality, const std::map<std::wstring, wstringVector>& nameValue)
{
    QImage qImage;
    bool hasAlpha = !alphaMap.isEmpty(true);
    Dimensions dimensionsA = A.getDimensions();

    if (dimensionsA.getLength() == 2) {
        qImage = imwriteIndexed8(A, colorMap, alphaMap);
    } else if (dimensionsA.getLength() == 3) {
        if (!hasAlpha) {
            qImage = imwriteRGB32(A);
        } else {
            qImage = imwriteRGBA32(A, alphaMap);
        }
    } else {
        Error(_W("Image data must be either MxN or MxNx3."));
        return;
    }

    int result = PcxFileHandler::savePCX(wstringToQString(filename), qImage);
    if (result != PcxFileHandler::PCX_ERROR_NONE) {
        Error(_W("Cannot save image file: ") + filename);
    }
}
//=============================================================================
#if WITH_GIF
void
writeGif(const std::wstring& filename, bool append, const ArrayOf& A, const ArrayOf& colorMap,
    const ArrayOf& alphaMap, int quality, int delayTime, int loopCount,
    const std::map<std::wstring, wstringVector>& nameValue)
{
    if (!append) {
        FileSystemWrapper::Path pathFileName(filename);
        if (pathFileName.is_regular_file()) {
            FileSystemWrapper::Path::remove(pathFileName);
        }
    }

    QImage qImage;
    bool hasAlpha = !alphaMap.isEmpty(true);
    Dimensions dimensionsA = A.getDimensions();
    if (dimensionsA.getLength() == 2) {
        qImage = imwriteIndexed8(A, colorMap, alphaMap);
    } else if (dimensionsA.getLength() == 3) {
        if (!hasAlpha) {
            qImage = imwriteRGB32(A);
        } else {
            qImage = imwriteRGBA32(A, alphaMap);
        }
    } else {
        Error(_W("Image data must be either MxN or MxNx3."));
    }
    QGifImage gif(QSize(qImage.width(), qImage.height()));

    if (append) {
        gif.load(wstringToQString(filename));
    } else {
        gif.setLoopCount(loopCount);
    }
    gif.addFrame(qImage, delayTime);
    if (!gif.save(wstringToQString(filename))) {
        Error(_W("Cannot save image file: ") + filename);
    }
}
#endif
//=============================================================================
#if WITH_TIFF
void
writeTiff(const std::wstring& filename, const ArrayOf& A, const ArrayOf& colorMap,
    const ArrayOf& alphaMap, const std::map<std::wstring, wstringVector>& nameValue)
{
    FileSystemWrapper::Path pathFileName(filename);
    if (pathFileName.is_regular_file()) {
        FileSystemWrapper::Path::remove(pathFileName);
    }

    QImage qImage;
    bool hasAlpha = !alphaMap.isEmpty(true);
    Dimensions dimensionsA = A.getDimensions();
    if (dimensionsA.getLength() == 2) {
        qImage = imwriteIndexed8(A, colorMap, alphaMap);
    } else if (dimensionsA.getLength() == 3) {
        if (!hasAlpha) {
            qImage = imwriteRGB32(A);
        } else {
            qImage = imwriteRGBA32(A, alphaMap);
        }
    } else {
        Error(_W("Image data must be either MxN or MxNx3."));
    }
    if (!TiffFileHandler::writeTiff(wstringToQString(filename), qImage)) {
        Error(_W("Cannot save image file: ") + filename);
    }
}
#endif
//=============================================================================
void
writeImage(const std::wstring& filename, const ArrayOf& A, const ArrayOf& colorMap,
    const std::wstring& format, const ArrayOf& alphaMap, int quality,
    const std::map<std::wstring, wstringVector>& nameValue)
{
    if (!isSupportedFormat(format)) {
        Error(_W("Not supported format."));
    }

    FileSystemWrapper::Path pathFileName(filename);
    if (pathFileName.is_regular_file()) {
        FileSystemWrapper::Path::remove(pathFileName);
    }
    bool hasAlpha = !alphaMap.isEmpty(true);

    QImageWriter imgWriter(wstringToQString(filename));
    if (!imgWriter.canWrite()) {
        Error(_W("Cannot write ") + filename);
    }
    imgWriter.setFormat(wstringToQString(format).toUtf8());
    imgWriter.setQuality(quality);

    setTextInfo(imgWriter, nameValue);

    QImage qImage;

    Dimensions dimensionsA = A.getDimensions();

    if (dimensionsA.getLength() == 2) {
        qImage = imwriteIndexed8(A, colorMap, alphaMap);
    } else if (dimensionsA.getLength() == 3) {
        if (!hasAlpha) {
            qImage = imwriteRGB32(A);
        } else {
            qImage = imwriteRGBA32(A, alphaMap);
        }
    } else {
        Error(_W("Image data must be either MxN or MxNx3."));
    }

    if (!imgWriter.write(qImage)) {
        Error(_W("Cannot save image file: ") + filename);
    }
}
//=============================================================================
bool
isSupportedFormat(const std::wstring& format)
{
    QList<QByteArray> supportedFormatList = QImageWriter::supportedImageFormats();
    for (auto element : supportedFormatList) {
        if (StringHelpers::iequals(QStringTowstring(QString(element)), format)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
QImage
imwriteRGBA32(const ArrayOf& A, const ArrayOf& alphaMap)
{
    ArrayOf imageAsUint = convertImageToUint(A);
    ArrayOf alphaAsUint = convertImageToUint(alphaMap);
    uint8* data = (uint8*)imageAsUint.getDataPointer();
    uint8* alpha = (uint8*)alphaAsUint.getDataPointer();
    QImage qImage(int(imageAsUint.getColumns()), int(imageAsUint.getRows()), QImage::Format_ARGB32);
    ompIndexType imageSlice = qImage.height() * qImage.width();
    OMP_PARALLEL_FOR_LOOP(imageSlice)
    for (ompIndexType idx = 0; idx < imageSlice; idx++) {
        ompIndexType row = idx % qImage.height();
        ompIndexType col = idx / qImage.height();
        QRgb* p = (QRgb*)qImage.scanLine(row);
        int ndx = row + col * qImage.height();
        p[col]
            = qRgba(data[ndx], data[ndx + 1 * imageSlice], data[ndx + 2 * imageSlice], alpha[ndx]);
    }
    return qImage;
}
//=============================================================================
QImage
imwriteRGB32(const ArrayOf& A)
{
    ArrayOf imageAsUint = convertImageToUint(A);
    uint8* data = (uint8*)imageAsUint.getDataPointer();
    QImage qImage(int(imageAsUint.getColumns()), int(imageAsUint.getRows()), QImage::Format_RGB32);

    ompIndexType imageSlice = qImage.height() * qImage.width();
    OMP_PARALLEL_FOR_LOOP(imageSlice)
    for (ompIndexType idx = 0; idx < imageSlice; idx++) {
        ompIndexType row = idx % qImage.height();
        ompIndexType col = idx / qImage.height();
        QRgb* p = (QRgb*)qImage.scanLine(row);
        ompIndexType ndx = row + col * qImage.height();
        p[col] = qRgb(data[ndx], data[ndx + 1 * imageSlice], data[ndx + 2 * imageSlice]);
    }
    return qImage;
}
//=============================================================================
QImage
imwriteIndexed8(const ArrayOf& A, const ArrayOf& colorMap, const ArrayOf& alphaMap)
{
    QImage image(int(A.getColumns()), int(A.getRows()), QImage::Format_Indexed8);
    ArrayOf imageAsUint = convertImageToUint(A);
    uint8* data = (uint8*)imageAsUint.getDataPointer();

    ArrayOf colorMapAsUint = convertImageToUint(colorMap);
    uint8* colors = colorMapAsUint.isEmpty() ? nullptr : (uint8*)colorMapAsUint.getDataPointer();

    ArrayOf alphaMapAsUint = convertImageToUint(alphaMap);
    uint8* alpha = alphaMapAsUint.isEmpty() ? nullptr : (uint8*)alphaMapAsUint.getDataPointer();

    ompIndexType imageCounter = image.height() * image.width();
    OMP_PARALLEL_FOR_LOOP(imageCounter)
    for (ompIndexType idx = 0; idx < imageCounter; idx++) {
        ompIndexType row = idx % image.height();
        ompIndexType col = idx / image.height();
        uchar* p = image.scanLine(row);
        p[col] = data[row + col * image.height()];
    }

    if (alpha) {
        QImage qAlpha(int(A.getColumns()), int(A.getRows()), QImage::Format_Indexed8);
        ompIndexType imageCounter = qAlpha.height() * qAlpha.width();
        OMP_PARALLEL_FOR_LOOP(imageCounter)
        for (ompIndexType i = 0; i < imageCounter; i++) {
            ompIndexType row = i / qAlpha.width();
            ompIndexType col = i % qAlpha.width();
            uchar* p = qAlpha.scanLine(row);
            p[col] = alpha[row + col * image.height()];
        }
        image.setAlphaChannel(qAlpha);
    }

    if (colors) {
        QVector<QRgb> colorVector(int(colorMapAsUint.getElementCount() / 3));
        int numcol = colorVector.size();
        OMP_PARALLEL_FOR_LOOP(numcol)
        for (int i = 0; i < numcol; i++) {
            colorVector[i]
                = qRgb(int(colors[i]), int(colors[i + numcol]), int(colors[i + 2 * numcol]));
        }
        image.setColorTable(colorVector);
    } else {
        int numrow = 256;
        QVector<QRgb> colorVector(numrow);
        OMP_PARALLEL_FOR_LOOP(numrow)
        for (int i = 0; i < numrow; i++) {
            colorVector[i] = qRgb(i, i, i);
        }
        image.setColorTable(colorVector);
    }
    return image;
}
//=============================================================================
template <class T>
static void
toUint8(const ArrayOf& A)
{
    T* ptrA = (T*)A.getDataPointer();
    ompIndexType elementCount = A.getElementCount();
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; ++k) {
        ptrA[k] = ptrA[k] * 255;
    }
}
//=============================================================================
ArrayOf
convertImageToUint(const ArrayOf& A)
{
    ArrayOf modifiedA(A);
    modifiedA.ensureSingleOwner();
    switch (A.getDataClass()) {
    case NLS_UINT8: {
        return modifiedA;
    } break;
    case NLS_LOGICAL: {
        modifiedA.promoteType(NLS_UINT8);
    } break;
    case NLS_SCOMPLEX: {
        modifiedA = RealPart(A);
        toUint8<single>(modifiedA);
        modifiedA.promoteType(NLS_UINT8);
    } break;
    case NLS_DCOMPLEX: {
        modifiedA = RealPart(A);
        toUint8<double>(modifiedA);
        modifiedA.promoteType(NLS_UINT8);
    } break;
    case NLS_SINGLE: {
        toUint8<single>(modifiedA);
        modifiedA.promoteType(NLS_UINT8);
    } break;
    case NLS_DOUBLE: {
        toUint8<double>(modifiedA);
        modifiedA.promoteType(NLS_UINT8);
    } break;
    default: {
    } break;
    }
    return modifiedA;
}
//=============================================================================
void
setTextInfo(QImageWriter& qimagewriter, const std::map<std::wstring, wstringVector>& nameValue)
{
    for (auto element : nameValue) {
        std::wstring name = element.first;
        wstringVector value = element.second;
        std::wstring line;
        bool bFirst = true;
        for (auto s : value) {
            if (bFirst) {
                line = s + L"\n";
                bFirst = false;
            } else {
                line = line + L"\n" + s;
            }
        }
        qimagewriter.setText(wstringToQString(name), wstringToQString(line));
    }
}
//=============================================================================
}
//=============================================================================
