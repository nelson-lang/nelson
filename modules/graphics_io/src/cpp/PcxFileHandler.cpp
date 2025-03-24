//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QDataStream>
#include "PcxFileHandler.hpp"
//=============================================================================
// basic implementation of PCX file format reader/writer
// based on the PCX file format specification at
// https://web.archive.org/web/20030111010058/http://www.nist.fss.ru/hr/doc/spec/pcx.htm
//=============================================================================
// PCX file header structure
#pragma pack(push, 1) // Ensures exact field alignment
struct PCXHeader
{
    uchar Manufacturer;
    uchar Version;
    uchar Encoding;
    uchar BitsPerPixel;
    short Xmin;
    short Ymin;
    short Xmax;
    short Ymax;
    short Hdpi;
    short Vdpi;
    uchar ColorMap[16][3];
    uchar Reserved;
    uchar Nplanes;
    short BytesPerLine;
    uchar filler[60];
};
#pragma pack(pop) // Restores default alignment
//=============================================================================
// PCX error codes
enum PCXErrorCode
{
    PCX_ERROR_NONE = 0,
    PCX_ERROR_OPENING,
    PCX_ERROR_READING,
    PCX_ERROR_WRITING,
    PCX_ERROR_NO_HEADER,
    PCX_ERROR_WRONG_VERSION,
    PCX_ERROR_NO_PALETTE,
    PCX_ERROR_MEMORY
};
//=============================================================================
QImage
PcxFileHandler::loadPCX(const QString& filePath)
{
    QFile file(filePath);
    if (!file.open(QIODevice::ReadOnly)) {
        // Failed to open file for reading
        return QImage();
    }

    PCXHeader header;
    if (file.read(reinterpret_cast<char*>(&header), sizeof(PCXHeader)) != sizeof(PCXHeader)) {
        // Failed to read PCX header.
        return QImage();
    }

    // Verify that the file is valid (check manufacturer and encoding)
    if (header.Manufacturer != 0x0A || header.Encoding != 1) {
        // Not a valid PCX file.
        return QImage();
    }

    int width = header.Xmax - header.Xmin + 1;
    int height = header.Ymax - header.Ymin + 1;
    QImage image(width, height, QImage::Format_RGB888);

    QByteArray imageData = file.readAll();
    int dataIndex = 0;

    if (header.BitsPerPixel == 1 && header.Nplanes == 1) {
        // Handle 1-bit monochrome PCX
        QVector<QRgb> monoPalette = { qRgb(0, 0, 0), qRgb(255, 255, 255) };

        const int bytesPerLine = header.BytesPerLine;

        for (int y = 0; y < height; ++y) {
            int bytesRead = 0;
            int x = 0;

            // Process one scan line
            while (bytesRead < bytesPerLine && dataIndex < imageData.size()) {
                quint8 runCount, value;
                quint8 byte = static_cast<quint8>(imageData[dataIndex++]);

                // Check if this is a run-length encoded byte
                if ((byte & 0xC0) == 0xC0) {
                    runCount = byte & 0x3F;
                    if (dataIndex >= imageData.size()) {
                        // Unexpected end of file during RLE decoding.
                        return QImage();
                    }
                    value = static_cast<quint8>(imageData[dataIndex++]);
                    bytesRead += runCount;
                } else {
                    runCount = 1;
                    value = byte;
                    bytesRead += 1;
                }

                // Process each bit in the decoded byte(s)
                for (int run = 0; run < runCount; ++run) {
                    for (int bit = 7; bit >= 0 && x < width; --bit) {
                        int colorIndex = (value >> bit) & 1;
                        image.setPixelColor(x++, y, monoPalette[colorIndex]);
                    }
                }
            }

            // Skip any padding at the end of the scan line
            if (x < width) {
                // Warning: Incomplete scan line at row << y;
            }
        }
    } else if (header.BitsPerPixel == 4 && header.Nplanes == 1) {
        // Handle 4-bit PCX with 16-color palette
        QVector<QRgb> palette(16);
        for (int i = 0; i < 16; ++i) {
            palette[i] = qRgb(header.ColorMap[i][0], header.ColorMap[i][1], header.ColorMap[i][2]);
        }

        for (int y = 0; y < height; ++y) {
            int x = 0;
            while (x < width) {
                if (dataIndex >= imageData.size()) {
                    // Out of bounds access in imageData
                    return QImage();
                }
                quint8 byte = static_cast<quint8>(imageData[dataIndex++]);
                for (int nibble = 1; nibble >= 0 && x < width; --nibble) {
                    int colorIndex = (byte >> (nibble * 4)) & 0x0F;
                    image.setPixelColor(x++, y, palette[colorIndex]);
                }
            }
        }
    } else if (header.BitsPerPixel == 8 && header.Nplanes == 1) {
        // Handle 8-bit PCX with 256-color palette
        QVector<QRgb> palette(256);
        file.seek(file.size() - 769); // Palette is stored at the end of the file
        char paletteMarker;
        file.read(&paletteMarker, 1);

        if (paletteMarker == 0x0C) {
            char paletteData[768];
            file.read(paletteData, 768);
            for (int i = 0; i < 256; ++i) {
                palette[i] = qRgb(static_cast<quint8>(paletteData[i * 3]),
                    static_cast<quint8>(paletteData[i * 3 + 1]),
                    static_cast<quint8>(paletteData[i * 3 + 2]));
            }
        }

        for (int y = 0; y < height; ++y) {
            int x = 0;
            while (x < width) {
                if (dataIndex >= imageData.size()) {
                    // Out of bounds access in imageData!
                    return QImage();
                }
                quint8 byte = static_cast<quint8>(imageData[dataIndex++]);
                if ((byte & 0xC0) == 0xC0) {
                    int count = byte & 0x3F;
                    quint8 value = static_cast<quint8>(imageData[dataIndex++]);
                    for (int i = 0; i < count; ++i) {
                        if (x < width)
                            image.setPixelColor(x++, y, palette[value]);
                    }
                } else {
                    image.setPixelColor(x++, y, palette[byte]);
                }
            }
        }
    } else if (header.BitsPerPixel == 8 && header.Nplanes == 3) {
        // Handle 24-bit true color PCX
        for (int y = 0; y < height; ++y) {
            QVector<quint8> red(width), green(width), blue(width);
            int x = 0;

            for (int p = 0; p < 3; ++p) { // Read R, G, B planes separately
                x = 0;
                while (x < width) {
                    if (dataIndex >= imageData.size()) {
                        // Out of bounds access in imageData
                        return QImage();
                    }
                    quint8 byte = static_cast<quint8>(imageData[dataIndex++]);
                    if ((byte & 0xC0) == 0xC0) {
                        int count = byte & 0x3F;
                        quint8 value = static_cast<quint8>(imageData[dataIndex++]);
                        for (int i = 0; i < count; ++i) {
                            if (x < width) {
                                if (p == 0)
                                    red[x] = value;
                                else if (p == 1)
                                    green[x] = value;
                                else
                                    blue[x] = value;
                                x++;
                            }
                        }
                    } else {
                        if (p == 0)
                            red[x] = byte;
                        else if (p == 1)
                            green[x] = byte;
                        else
                            blue[x] = byte;
                        x++;
                    }
                }
            }

            for (x = 0; x < width; ++x) {
                image.setPixelColor(x, y, qRgb(red[x], green[x], blue[x]));
            }
        }
    }

    return image;
}
//=============================================================================
int
PcxFileHandler::pcxEncodeLine(QFile& file, const QVector<quint8>& data, int length)
{
    if (length <= 0) {
        return 0;
    }

    int total = 0;
    quint8 runCount = 1;
    quint8 last = data[0];

    for (int i = 1; i < length; i++) {
        quint8 curr = data[i];

        if (curr == last) {
            runCount++;
            if (runCount == 63) {
                int bytes = pcxEncodeByte(file, last, runCount);
                if (bytes == 0) {
                    return 0; // Error writing
                }
                total += bytes;
                runCount = 0;
            }
        } else {
            if (runCount > 0) {
                int bytes = pcxEncodeByte(file, last, runCount);
                if (bytes == 0) {
                    return 0; // Error writing
                }
                total += bytes;
            }
            last = curr;
            runCount = 1;
        }
    }

    // Handle any remaining run
    if (runCount > 0) {
        int bytes = pcxEncodeByte(file, last, runCount);
        if (bytes == 0) {
            return 0; // Error writing
        }
        total += bytes;
    }

    return total;
}
//=============================================================================
int
PcxFileHandler::pcxEncodeByte(QFile& file, quint8 byte, quint8 count)
{
    if (count == 0) {
        return 0; // No bytes to write
    }

    if (count == 1 && (byte & 0xC0) != 0xC0) {
        // Single byte without the high bits set - write it directly
        if (file.putChar(byte) == false) {
            return 0; // Disk write error
        }
        return 1;
    } else {
        // RLE encode the run
        if (file.putChar(0xC0 | count) == false) {
            return 0; // Disk write error
        }
        if (file.putChar(byte) == false) {
            return 0; // Disk write error
        }
        return 2;
    }
}
//=============================================================================
int
PcxFileHandler::savePCX(const QString& filePath, const QImage& image)
{
    // Save the given QImage as a PCX file at the specified file path.
    // Parameters:
    // - filePath: The path where the PCX file will be saved.
    // - image: The QImage to be saved as a PCX file.

    QFile file(filePath);
    if (!file.open(QIODevice::WriteOnly)) {
        return PCX_ERROR_OPENING; // Error opening the file for writing
    }
    if (image.isNull()) {
        return PCX_ERROR_MEMORY; // Error: the image is null
    }

    QDataStream stream(&file);
    stream.setByteOrder(QDataStream::LittleEndian); // PCX files use little-endian byte order

    // Initialize the PCX file header
    PCXHeader header = {};
    header.Manufacturer = 10; // ZSoft .PCX file identifier
    header.Version = 5; // Version number
    header.Encoding = 1; // 1 = RLE encoding
    header.Xmin = 0;
    header.Ymin = 0;
    header.Xmax = image.width() - 1;
    header.Ymax = image.height() - 1;
    header.Hdpi = 300;
    header.Vdpi = 300;
    header.Reserved = 0;
    memset(header.filler, 0, sizeof(header.filler));

    QImage convertedImage = image;
    QVector<QRgb> palette;

    if (image.isGrayscale()) {
        if (image.format() != QImage::Format_Mono && image.format() != QImage::Format_MonoLSB) {
            bool isMonochrome = true;
            for (int y = 0; y < image.height(); ++y) {
                for (int x = 0; x < image.width(); ++x) {
                    QRgb pixel = image.pixel(x, y);
                    int grayValue = qGray(pixel);
                    if (grayValue != 0 && grayValue != 255) {
                        isMonochrome = false;
                        break;
                    }
                }
                if (!isMonochrome)
                    break;
            }
            if (isMonochrome) {
                convertedImage = image.convertToFormat(QImage::Format_Mono);
            }
        }
    }
    if (convertedImage.format() == QImage::Format_Indexed8) {
        QImage rgbImage(image.width(), image.height(), QImage::Format_RGB888);

        for (int y = 0; y < image.height(); ++y) {
            for (int x = 0; x < convertedImage.width(); ++x) {
                int index = convertedImage.pixelIndex(x, y);
                QColor color = convertedImage.color(index);
                rgbImage.setPixelColor(x, y, color);
            }
        }
        convertedImage = rgbImage;
        header.BitsPerPixel = 8;
        header.Nplanes = 3;
        header.BytesPerLine = convertedImage.width();
    } else if (convertedImage.format() == QImage::Format_RGB888) {
        header.BitsPerPixel = 8;
        header.Nplanes = 3;
        header.BytesPerLine = convertedImage.width();
    } else if (convertedImage.format() == QImage::Format_Mono) {
        header.BitsPerPixel = 1;
        header.Nplanes = 1;
        header.BytesPerLine = (convertedImage.width() + 7) / 8;
    } else {
        convertedImage = convertedImage.convertToFormat(QImage::Format_RGB888);
        header.BitsPerPixel = 8;
        header.Nplanes = 3;
        header.BytesPerLine = convertedImage.width();
    }

    QByteArray headerData(reinterpret_cast<const char*>(&header), sizeof(PCXHeader));
    stream.writeRawData(headerData.data(), headerData.size());

    if (header.BitsPerPixel == 1 && header.Nplanes == 1) {
        // Handle 1-bit monochrome PCX
        for (int row = 0; row < convertedImage.height(); ++row) {
            QVector<quint8> lineData;
            for (int col = 0; col < convertedImage.width(); col += 8) {
                quint8 byte = 0;
                for (int bit = 0; bit < 8 && col + bit < convertedImage.width(); ++bit) {
                    QRgb pixel = convertedImage.pixel(col + bit, row);
                    if (qGray(pixel) > 127) {
                        byte |= (1 << (7 - bit));
                    }
                }
                lineData.append(byte);
            }
            int bytesWritten = pcxEncodeLine(file, lineData, lineData.size());
            if (bytesWritten == 0) {
                return PCX_ERROR_WRITING;
            }
        }
    } else if (header.BitsPerPixel == 8 && header.Nplanes == 1) {
        for (int row = 0; row < convertedImage.height(); ++row) {
            QVector<quint8> lineData;
            for (int col = 0; col < convertedImage.width(); ++col) {
                QRgb pixel = convertedImage.pixel(col, row);
                quint8 colorIndex = palette.indexOf(pixel);
                if (colorIndex == -1) {
                    // no color found in the palette
                    return PCX_ERROR_WRITING;
                }
                lineData.append(colorIndex);
            }
            int bytesWritten = pcxEncodeLine(file, lineData, lineData.size());
            if (bytesWritten == 0) {
                return PCX_ERROR_WRITING;
            }
        }

        uchar paletteFlag = 12;
        stream.writeRawData(reinterpret_cast<const char*>(&paletteFlag), 1);
        for (int i = 0; i < 256; ++i) {
            QRgb color = palette[i];
            char colorData[3] = { static_cast<char>(qRed(color)), static_cast<char>(qGreen(color)),
                static_cast<char>(qBlue(color)) };
            stream.writeRawData(colorData, 3);
        }
    } else if (header.BitsPerPixel == 8 && header.Nplanes == 3) {
        for (int row = 0; row < convertedImage.height(); ++row) {
            QVector<quint8> redLine, greenLine, blueLine;
            for (int col = 0; col < convertedImage.width(); ++col) {
                QRgb pixel = convertedImage.pixel(col, row);
                redLine.append(qRed(pixel));
                greenLine.append(qGreen(pixel));
                blueLine.append(qBlue(pixel));
            }
            if (pcxEncodeLine(file, redLine, redLine.size()) == 0
                || pcxEncodeLine(file, greenLine, greenLine.size()) == 0
                || pcxEncodeLine(file, blueLine, blueLine.size()) == 0) {
                return PCX_ERROR_WRITING;
            }
        }
    }
    return PCX_ERROR_NONE;
}
//=============================================================================
