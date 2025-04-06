//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "ImageFormats.hpp"
#include "nlsBuildConfig.h"
#include <iostream>
#include <unordered_map>
#include <tuple>
#include <algorithm>
#include <mutex>
#include <QtCore/QBuffer>
#include <unordered_set>
#include <QtGui/QImageWriter>
#include <QtGui/QImageReader>
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Format data structure: {extension, isa, info, reader, writer, alpha, description, multipage}
using FormatData = std::tuple<std::string, std::string, std::string, std::string, std::string, bool,
    std::string, bool>;
using FormatMap = std::unordered_map<std::string, FormatData>;
//=============================================================================
// Static data
static FormatMap fileExtensions;
static std::once_flag initFlag;
//=============================================================================
// Image format descriptions
static const std::unordered_map<std::string, std::string> imageDescriptions = {
    { "bmp", "Windows Bitmap" },
    { "cur", "Windows Cursor resources" },
    { "fts", "Flexible Image Transport System" },
    { "fits", "Flexible Image Transport System" },
    { "gif", "Graphics Interchange Format" },
    { "hdf", "Hierarchical Data Format" },
    { "ico", "Windows Icon resources" },
    { "j2c", "JPEG 2000 (raw codestream)" },
    { "j2k", "JPEG 2000 (raw codestream)" },
    { "jp2", "JPEG 2000 (Part 1)" },
    { "jpg", "Joint Photographic Experts Group" },
    { "jpeg", "Joint Photographic Experts Group" },
    { "pbm", "Portable Bitmap" },
    { "pcx", "Windows Paintbrush" },
    { "pgm", "Portable Graymap" },
    { "png", "Portable Network Graphics" },
    { "pnm", "Portable Any Map" },
    { "ppm", "Portable Pixmap" },
    { "ras", "Sun Raster" },
    { "svg", "Scalable Vector Graphics" },
    { "svgz", "Compressed Scalable Vector Graphics" },
    { "svs", "Aperio ScanScope Virtual Slide" },
    { "tif", "Tagged Image File Format" },
    { "tiff", "Tagged Image File Format" },
    { "xwd", "X Window Dump" },
    { "xbm", "X Bitmap (X11)" },
    { "xpm", "X Pixmap (X11)" },
};
//=============================================================================
static std::string
normalizeFormat(const std::wstring& format)
{
    std::string formatLower = wstring_to_utf8(format);
    std::transform(formatLower.begin(), formatLower.end(), formatLower.begin(),
        [](unsigned char c) { return std::tolower(c); });
    return formatLower;
}
//=============================================================================
static std::string
getFormatDescription(const std::string& ext)
{
    auto it = imageDescriptions.find(ext);
    if (it != imageDescriptions.end()) {
        return it->second;
    }
    return ext + " Image Format";
}
//=============================================================================
static void
detectAlphaSupport()
{
    // Formats that are known to support alpha channels
    std::unordered_set<std::string> formatsWithAlpha = { "cur", "ico", "png" };
    std::unordered_set<std::string> formatsWithouthAlpha = { "jpg", "jpeg", "j2k", "j2c", "jp2" };

    // Test reading alpha support
    for (auto& pair : fileExtensions) {
        printf("Testing format: %s\n", pair.first.c_str());
        std::string format = pair.first;
        FormatData& formatData = pair.second;

        if (formatsWithouthAlpha.find(format) != formatsWithouthAlpha.end()) {
            continue;
        }
        // Check if this format is in our known list
        bool hasAlpha = formatsWithAlpha.find(format) != formatsWithAlpha.end();

        // For formats not in our list, we can test with QImageReader/QImageWriter
        if (!hasAlpha) {
            // Create a test image with alpha channel
            QImage testImage(10, 10, QImage::Format_ARGB32);
            testImage.fill(Qt::transparent); // Fill with transparent color

            // Test if the format supports writing with alpha
            if (!std::get<4>(formatData).empty()) {
                QBuffer buffer;
                buffer.open(QIODevice::WriteOnly);
                QImageWriter writer(&buffer, format.c_str());
                bool success = writer.write(testImage);

                // If writing succeeded, check if alpha was preserved
                if (success) {
                    buffer.seek(0);
                    QImageReader reader(&buffer, format.c_str());
                    QImage readImage = reader.read();

                    if (!readImage.isNull() && readImage.hasAlphaChannel()) {
                        hasAlpha = true;
                    }
                }
            }
        }

        // Update the alpha flag in the format data
        std::get<5>(formatData) = hasAlpha;
    }
}
//=============================================================================
static void
initializeImageFormats()
{
    // Register writable formats
    for (const QByteArray& format : QImageWriter::supportedImageFormats()) {
        std::string ext = format.toStdString();
        std::string description = getFormatDescription(ext);
        fileExtensions[ext] = { ext, "", "", "", "imwrite", false, description, false };
    }

    // Register readable formats, preserving writable status if already registered
    for (const QByteArray& format : QImageReader::supportedImageFormats()) {
        std::string ext = format.toStdString();
        std::string description = getFormatDescription(ext);
        auto it = fileExtensions.find(ext);

        if (it == fileExtensions.end()) {
            fileExtensions[ext] = { ext, "", "", "imread", "", false, description, false };
        } else {
            fileExtensions[ext] = { std::get<0>(it->second), std::get<1>(it->second),
                std::get<2>(it->second), "imread", std::get<4>(it->second), std::get<5>(it->second),
                std::get<6>(it->second), std::get<7>(it->second) };
        }
    }

#if WITH_GIF
    // Special handling for GIF format if WITH_GIF is defined
    std::string ext = "gif";
    std::string description = getFormatDescription(ext);
    auto it = fileExtensions.find(ext);

    if (it == fileExtensions.end()) {
        fileExtensions[ext] = { ext, "", "", "", "imwrite", false, description, true };
    } else {
        fileExtensions[ext]
            = { std::get<0>(it->second), std::get<1>(it->second), std::get<2>(it->second),
                  std::get<3>(it->second), "imwrite", false, std::get<6>(it->second), true };
    }
#endif
    fileExtensions["pcx"]
        = { "pcx", "", "", "imread", "imwrite", false, getFormatDescription("pcx"), false };

#if WITH_TIFF
    fileExtensions["tiff"]
        = { "tiff", "", "", "imread", "imwrite", false, getFormatDescription("tiff"), false };

#endif

    detectAlphaSupport();
}
//=============================================================================
static void
ensureInitialized()
{
    std::call_once(initFlag, initializeImageFormats);
}
//=============================================================================
static const FormatData*
getFormatData(const std::string& formatLower)
{
    auto it = fileExtensions.find(formatLower);
    if (it != fileExtensions.end()) {
        return &(it->second);
    }
    return nullptr;
}
//=============================================================================
ArrayOf
imageSupport(Evaluator* eval)
{
    ensureInitialized();
    stringVector fieldnames
        = { "ext", "isa", "info", "read", "write", "alpha", "description", "multipage" };
    ArrayOfVector fieldvalues;

    ArrayOfVector exts;
    ArrayOfVector isas;
    ArrayOfVector infos;
    ArrayOfVector reads;
    ArrayOfVector writes;
    ArrayOfVector alphas;
    ArrayOfVector descriptions;
    ArrayOfVector multipages;

    for (const auto& entry : fileExtensions) {
        const FormatData& formatData = entry.second;
        exts << ArrayOf::characterArrayConstructor(std::get<0>(formatData));
        isas << ArrayOf::characterArrayConstructor(std::get<1>(formatData));
        infos << ArrayOf::characterArrayConstructor(std::get<2>(formatData));
        reads << ArrayOf::characterArrayConstructor(std::get<3>(formatData));
        writes << ArrayOf::characterArrayConstructor(std::get<4>(formatData));
        alphas << ArrayOf::logicalConstructor(std::get<5>(formatData));
        descriptions << ArrayOf::characterArrayConstructor(std::get<6>(formatData));
        multipages << ArrayOf::logicalConstructor(std::get<7>(formatData));
    }
    Dimensions dims;
    dims[0] = 1;
    dims[1] = exts.size();

    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames, false));
    ArrayOf st = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
    st.setFieldAsList("ext", exts);
    st.setFieldAsList("isa", isas);
    st.setFieldAsList("info", infos);
    st.setFieldAsList("read", reads);
    st.setFieldAsList("write", writes);
    st.setFieldAsList("alpha", alphas);
    st.setFieldAsList("description", descriptions);
    st.setFieldAsList("multipage", multipages);

    return st;
}
//=============================================================================
ArrayOf
imageSupport(Evaluator* eval, const std::wstring& format)
{
    ensureInitialized();
    std::string formatLower = normalizeFormat(format);

    const FormatData* formatData = getFormatData(formatLower);
    if (formatData) {
        // Create struct with format information
        // {extension, isa, info, reader, writer, alpha, description, multipage}

        stringVector fieldnames
            = { "ext", "isa", "info", "read", "write", "alpha", "description", "multipage" };
        ArrayOfVector fieldvalues;

        std::string ext = std::get<0>(*formatData);
        fieldvalues << ArrayOf::characterArrayConstructor(ext);

        auto createFunctionHandleOrEmpty = [](Evaluator* eval, const std::string& str) {
            if (str.empty()) {
                return ArrayOf::emptyConstructor(0, 0);
            }
            FunctionDef* funcDef = nullptr;
            std::string str2func = "str2func";
            if (eval->getContext()->lookupFunction(str2func, funcDef)) {
                ArrayOfVector argsIn;
                argsIn << ArrayOf::characterArrayConstructor(str);
                ArrayOfVector resVect = funcDef->evaluateFunction(eval, argsIn, 1);
                return resVect[0];
            }
            return ArrayOf::emptyConstructor(0, 0);
        };
        std::string isa = std::get<1>(*formatData);
        fieldvalues << createFunctionHandleOrEmpty(eval, isa);

        std::string info = std::get<2>(*formatData);
        fieldvalues << createFunctionHandleOrEmpty(eval, info);

        std::string read = std::get<3>(*formatData);
        fieldvalues << createFunctionHandleOrEmpty(eval, read);

        std::string write = std::get<4>(*formatData);
        fieldvalues << createFunctionHandleOrEmpty(eval, write);

        bool alpha = std::get<5>(*formatData);
        fieldvalues << ArrayOf::logicalConstructor(alpha);

        std::string description = std::get<6>(*formatData);
        fieldvalues << ArrayOf::characterArrayConstructor(description);

        bool multipage = std::get<7>(*formatData);
        fieldvalues << ArrayOf::logicalConstructor(multipage);

        return ArrayOf::structScalarConstructor(fieldnames, fieldvalues);
    } else {
        // Return empty struct if format not found
        Dimensions dims(0, 0);
        return ArrayOf::emptyStructConstructor(stringVector(), dims);
    }
}
//=============================================================================
bool
isImageFormatReadable(const std::wstring& format)
{
    ensureInitialized();
    std::string formatLower = normalizeFormat(format);

    const FormatData* formatData = getFormatData(formatLower);
    return formatData ? !std::get<3>(*formatData).empty() : false;
}
//=============================================================================
NLSGRAPHICS_IO_IMPEXP bool
isImageFormatWritable(const std::wstring& format)
{
    ensureInitialized();
    std::string formatLower = normalizeFormat(format);
    const FormatData* formatData = getFormatData(formatLower);
    return formatData ? !std::get<4>(*formatData).empty() : false;
}
//=============================================================================
ArrayOf
imageFormatDisplay(Evaluator* eval)
{
    if (!eval) {
        return {};
    }
    Interface* io = eval->getInterface();

    ensureInitialized();

    // Define column headers
    std::string headerExt = "EXT";
    std::string headerIsa = "ISA";
    std::string headerInfo = "INFO";
    std::string headerRead = "READ";
    std::string headerWrite = "WRITE";
    std::string headerAlpha = "ALPHA";
    std::string headerMulti = "MULTIPAGE";
    std::string headerDesc = "DESCRIPTION";

    // Determine column widths for alignment
    size_t widthExt = headerExt.length();
    size_t widthIsa = headerIsa.length();
    size_t widthInfo = headerInfo.length();
    size_t widthRead = headerRead.length();
    size_t widthWrite = headerWrite.length();
    size_t widthAlpha = headerAlpha.length();
    size_t widthDesc = headerDesc.length();
    size_t widthMulti = headerMulti.length();

    // Calculate maximum width for each column
    for (const auto& entry : fileExtensions) {
        const FormatData& formatData = entry.second;

        widthExt = std::max(widthExt, std::get<0>(formatData).length());
        widthIsa = std::max(widthIsa, std::get<1>(formatData).length());
        widthInfo = std::max(widthInfo, std::get<2>(formatData).length());
        widthRead = std::max(widthRead, std::get<3>(formatData).length());
        widthWrite = std::max(widthWrite, std::get<4>(formatData).length());
        widthDesc = std::max(widthDesc, std::get<6>(formatData).length());
    }

    // Add some padding
    widthExt += 2;
    widthIsa += 2;
    widthInfo += 2;
    widthRead += 2;
    widthWrite += 2;
    widthAlpha += 2;
    widthDesc += 2;
    widthMulti += 2;

    // Format and print the header
    io->outputMessage("\n");
    io->outputMessage(fmt::format("{:<{}} {:<{}} {:<{}} {:<{}} {:<{}} {:<{}} {:<{}} {:<{}}\n",
        headerExt, widthExt, headerIsa, widthIsa, headerInfo, widthInfo, headerRead, widthRead,
        headerWrite, widthWrite, headerAlpha, widthAlpha, headerMulti, widthMulti, headerDesc,
        widthDesc));

    // Print separator line
    std::string separator(widthExt + widthIsa + widthInfo + widthRead + widthWrite + widthAlpha
            + widthDesc + widthMulti + 7,
        '-');
    io->outputMessage(separator + "\n");

    // Sort the formats alphabetically by extension
    std::vector<std::string> sortedKeys;
    for (const auto& entry : fileExtensions) {
        sortedKeys.push_back(entry.first);
    }
    std::sort(sortedKeys.begin(), sortedKeys.end());

    // Print each format's data
    for (const auto& key : sortedKeys) {
        const FormatData& formatData = fileExtensions[key];

        std::string ext = std::get<0>(formatData);
        std::string isa = std::get<1>(formatData);
        std::string info = std::get<2>(formatData);
        std::string read = std::get<3>(formatData);
        std::string write = std::get<4>(formatData);
        bool alpha = std::get<5>(formatData);
        std::string description = std::get<6>(formatData);
        bool multipage = std::get<7>(formatData);

        io->outputMessage(fmt::format("{:<{}} {:<{}} {:<{}} {:<{}} {:<{}} {:<{}} {:<{}} {:<{}}\n",
            ext, widthExt, isa.empty() ? "-" : isa, widthIsa, info.empty() ? "-" : info, widthInfo,
            read.empty() ? "-" : read, widthRead, write.empty() ? "-" : write, widthWrite,
            alpha ? "true" : "false", widthAlpha, multipage ? "true" : "false", widthMulti,
            description, widthDesc));
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
