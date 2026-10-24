//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ImageWriter.hpp"
#include "RasterCodec.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "RealPart.hpp"
#include "StringHelpers.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    template <class T>
    void
    scaleNormalized(ArrayOf& array)
    {
        auto* values = const_cast<T*>(static_cast<const T*>(array.dataPointer()));
        for (indexType i = 0; i < array.elementCount(); ++i) {
            values[i] = std::clamp<T>(values[i], 0, 1) * 255;
        }
    }
    //=============================================================================
    ArrayOf
    asUint8(const ArrayOf& source)
    {
        ArrayOf result(source);
        result.ensureSingleOwner();
        switch (result.dataClass()) {
        case NLS_UINT8:
            break;
        case NLS_LOGICAL:
            result.promoteType(NLS_UINT8);
            for (indexType i = 0; i < result.elementCount(); ++i) {
                const_cast<uint8*>(static_cast<const uint8*>(result.dataPointer()))[i] *= 255;
            }
            break;
        case NLS_SCOMPLEX:
            result = RealPart(result);
            scaleNormalized<single>(result);
            result.promoteType(NLS_UINT8);
            break;
        case NLS_DCOMPLEX:
            result = RealPart(result);
            scaleNormalized<double>(result);
            result.promoteType(NLS_UINT8);
            break;
        case NLS_SINGLE:
            scaleNormalized<single>(result);
            result.promoteType(NLS_UINT8);
            break;
        case NLS_DOUBLE:
            scaleNormalized<double>(result);
            result.promoteType(NLS_UINT8);
            break;
        default:
            result.promoteType(NLS_UINT8);
            break;
        }
        return result;
    }
    //=============================================================================
    RasterImage
    indexedRaster(const ArrayOf& source, const ArrayOf& map, const ArrayOf& alpha)
    {
        RasterImage image;
        image.width = static_cast<int>(source.columns());
        image.height = static_cast<int>(source.rows());
        image.format = RasterPixelFormat::Indexed8;
        ArrayOf indices = asUint8(source);
        const auto* input = static_cast<const uint8*>(indices.dataPointer());
        image.pixels.resize(static_cast<size_t>(image.width) * image.height);
        for (int y = 0; y < image.height; ++y) {
            for (int x = 0; x < image.width; ++x) {
                image.pixels[static_cast<size_t>(y) * image.width + x]
                    = input[y + static_cast<size_t>(x) * image.height];
            }
        }
        ArrayOf colors = asUint8(map);
        const auto* colorData = static_cast<const uint8*>(colors.dataPointer());
        const size_t colorCount = map.isEmpty() ? 256 : static_cast<size_t>(map.rows());
        image.palette.resize(colorCount * 4, 255);
        for (size_t i = 0; i < colorCount; ++i) {
            if (map.isEmpty()) {
                image.palette[i * 4] = image.palette[i * 4 + 1] = image.palette[i * 4 + 2]
                    = static_cast<uint8_t>(i);
            } else {
                image.palette[i * 4] = colorData[i];
                image.palette[i * 4 + 1] = colorData[i + colorCount];
                image.palette[i * 4 + 2] = colorData[i + colorCount * 2];
            }
        }
        if (!alpha.isEmpty()) {
            ArrayOf alphaBytes = asUint8(alpha);
            const auto* alphaData = static_cast<const uint8*>(alphaBytes.dataPointer());
            std::vector<int> paletteAlpha(colorCount, -1);
            bool paletteCompatible = true;
            for (size_t i = 0; i < image.pixels.size(); ++i) {
                const int y = static_cast<int>(i / image.width);
                const int x = static_cast<int>(i % image.width);
                const size_t color = image.pixels[i];
                const int value = alphaData[y + static_cast<size_t>(x) * image.height];
                if (color >= paletteAlpha.size()
                    || (paletteAlpha[color] >= 0 && paletteAlpha[color] != value)) {
                    paletteCompatible = false;
                    break;
                }
                paletteAlpha[color] = value;
            }
            if (paletteCompatible) {
                for (size_t color = 0; color < paletteAlpha.size(); ++color) {
                    if (paletteAlpha[color] >= 0) {
                        image.palette[color * 4 + 3] = static_cast<uint8_t>(paletteAlpha[color]);
                    }
                }
                return image;
            }
            RasterImage rgba;
            rgba.width = image.width;
            rgba.height = image.height;
            rgba.format = RasterPixelFormat::RGBA8;
            rgba.pixels.resize(image.pixels.size() * 4);
            for (size_t i = 0; i < image.pixels.size(); ++i) {
                const int y = static_cast<int>(i / image.width);
                const int x = static_cast<int>(i % image.width);
                const size_t paletteOffset = static_cast<size_t>(image.pixels[i]) * 4;
                rgba.pixels[i * 4] = image.palette[paletteOffset];
                rgba.pixels[i * 4 + 1] = image.palette[paletteOffset + 1];
                rgba.pixels[i * 4 + 2] = image.palette[paletteOffset + 2];
                rgba.pixels[i * 4 + 3] = alphaData[y + static_cast<size_t>(x) * image.height];
            }
            return rgba;
        }
        return image;
    }
    //=============================================================================
    RasterImage
    directRaster(const ArrayOf& source, const ArrayOf& alpha)
    {
        RasterImage image;
        image.width = static_cast<int>(source.columns());
        image.height = static_cast<int>(source.rows());
        if (source.dataClass() == NLS_UINT16 && source.dimensions().length() == 2) {
            if (!alpha.isEmpty()) {
                Error(L"Nelson:imwrite:alphaUint16Unsupported",
                    L"Alpha is not supported with uint16 image data.");
            }
            image.format = RasterPixelFormat::Gray16;
            image.pixels.resize(static_cast<size_t>(image.width) * image.height * 2);
            const auto* input = static_cast<const uint16*>(source.dataPointer());
            auto* output = reinterpret_cast<uint16_t*>(image.pixels.data());
            for (int y = 0; y < image.height; ++y) {
                for (int x = 0; x < image.width; ++x) {
                    output[static_cast<size_t>(y) * image.width + x]
                        = input[y + static_cast<size_t>(x) * image.height];
                }
            }
            return image;
        }
        ArrayOf bytes = asUint8(source);
        const auto* input = static_cast<const uint8*>(bytes.dataPointer());
        const bool gray = source.dimensions().length() == 2;
        const bool withAlpha = !alpha.isEmpty();
        image.format = withAlpha ? RasterPixelFormat::RGBA8
                                 : (gray ? RasterPixelFormat::Gray8 : RasterPixelFormat::RGB8);
        image.pixels.resize(static_cast<size_t>(image.width) * image.height * image.channels());
        ArrayOf alphaBytes = alpha.isEmpty() ? ArrayOf::empty(0, 0) : asUint8(alpha);
        const auto* alphaData
            = alpha.isEmpty() ? nullptr : static_cast<const uint8*>(alphaBytes.dataPointer());
        const size_t slice = static_cast<size_t>(image.width) * image.height;
        for (int y = 0; y < image.height; ++y) {
            for (int x = 0; x < image.width; ++x) {
                const size_t columnMajor = y + static_cast<size_t>(x) * image.height;
                const size_t rowMajor = static_cast<size_t>(y) * image.width + x;
                if (gray) {
                    if (withAlpha) {
                        image.pixels[rowMajor * 4] = input[columnMajor];
                        image.pixels[rowMajor * 4 + 1] = input[columnMajor];
                        image.pixels[rowMajor * 4 + 2] = input[columnMajor];
                        image.pixels[rowMajor * 4 + 3] = alphaData[columnMajor];
                    } else {
                        image.pixels[rowMajor] = input[columnMajor];
                    }
                } else {
                    image.pixels[rowMajor * image.channels()] = input[columnMajor];
                    image.pixels[rowMajor * image.channels() + 1] = input[columnMajor + slice];
                    image.pixels[rowMajor * image.channels() + 2] = input[columnMajor + slice * 2];
                    if (withAlpha) {
                        image.pixels[rowMajor * 4 + 3] = alphaData[columnMajor];
                    }
                }
            }
        }
        return image;
    }
    //=============================================================================
    RasterImage
    toRaster(const ArrayOf& source, const ArrayOf& map, const ArrayOf& alpha)
    {
        return source.dimensions().length() == 2 && !map.isEmpty()
            ? indexedRaster(source, map, alpha)
            : directRaster(source, alpha);
    }
    //=============================================================================
    RasterImage
    withMetadata(RasterImage image, const std::map<std::wstring, wstringVector>& nameValue)
    {
        for (const auto& item : nameValue) {
            if (item.second.empty()) {
                continue;
            }
            std::string key;
            if (StringHelpers::iequals(item.first, L"Comment")
                || StringHelpers::iequals(item.first, L"Comments")) {
                key = "Comment";
            } else if (StringHelpers::iequals(item.first, L"Author")) {
                key = "Author";
            }
            if (!key.empty()) {
                std::wstring value;
                for (const std::wstring& line : item.second) {
                    if (!value.empty()) {
                        value += L'\n';
                    }
                    value += line;
                }
                image.metadata[key] = wstring_to_utf8(value);
            }
        }
        return image;
    }
    //=============================================================================
    void
    writeRasterOrError(const std::wstring& filename, const std::wstring& format,
        const RasterImage& image, int quality)
    {
        std::string error;
        if (!writeRasterImageAtomic(filename, wstring_to_utf8(format), image, quality, error)) {
            Error(L"Nelson:imwrite:cannotWriteFile", L"Cannot save image file: %s (%s)", filename,
                utf8_to_wstring(error));
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
void
writeGif(const std::wstring& filename, bool append, const ArrayOf& source, const ArrayOf& map,
    const ArrayOf& alpha, int, int delayTime, int loopCount,
    const std::map<std::wstring, wstringVector>& nameValue)
{
    RasterImage image = withMetadata(toRaster(source, map, alpha), nameValue);
    std::string error;
#ifdef NELSON_GRAPHICS_IO_STB_ONLY
    (void)append;
    (void)delayTime;
    (void)loopCount;
    error = "GIF writing is unavailable in this build";
    Error(L"Nelson:imwrite:cannotWriteFile", L"Cannot save image file: %s (%s)", filename,
        utf8_to_wstring(error));
#else
    if (!writeRasterGif(
            filename, image, append, delayTime < 0 ? 1000 : delayTime, loopCount, error)) {
        Error(L"Nelson:imwrite:cannotWriteFile", L"Cannot save image file: %s (%s)", filename,
            utf8_to_wstring(error));
    }
#endif
}
//=============================================================================
void
writeTiff(const std::wstring& filename, const ArrayOf& source, const ArrayOf& map,
    const ArrayOf& alpha, const std::map<std::wstring, wstringVector>& nameValue)
{
    writeRasterOrError(
        filename, L"tiff", withMetadata(toRaster(source, map, alpha), nameValue), DEFAULT_QUALITY);
}
//=============================================================================
void
writePcx(const std::wstring& filename, const ArrayOf& source, const ArrayOf& map,
    const ArrayOf& alpha, int quality, const std::map<std::wstring, wstringVector>& nameValue)
{
    writeRasterOrError(
        filename, L"pcx", withMetadata(toRaster(source, map, alpha), nameValue), quality);
}
//=============================================================================
void
writeImage(const std::wstring& filename, const ArrayOf& source, const ArrayOf& map,
    const std::wstring& format, const ArrayOf& alpha, int quality,
    const std::map<std::wstring, wstringVector>& nameValue)
{
    writeRasterOrError(
        filename, format, withMetadata(toRaster(source, map, alpha), nameValue), quality);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
