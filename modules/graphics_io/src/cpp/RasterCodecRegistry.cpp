//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RasterCodec.hpp"
#include "RasterCodecInternal.hpp"
#include "RasterAtomicFile.hpp"
#include <algorithm>
#include <cctype>
#include <filesystem>
#include <fstream>
#include <iterator>
//=============================================================================
namespace Nelson {
//=============================================================================
const std::vector<RasterFormatInfo>&
rasterFormats()
{
    static const std::vector<RasterFormatInfo> formats = {
        { "png", {}, "Portable Network Graphics", true, true, true, false },
        { "jpg", { "jpeg", "jfif" }, "Joint Photographic Experts Group", true, true, false, false },
        { "gif", {}, "Graphics Interchange Format", true, true, true, true },
        { "webp", {}, "WebP", true, true, true, false },
        { "tiff", { "tif" }, "Tagged Image File Format", true, true, true, false },
        { "bmp", { "dib" }, "Bitmap Image", true, true, true, false },
        { "tga", {}, "Truevision TGA", true, true, true, false },
        { "pbm", {}, "Portable Bitmap", true, true, false, false },
        { "pgm", {}, "Portable Graymap", true, true, false, false },
        { "ppm", {}, "Portable Pixmap", true, true, false, false },
        { "pnm", {}, "Portable Anymap", true, true, false, false },
        { "pcx", {}, "PC Paintbrush", true, true, false, false },
        { "psd", {}, "Photoshop Document", true, false, true, false },
        { "hdr", { "rgbe" }, "Radiance RGBE", true, false, false, false },
        { "pic", {}, "Softimage PIC", true, false, true, false },
    };
    return formats;
}
//=============================================================================
std::string
normalizeRasterFormat(const std::string& format)
{
    std::string normalized = format;
    if (!normalized.empty() && normalized.front() == '.') {
        normalized.erase(normalized.begin());
    }
    std::transform(normalized.begin(), normalized.end(), normalized.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    for (const auto& info : rasterFormats()) {
        if (normalized == info.extension
            || std::find(info.aliases.begin(), info.aliases.end(), normalized)
                != info.aliases.end()) {
            return info.extension;
        }
    }
    return normalized;
}
//=============================================================================
const RasterFormatInfo*
rasterFormat(const std::string& format)
{
    const std::string normalized = normalizeRasterFormat(format);
    for (const auto& info : rasterFormats()) {
        if (info.extension == normalized) {
            return &info;
        }
    }
    return nullptr;
}
//=============================================================================
static bool
startsWith(const std::vector<uint8_t>& b, std::initializer_list<uint8_t> signature)
{
    return b.size() >= signature.size()
        && std::equal(signature.begin(), signature.end(), b.begin());
}
//=============================================================================
std::string
detectRasterFormat(const std::vector<uint8_t>& b, const std::string& extensionHint)
{
    if (startsWith(b, { 0x89, 'P', 'N', 'G', 0x0d, 0x0a, 0x1a, 0x0a })) {
        return "png";
    }
    if (startsWith(b, { 0xff, 0xd8, 0xff })) {
        return "jpg";
    }
    if (startsWith(b, { 'G', 'I', 'F', '8' })) {
        return "gif";
    }
    if (b.size() >= 12 && startsWith(b, { 'R', 'I', 'F', 'F' })
        && std::equal(b.begin() + 8, b.begin() + 12, "WEBP")) {
        return "webp";
    }
    if (startsWith(b, { 'I', 'I', 42, 0 }) || startsWith(b, { 'M', 'M', 0, 42 })) {
        return "tiff";
    }
    if (startsWith(b, { 'B', 'M' })) {
        return "bmp";
    }
    if (startsWith(b, { '8', 'B', 'P', 'S' })) {
        return "psd";
    }
    if (startsWith(b, { '#', '?', 'R', 'A', 'D', 'I', 'A', 'N', 'C', 'E' })
        || startsWith(b, { '#', '?', 'R', 'G', 'B', 'E' })) {
        return "hdr";
    }
    if (startsWith(b, { 0x53, 0x80, 0xf6, 0x34 })) {
        return "pic";
    }
    if (b.size() >= 2 && b[0] == 'P' && b[1] >= '1' && b[1] <= '6') {
        return b[1] == '1' || b[1] == '4' ? "pbm" : (b[1] == '2' || b[1] == '5' ? "pgm" : "ppm");
    }
    if (b.size() >= 4 && b[0] == 0x0a && b[2] == 1) {
        return "pcx";
    }
    static const std::string tgaFooter = "TRUEVISION-XFILE.";
    if (b.size() >= 18 && std::equal(tgaFooter.begin(), tgaFooter.end(), b.end() - 18)) {
        return "tga";
    }
    return normalizeRasterFormat(extensionHint);
}
//=============================================================================
static bool
readBytes(const std::wstring& filename, std::vector<uint8_t>& bytes, std::string& error)
{
    std::ifstream input(std::filesystem::path(filename), std::ios::binary);
    if (!input) {
        error = "cannot open image file";
        return false;
    }
    bytes.assign(std::istreambuf_iterator<char>(input), std::istreambuf_iterator<char>());
    if (bytes.empty()) {
        error = "empty image file";
        return false;
    }
    return true;
}
//=============================================================================
bool
decodeRasterImage(const std::vector<uint8_t>& bytes, const std::string& hint, RasterImage& image,
    std::string& error)
{
    const std::string format = detectRasterFormat(bytes, hint);
#ifdef NELSON_GRAPHICS_IO_STB_ONLY
    if (format == "pbm" || format == "pgm" || format == "ppm" || format == "pnm") {
        return decodePnm(bytes, image, error);
    }
    if (format == "pcx") {
        return decodePcx(bytes, image, error);
    }
    if (format == "webp" || format == "tiff") {
        error = "image format is unavailable in this portable build";
        return false;
    }
    return decodeStb(bytes, image, error);
#else
    if (format == "png") {
        return decodePng(bytes, image, error);
    }
    if (format == "jpg") {
        return decodeJpeg(bytes, image, error);
    }
    if (format == "gif") {
        return decodeGif(bytes, image, error);
    }
    if (format == "webp") {
        return decodeWebp(bytes, image, error);
    }
    if (format == "pbm" || format == "pgm" || format == "ppm" || format == "pnm") {
        return decodePnm(bytes, image, error);
    }
    if (format == "pcx") {
        return decodePcx(bytes, image, error);
    }
    if (format == "tiff") {
        error = "TIFF decoding requires a file path";
        return false;
    }
    return decodeStb(bytes, image, error);
#endif
}
//=============================================================================
bool
readRasterImage(const std::wstring& filename, RasterImage& image, std::string& error)
{
    std::vector<uint8_t> bytes;
    if (!readBytes(filename, bytes, error)) {
        return false;
    }
    const std::filesystem::path path(filename);
    const std::string hint = path.has_extension() ? path.extension().string() : std::string();
    if (detectRasterFormat(bytes, hint) == "tiff") {
#ifdef NELSON_GRAPHICS_IO_STB_ONLY
        error = "TIFF decoding is unavailable in this portable build";
        return false;
#else
        return decodeTiff(filename, image, error);
#endif
    }
    return decodeRasterImage(bytes, hint, image, error);
}
//=============================================================================
bool
encodeRasterImage(const std::string& requested, const RasterImage& image, int quality,
    std::vector<uint8_t>& bytes, std::string& error)
{
    const std::string format = normalizeRasterFormat(requested);
    const RasterFormatInfo* info = rasterFormat(format);
    if (info == nullptr || !info->writable) {
        error = "unsupported image format: " + requested;
        return false;
    }
    if (!image.valid()) {
        error = "invalid raster image";
        return false;
    }
#ifdef NELSON_GRAPHICS_IO_STB_ONLY
    if (format == "pbm" || format == "pgm" || format == "ppm" || format == "pnm") {
        return encodePnm(format, image, bytes, error);
    }
    if (format == "pcx") {
        return encodePcx(image, bytes, error);
    }
    if (format == "gif" || format == "webp" || format == "tiff") {
        error = "image format is unavailable in this portable build";
        return false;
    }
    return encodeStb(format, image, quality, bytes, error);
#else
    if (format == "png") {
        return encodePng(image, bytes, error);
    }
    if (format == "jpg") {
        return encodeJpeg(image, quality, bytes, error);
    }
    if (format == "webp") {
        return encodeWebp(image, quality, bytes, error);
    }
    if (format == "pbm" || format == "pgm" || format == "ppm" || format == "pnm") {
        return encodePnm(format, image, bytes, error);
    }
    if (format == "pcx") {
        return encodePcx(image, bytes, error);
    }
    if (format == "gif") {
        return encodeGif(image, bytes, error);
    }
    if (format == "tiff") {
        error = "TIFF encoding requires a file path";
        return false;
    }
    return encodeStb(format, image, quality, bytes, error);
#endif
}
//=============================================================================
static bool
writeBytes(const std::filesystem::path& path, const std::vector<uint8_t>& bytes, std::string& error)
{
    std::ofstream output(path, std::ios::binary | std::ios::trunc);
    if (!output) {
        error = "cannot open output file";
        return false;
    }
    output.write(
        reinterpret_cast<const char*>(bytes.data()), static_cast<std::streamsize>(bytes.size()));
    if (!output.good()) {
        error = "cannot write output file";
        return false;
    }
    return true;
}
//=============================================================================
bool
writeRasterImage(const std::wstring& filename, const std::string& requested,
    const RasterImage& image, int quality, std::string& error)
{
    const std::string format = normalizeRasterFormat(requested);
    if (format == "tiff") {
#ifdef NELSON_GRAPHICS_IO_STB_ONLY
        error = "TIFF encoding is unavailable in this portable build";
        return false;
#else
        return encodeTiff(filename, image, error);
#endif
    }
    std::vector<uint8_t> bytes;
    if (!encodeRasterImage(format, image, quality, bytes, error)) {
        return false;
    }
    return writeBytes(std::filesystem::path(filename), bytes, error);
}
//=============================================================================
bool
writeRasterImageAtomic(const std::wstring& filename, const std::string& format,
    const RasterImage& image, int quality, std::string& error)
{
    const std::filesystem::path target(filename);
    const std::filesystem::path temporary = rasterTemporaryPath(target);
    std::error_code ec;
    std::filesystem::remove(temporary, ec);
    if (!writeRasterImage(temporary.wstring(), format, image, quality, error)) {
        std::filesystem::remove(temporary, ec);
        return false;
    }
    return replaceFileAtomically(temporary, target, error);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
