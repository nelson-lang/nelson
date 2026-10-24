//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RasterAtomicFile.hpp"
#include "RasterCodecInternal.hpp"
#include <algorithm>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <gif_lib.h>
#include <iterator>
#include <unordered_map>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    constexpr uint8_t TRANSPARENT_INDEX = 216;
    struct GifInput
    {
        const std::vector<uint8_t>& bytes;
        size_t offset { 0 };
    };
    //=============================================================================
    struct GifAnimation
    {
        int width { 0 };
        int height { 0 };
        int loopCount { 0 };
        std::vector<std::vector<uint8_t>> frames;
        std::vector<int> delaysMs;
        std::map<std::string, std::string> metadata;
    };
    //=============================================================================
    int
    readGifBytes(GifFileType* file, GifByteType* destination, int count)
    {
        auto* input = static_cast<GifInput*>(file->UserData);
        const size_t available = input->bytes.size() - std::min(input->offset, input->bytes.size());
        const size_t copied = std::min(available, static_cast<size_t>(count));
        std::copy_n(input->bytes.data() + input->offset, copied, destination);
        input->offset += copied;
        return static_cast<int>(copied);
    }
    //=============================================================================
    int
    writeGifBytes(GifFileType* file, const GifByteType* source, int count)
    {
        auto* output = static_cast<std::vector<uint8_t>*>(file->UserData);
        output->insert(output->end(), source, source + count);
        return count;
    }
    //=============================================================================
    std::string
    gifError(int code, const char* fallback)
    {
        const char* message = GifErrorString(code);
        return message ? message : fallback;
    }
    //=============================================================================
    std::vector<uint8_t>
    orderedIndices(const SavedImage& frame)
    {
        const int width = frame.ImageDesc.Width;
        const int height = frame.ImageDesc.Height;
        std::vector<uint8_t> indices(static_cast<size_t>(width) * height);
        if (!frame.ImageDesc.Interlace) {
            std::copy_n(frame.RasterBits, indices.size(), indices.data());
            return indices;
        }
        constexpr int offsets[] = { 0, 4, 2, 1 };
        constexpr int jumps[] = { 8, 8, 4, 2 };
        int sourceRow = 0;
        for (int pass = 0; pass < 4; ++pass) {
            for (int row = offsets[pass]; row < height; row += jumps[pass]) {
                std::copy_n(frame.RasterBits + static_cast<size_t>(sourceRow++) * width, width,
                    indices.data() + static_cast<size_t>(row) * width);
            }
        }
        return indices;
    }
    //=============================================================================
    int
    gifLoopCount(const GifFileType* file)
    {
        auto inspect = [](const ExtensionBlock* blocks, int count) {
            for (int i = 0; i + 1 < count; ++i) {
                const ExtensionBlock& app = blocks[i];
                const ExtensionBlock& data = blocks[i + 1];
                if (app.Function == APPLICATION_EXT_FUNC_CODE && app.ByteCount == 11
                    && std::memcmp(app.Bytes, "NETSCAPE2.0", 11) == 0 && data.ByteCount >= 3
                    && data.Bytes[0] == 1) {
                    return static_cast<int>(data.Bytes[1] | (data.Bytes[2] << 8));
                }
            }
            return -1;
        };
        for (int i = 0; i < file->ImageCount; ++i) {
            const SavedImage& frame = file->SavedImages[i];
            const int loop = inspect(frame.ExtensionBlocks, frame.ExtensionBlockCount);
            if (loop >= 0) {
                return loop;
            }
        }
        const int loop = inspect(file->ExtensionBlocks, file->ExtensionBlockCount);
        return loop >= 0 ? loop : 0;
    }
    //=============================================================================
    void
    readGifComments(const ExtensionBlock* blocks, int count, GifAnimation& animation)
    {
        bool continuing = false;
        for (int i = 0; i < count; ++i) {
            if (blocks[i].Function == COMMENT_EXT_FUNC_CODE) {
                continuing = true;
            } else if (blocks[i].Function != CONTINUE_EXT_FUNC_CODE || !continuing) {
                continuing = false;
            }
            if (continuing && blocks[i].ByteCount > 0) {
                animation.metadata["Comment"].append(
                    reinterpret_cast<const char*>(blocks[i].Bytes), blocks[i].ByteCount);
            }
        }
    }
    //=============================================================================
    bool
    decodeGifAnimation(
        const std::vector<uint8_t>& bytes, GifAnimation& animation, std::string& error)
    {
        GifInput input { bytes };
        int code = 0;
        GifFileType* file = DGifOpen(&input, readGifBytes, &code);
        if (!file) {
            error = gifError(code, "cannot open GIF image");
            return false;
        }
        if (DGifSlurp(file) == GIF_ERROR || file->ImageCount <= 0) {
            error = gifError(file->Error, "cannot decode GIF image");
            DGifCloseFile(file, &code);
            return false;
        }
        animation.width = file->SWidth;
        animation.height = file->SHeight;
        animation.loopCount = gifLoopCount(file);
        readGifComments(file->ExtensionBlocks, file->ExtensionBlockCount, animation);
        for (int index = 0; index < file->ImageCount; ++index) {
            readGifComments(file->SavedImages[index].ExtensionBlocks,
                file->SavedImages[index].ExtensionBlockCount, animation);
        }
        std::vector<uint8_t> canvas(
            static_cast<size_t>(animation.width) * animation.height * 4, uint8_t { 0 });
        for (int index = 0; index < file->ImageCount; ++index) {
            const SavedImage& frame = file->SavedImages[index];
            const ColorMapObject* colors
                = frame.ImageDesc.ColorMap ? frame.ImageDesc.ColorMap : file->SColorMap;
            if (!colors) {
                error = "GIF frame has no color map";
                DGifCloseFile(file, &code);
                return false;
            }
            GraphicsControlBlock control { DISPOSAL_UNSPECIFIED, false, 0, NO_TRANSPARENT_COLOR };
            DGifSavedExtensionToGCB(file, index, &control);
            const std::vector<uint8_t> before = canvas;
            const std::vector<uint8_t> indices = orderedIndices(frame);
            for (int y = 0; y < frame.ImageDesc.Height; ++y) {
                for (int x = 0; x < frame.ImageDesc.Width; ++x) {
                    const int colorIndex
                        = indices[static_cast<size_t>(y) * frame.ImageDesc.Width + x];
                    const int targetX = frame.ImageDesc.Left + x;
                    const int targetY = frame.ImageDesc.Top + y;
                    if (colorIndex == control.TransparentColor || colorIndex >= colors->ColorCount
                        || targetX < 0 || targetY < 0 || targetX >= animation.width
                        || targetY >= animation.height) {
                        continue;
                    }
                    const GifColorType& color = colors->Colors[colorIndex];
                    uint8_t* pixel = canvas.data()
                        + (static_cast<size_t>(targetY) * animation.width + targetX) * 4;
                    pixel[0] = color.Red;
                    pixel[1] = color.Green;
                    pixel[2] = color.Blue;
                    pixel[3] = 255;
                }
            }
            animation.frames.push_back(canvas);
            animation.delaysMs.push_back(control.DelayTime * 10);
            if (control.DisposalMode == DISPOSE_BACKGROUND) {
                for (int y = 0; y < frame.ImageDesc.Height; ++y) {
                    const int targetY = frame.ImageDesc.Top + y;
                    if (targetY < 0 || targetY >= animation.height) {
                        continue;
                    }
                    const int left = std::clamp(frame.ImageDesc.Left, 0, animation.width);
                    const int right = std::clamp(
                        frame.ImageDesc.Left + frame.ImageDesc.Width, 0, animation.width);
                    std::fill(canvas.begin()
                            + (static_cast<size_t>(targetY) * animation.width + left) * 4,
                        canvas.begin()
                            + (static_cast<size_t>(targetY) * animation.width + right) * 4,
                        0);
                }
            } else if (control.DisposalMode == DISPOSE_PREVIOUS) {
                canvas = before;
            }
        }
        DGifCloseFile(file, &code);
        return true;
    }
    //=============================================================================
    ColorMapObject*
    makeGifPalette()
    {
        ColorMapObject* palette = GifMakeMapObject(256, nullptr);
        if (!palette) {
            return nullptr;
        }
        for (int red = 0; red < 6; ++red) {
            for (int green = 0; green < 6; ++green) {
                for (int blue = 0; blue < 6; ++blue) {
                    GifColorType& color = palette->Colors[red * 36 + green * 6 + blue];
                    color.Red = static_cast<GifByteType>(red * 51);
                    color.Green = static_cast<GifByteType>(green * 51);
                    color.Blue = static_cast<GifByteType>(blue * 51);
                }
            }
        }
        return palette;
    }
    //=============================================================================
    std::vector<uint8_t>
    quantizeGifFrame(const std::vector<uint8_t>& rgba)
    {
        std::vector<uint8_t> indices(rgba.size() / 4);
        for (size_t i = 0; i < indices.size(); ++i) {
            if (rgba[i * 4 + 3] < 128) {
                indices[i] = TRANSPARENT_INDEX;
                continue;
            }
            const int red = (rgba[i * 4] * 5 + 127) / 255;
            const int green = (rgba[i * 4 + 1] * 5 + 127) / 255;
            const int blue = (rgba[i * 4 + 2] * 5 + 127) / 255;
            indices[i] = static_cast<uint8_t>(red * 36 + green * 6 + blue);
        }
        return indices;
    }
    //=============================================================================
    RasterImage
    indexedGifFrame(const GifAnimation& animation)
    {
        RasterImage image;
        image.width = animation.width;
        image.height = animation.height;
        image.format = RasterPixelFormat::Indexed8;
        const std::vector<uint8_t>& rgba = animation.frames.front();
        image.pixels.resize(rgba.size() / 4);
        std::unordered_map<uint32_t, uint8_t> colors;
        for (size_t i = 0; i < image.pixels.size(); ++i) {
            const uint32_t key = static_cast<uint32_t>(rgba[i * 4]) << 24
                | static_cast<uint32_t>(rgba[i * 4 + 1]) << 16
                | static_cast<uint32_t>(rgba[i * 4 + 2]) << 8 | rgba[i * 4 + 3];
            auto found = colors.find(key);
            if (found == colors.end() && colors.size() < 256) {
                const uint8_t index = static_cast<uint8_t>(colors.size());
                colors.emplace(key, index);
                image.palette.insert(image.palette.end(),
                    rgba.begin() + static_cast<ptrdiff_t>(i * 4),
                    rgba.begin() + static_cast<ptrdiff_t>(i * 4 + 4));
                found = colors.find(key);
            }
            if (found == colors.end()) {
                image.palette.clear();
                image.pixels = quantizeGifFrame(rgba);
                ColorMapObject* palette = makeGifPalette();
                image.palette.resize(256 * 4, 255);
                for (int color = 0; color < 256; ++color) {
                    image.palette[color * 4] = palette->Colors[color].Red;
                    image.palette[color * 4 + 1] = palette->Colors[color].Green;
                    image.palette[color * 4 + 2] = palette->Colors[color].Blue;
                }
                image.palette[TRANSPARENT_INDEX * 4 + 3] = 0;
                GifFreeMapObject(palette);
                return image;
            }
            image.pixels[i] = found->second;
        }
        return image;
    }
    //=============================================================================
    bool
    encodeGifAnimation(const GifAnimation& animation,
        const std::map<std::string, std::string>& metadata, std::vector<uint8_t>& bytes,
        std::string& error)
    {
        int code = 0;
        GifFileType* file = EGifOpen(&bytes, writeGifBytes, &code);
        ColorMapObject* palette = makeGifPalette();
        if (!file || !palette) {
            if (file) {
                EGifCloseFile(file, &code);
            }
            GifFreeMapObject(palette);
            error = gifError(code, "cannot create GIF image");
            return false;
        }
        EGifSetGifVersion(file, true);
        bool ok = EGifPutScreenDesc(
                      file, animation.width, animation.height, 8, TRANSPARENT_INDEX, palette)
            != GIF_ERROR;
        const uint8_t application[] = "NETSCAPE2.0";
        const uint8_t loop[] = { 1, static_cast<uint8_t>(animation.loopCount & 0xff),
            static_cast<uint8_t>((animation.loopCount >> 8) & 0xff) };
        ok = ok && EGifPutExtensionLeader(file, APPLICATION_EXT_FUNC_CODE) != GIF_ERROR
            && EGifPutExtensionBlock(file, 11, application) != GIF_ERROR
            && EGifPutExtensionBlock(file, 3, loop) != GIF_ERROR
            && EGifPutExtensionTrailer(file) != GIF_ERROR;
        auto comment = metadata.find("Comment");
        if (ok && comment != metadata.end() && !comment->second.empty()) {
            ok = EGifPutComment(file, comment->second.c_str()) != GIF_ERROR;
        }
        for (size_t frame = 0; ok && frame < animation.frames.size(); ++frame) {
            GraphicsControlBlock control { DISPOSE_DO_NOT, false,
                std::clamp(animation.delaysMs[frame] / 10, 0, 65535), TRANSPARENT_INDEX };
            GifByteType extension[4];
            EGifGCBToExtension(&control, extension);
            ok = EGifPutExtension(file, GRAPHICS_EXT_FUNC_CODE, 4, extension) != GIF_ERROR
                && EGifPutImageDesc(file, 0, 0, animation.width, animation.height, false, nullptr)
                    != GIF_ERROR;
            std::vector<uint8_t> indices = quantizeGifFrame(animation.frames[frame]);
            for (int y = 0; ok && y < animation.height; ++y) {
                ok = EGifPutLine(file, indices.data() + static_cast<size_t>(y) * animation.width,
                         animation.width)
                    != GIF_ERROR;
            }
        }
        GifFreeMapObject(palette);
        if (EGifCloseFile(file, &code) == GIF_ERROR) {
            ok = false;
        }
        if (!ok) {
            bytes.clear();
            error = gifError(code, "GIF encoder failed");
        }
        return ok;
    }
    //=============================================================================
    std::vector<uint8_t>
    padGifFrame(const std::vector<uint8_t>& source, int sourceWidth, int sourceHeight,
        int targetWidth, int targetHeight)
    {
        std::vector<uint8_t> result(static_cast<size_t>(targetWidth) * targetHeight * 4, 0);
        for (int y = 0; y < sourceHeight; ++y) {
            std::copy_n(source.data() + static_cast<size_t>(y) * sourceWidth * 4,
                static_cast<size_t>(sourceWidth) * 4,
                result.data() + static_cast<size_t>(y) * targetWidth * 4);
        }
        return result;
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
decodeGif(const std::vector<uint8_t>& bytes, RasterImage& image, std::string& error)
{
    GifAnimation animation;
    if (!decodeGifAnimation(bytes, animation, error)) {
        return false;
    }
    image = indexedGifFrame(animation);
    image.metadata = std::move(animation.metadata);
    return image.valid();
}
//=============================================================================
bool
encodeGif(const RasterImage& image, std::vector<uint8_t>& bytes, std::string& error)
{
    if (image.format == RasterPixelFormat::Gray16) {
        error = "16-bit images cannot be written as GIF";
        return false;
    }
    GifAnimation animation;
    animation.width = image.width;
    animation.height = image.height;
    animation.frames.push_back(expandedRasterPixels(image, 4));
    animation.delaysMs.push_back(0);
    return encodeGifAnimation(animation, image.metadata, bytes, error);
}
//=============================================================================
bool
writeRasterGif(const std::wstring& filename, const RasterImage& image, bool append, int delayMs,
    int loopCount, std::string& error)
{
    if (image.format == RasterPixelFormat::Gray16) {
        error = "16-bit images cannot be written as GIF";
        return false;
    }
    GifAnimation animation;
    const std::filesystem::path target(filename);
    if (append && std::filesystem::is_regular_file(target)) {
        std::ifstream input(target, std::ios::binary);
        std::vector<uint8_t> existing { std::istreambuf_iterator<char>(input),
            std::istreambuf_iterator<char>() };
        if (!decodeGifAnimation(existing, animation, error)) {
            return false;
        }
    }
    const int oldWidth = animation.width;
    const int oldHeight = animation.height;
    animation.width = std::max(animation.width, image.width);
    animation.height = std::max(animation.height, image.height);
    if (oldWidth > 0 && (oldWidth != animation.width || oldHeight != animation.height)) {
        for (std::vector<uint8_t>& frame : animation.frames) {
            frame = padGifFrame(frame, oldWidth, oldHeight, animation.width, animation.height);
        }
    }
    if (animation.frames.empty()) {
        animation.loopCount = std::clamp(loopCount, 0, 65535);
    }
    animation.frames.push_back(padGifFrame(expandedRasterPixels(image, 4), image.width,
        image.height, animation.width, animation.height));
    animation.delaysMs.push_back(std::clamp(delayMs, 0, 655350));
    for (const auto& item : image.metadata) {
        animation.metadata[item.first] = item.second;
    }
    std::vector<uint8_t> bytes;
    if (!encodeGifAnimation(animation, animation.metadata, bytes, error)) {
        return false;
    }
    const std::filesystem::path temporary = rasterTemporaryPath(target);
    std::ofstream output(temporary, std::ios::binary | std::ios::trunc);
    output.write(
        reinterpret_cast<const char*>(bytes.data()), static_cast<std::streamsize>(bytes.size()));
    const bool written = output.good();
    output.close();
    if (!written) {
        std::error_code cleanup;
        std::filesystem::remove(temporary, cleanup);
        error = "cannot write GIF file";
        return false;
    }
    return replaceFileAtomically(temporary, target, error);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
