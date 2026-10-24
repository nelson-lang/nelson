//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RasterCodecInternal.hpp"
#include <algorithm>
#include <csetjmp>
#include <cstdlib>
#include <cstring>
#include <jpeglib.h>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    struct JpegError
    {
        jpeg_error_mgr manager;
        std::jmp_buf jump;
        char message[JMSG_LENGTH_MAX] {};
    };
    //=============================================================================
    void
    failJpeg(j_common_ptr context)
    {
        auto* state = reinterpret_cast<JpegError*>(context->err);
        (*context->err->format_message)(context, state->message);
        std::longjmp(state->jump, 1);
    }
    //=============================================================================
    void
    readJpegComments(jpeg_decompress_struct& decoder, RasterImage& image)
    {
        for (jpeg_saved_marker_ptr marker = decoder.marker_list; marker; marker = marker->next) {
            if (marker->marker != JPEG_COM || marker->data_length == 0) {
                continue;
            }
            std::string value(reinterpret_cast<const char*>(marker->data), marker->data_length);
            constexpr const char authorPrefix[] = "Author: ";
            if (value.rfind(authorPrefix, 0) == 0) {
                image.metadata["Author"] = value.substr(sizeof(authorPrefix) - 1);
            } else if (image.metadata.find("Comment") == image.metadata.end()) {
                image.metadata["Comment"] = std::move(value);
            }
        }
    }
    //=============================================================================
    uint8_t
    cmykChannel(uint8_t color, uint8_t black, bool inverted)
    {
        return inverted ? static_cast<uint8_t>((color * black + 127) / 255)
                        : static_cast<uint8_t>(((255 - color) * (255 - black) + 127) / 255);
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
decodeJpeg(const std::vector<uint8_t>& bytes, RasterImage& image, std::string& error)
{
    jpeg_decompress_struct decoder {};
    JpegError state {};
    decoder.err = jpeg_std_error(&state.manager);
    state.manager.error_exit = failJpeg;
    if (setjmp(state.jump)) {
        jpeg_destroy_decompress(&decoder);
        error = state.message[0] ? state.message : "JPEG decoder failed";
        return false;
    }
    jpeg_create_decompress(&decoder);
    jpeg_save_markers(&decoder, JPEG_COM, 0xFFFF);
    jpeg_mem_src(&decoder, bytes.data(), static_cast<unsigned long>(bytes.size()));
    jpeg_read_header(&decoder, TRUE);
    const bool cmyk = decoder.jpeg_color_space == JCS_CMYK || decoder.jpeg_color_space == JCS_YCCK;
    decoder.out_color_space
        = cmyk ? JCS_CMYK : (decoder.jpeg_color_space == JCS_GRAYSCALE ? JCS_GRAYSCALE : JCS_RGB);
    jpeg_start_decompress(&decoder);
    image = RasterImage {};
    image.width = static_cast<int>(decoder.output_width);
    image.height = static_cast<int>(decoder.output_height);
    image.format
        = decoder.output_components == 1 ? RasterPixelFormat::Gray8 : RasterPixelFormat::RGB8;
    const size_t inputStride
        = static_cast<size_t>(decoder.output_width) * decoder.output_components;
    std::vector<uint8_t> row(inputStride);
    image.pixels.resize(static_cast<size_t>(image.width) * image.height * image.channels());
    while (decoder.output_scanline < decoder.output_height) {
        JSAMPROW scanline = row.data();
        jpeg_read_scanlines(&decoder, &scanline, 1);
        const size_t y = decoder.output_scanline - 1;
        uint8_t* destination
            = image.pixels.data() + y * static_cast<size_t>(image.width) * image.channels();
        if (!cmyk) {
            std::copy_n(
                row.data(), static_cast<size_t>(image.width) * image.channels(), destination);
            continue;
        }
        for (int x = 0; x < image.width; ++x) {
            const uint8_t* source = row.data() + static_cast<size_t>(x) * 4;
            destination[x * 3] = cmykChannel(source[0], source[3], decoder.saw_Adobe_marker);
            destination[x * 3 + 1] = cmykChannel(source[1], source[3], decoder.saw_Adobe_marker);
            destination[x * 3 + 2] = cmykChannel(source[2], source[3], decoder.saw_Adobe_marker);
        }
    }
    readJpegComments(decoder, image);
    jpeg_finish_decompress(&decoder);
    jpeg_destroy_decompress(&decoder);
    return image.valid();
}
//=============================================================================
bool
encodeJpeg(const RasterImage& image, int quality, std::vector<uint8_t>& bytes, std::string& error)
{
    if (image.format == RasterPixelFormat::Gray16) {
        error = "16-bit images cannot be written as JPEG";
        return false;
    }
    jpeg_compress_struct encoder {};
    JpegError state {};
    encoder.err = jpeg_std_error(&state.manager);
    state.manager.error_exit = failJpeg;
    unsigned char* encoded = nullptr;
    unsigned long encodedSize = 0;
    if (setjmp(state.jump)) {
        jpeg_destroy_compress(&encoder);
        std::free(encoded);
        error = state.message[0] ? state.message : "JPEG encoder failed";
        return false;
    }
    const bool gray = image.format == RasterPixelFormat::Gray8;
    const int channels = gray ? 1 : 3;
    std::vector<uint8_t> pixels = expandedRasterPixels(image, channels);
    jpeg_create_compress(&encoder);
    jpeg_mem_dest(&encoder, &encoded, &encodedSize);
    encoder.image_width = image.width;
    encoder.image_height = image.height;
    encoder.input_components = channels;
    encoder.in_color_space = gray ? JCS_GRAYSCALE : JCS_RGB;
    jpeg_set_defaults(&encoder);
    jpeg_set_quality(&encoder, std::clamp(quality, 0, 100), TRUE);
    jpeg_start_compress(&encoder, TRUE);
    for (const char* key : { "Comment", "Author" }) {
        auto found = image.metadata.find(key);
        if (found == image.metadata.end()) {
            continue;
        }
        const std::string value
            = std::strcmp(key, "Author") == 0 ? "Author: " + found->second : found->second;
        jpeg_write_marker(&encoder, JPEG_COM, reinterpret_cast<const JOCTET*>(value.data()),
            static_cast<unsigned int>(value.size()));
    }
    while (encoder.next_scanline < encoder.image_height) {
        JSAMPROW row
            = pixels.data() + static_cast<size_t>(encoder.next_scanline) * image.width * channels;
        jpeg_write_scanlines(&encoder, &row, 1);
    }
    jpeg_finish_compress(&encoder);
    bytes.assign(encoded, encoded + encodedSize);
    jpeg_destroy_compress(&encoder);
    std::free(encoded);
    return !bytes.empty();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
