//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RenderWeb.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    void
    hashBytes(uint64_t& hash, const void* data, size_t size)
    {
        const auto* bytes = static_cast<const unsigned char*>(data);
        for (size_t i = 0; i < size; ++i) {
            hash ^= bytes[i];
            hash *= 1099511628211ULL;
        }
    }
    //=============================================================================
    template <typename T>
    void
    hashValue(uint64_t& hash, const T& value)
    {
        hashBytes(hash, &value, sizeof(T));
    }
    //=============================================================================
    void
    hashString(uint64_t& hash, const std::string& value)
    {
        hashBytes(hash, value.data(), value.size());
        const char terminator = 0;
        hashBytes(hash, &terminator, sizeof(terminator));
    }
    //=============================================================================
    // Image payloads are base64 PNG data URIs that can weigh megabytes; hashing
    // them byte per byte on every animation frame dominates the signature cost.
    // Length + head + tail is enough: PNG deflate makes any pixel change ripple
    // through the compressed stream, so identical boundaries with an identical
    // length virtually always mean an identical image.
    void
    hashLargeStringSampled(uint64_t& hash, const std::string& value)
    {
        constexpr size_t sampleSize = 64;
        hashValue(hash, value.size());
        if (value.size() <= 2 * sampleSize) {
            hashBytes(hash, value.data(), value.size());
            return;
        }
        hashBytes(hash, value.data(), sampleSize);
        hashBytes(hash, value.data() + value.size() - sampleSize, sampleSize);
    }
    //=============================================================================
    void
    hashControls(uint64_t& hash, const std::vector<WebUIControl>& controls)
    {
        hashValue(hash, controls.size());
        for (const WebUIControl& control : controls) {
            hashValue(hash, control.handle);
            hashString(hash, control.style);
            hashString(hash, control.text);
            for (const std::string& item : control.items) {
                hashString(hash, item);
            }
            hashString(hash, control.tooltip);
            hashString(hash, control.background);
            hashString(hash, control.foreground);
            hashString(hash, control.units);
            hashValue(hash, control.x);
            hashValue(hash, control.y);
            hashValue(hash, control.w);
            hashValue(hash, control.h);
            hashValue(hash, control.value);
            hashValue(hash, control.minimum);
            hashValue(hash, control.maximum);
            hashValue(hash, control.fontSize);
            hashValue(hash, control.enabled);
            hashString(hash, control.horizontalAlignment);
            hashString(hash, control.verticalAlignment);
            hashString(hash, control.extraJson);
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
static void
setIdentity(double matrix[16])
{
    for (int i = 0; i < 16; ++i) {
        matrix[i] = 0;
    }
    matrix[0] = matrix[5] = matrix[10] = matrix[15] = 1;
}
//=============================================================================
RenderWeb::RenderWeb(double x1, double y1, double width, double height,
    bool softwareFallbackEnabled, bool encodeImagePngPayloads)
    : m_x1(x1)
    , m_y1(y1)
    , m_width(width)
    , m_height(height)
    , m_softwareFallbackEnabled(softwareFallbackEnabled)
    , m_encodeImagePngPayloads(encodeImagePngPayloads)
{
    setIdentity(model);
    setIdentity(proj);
    viewp[0] = static_cast<int>(x1);
    viewp[1] = static_cast<int>(y1);
    viewp[2] = static_cast<int>(width);
    viewp[3] = static_cast<int>(height);
}
//=============================================================================
uint64_t
RenderWeb::displaySignature(bool includeImagePayload) const
{
    uint64_t hash = 1469598103934665603ULL;
    hashValue(hash, m_prims.size());
    for (const WebDrawPrimitive& primitive : m_prims) {
        hashString(hash, primitive.kind);
        hashValue(hash, primitive.x);
        hashValue(hash, primitive.y);
        hashValue(hash, primitive.w);
        hashValue(hash, primitive.h);
        hashValue(hash, primitive.x1);
        hashValue(hash, primitive.y1);
        hashValue(hash, primitive.x2);
        hashValue(hash, primitive.y2);
        for (const auto& point : primitive.points) {
            hashValue(hash, point.first);
            hashValue(hash, point.second);
        }
        hashString(hash, primitive.color);
        hashString(hash, primitive.stroke);
        for (double dashValue : primitive.dash) {
            hashValue(hash, dashValue);
        }
        hashValue(hash, primitive.width);
        hashString(hash, primitive.text);
        if (includeImagePayload) {
            hashLargeStringSampled(hash, primitive.image);
        }
        hashValue(hash, primitive.imageWidth);
        hashValue(hash, primitive.imageHeight);
        if (includeImagePayload && !primitive.imageRgba.empty()) {
            hashBytes(hash, primitive.imageRgba.data(), primitive.imageRgba.size());
        }
        // The token is already a content signature for raw images; keeping it in
        // compact signatures tells animated frames apart without hashing the RGBA
        // payload a second time.
        hashString(hash, primitive.imageToken);
        hashString(hash, primitive.align);
        hashValue(hash, primitive.rotation);
        hashValue(hash, primitive.size);
        hashString(hash, primitive.fontFamily);
        hashValue(hash, primitive.fontBold);
        hashValue(hash, primitive.fontItalic);
        hashValue(hash, primitive.webglFallback);
        // "points" marker batch carries its geometry here, not in the fields above,
        // so the animation signature must fold it in or a moving scatter would hash
        // identically frame to frame and be skipped as unchanged.
        hashValue(hash, primitive.pointShape);
        hashValue(hash, primitive.pointEdgeWidth);
        for (float value : primitive.pointCenters) {
            hashValue(hash, value);
        }
        for (float value : primitive.pointRadii) {
            hashValue(hash, value);
        }
        for (uint8_t value : primitive.pointColors) {
            hashValue(hash, value);
        }
        for (uint8_t value : primitive.pointEdgeColors) {
            hashValue(hash, value);
        }
        for (float value : primitive.rectXYWH) {
            hashValue(hash, value);
        }
    }
    hashControls(hash, m_controls);
    return hash;
}
//=============================================================================
uint64_t
RenderWeb::layoutSignature() const
{
    uint64_t hash = 1469598103934665603ULL;
    hashControls(hash, m_controls);
    return hash;
}
//=============================================================================
void
RenderWeb::currentWebGlTransform(WebGlMesh& mesh) const
{
    for (int i = 0; i < 16; ++i) {
        mesh.model[i] = model[i];
        mesh.projection[i] = proj[i];
    }
    for (int i = 0; i < 4; ++i) {
        mesh.viewport[i] = viewp[i];
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
