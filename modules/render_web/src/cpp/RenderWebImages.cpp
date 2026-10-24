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
#include "RenderWebInternal.hpp"
#include "RenderLighting.hpp"
#include "GOPropertyValues.hpp"
#include <algorithm>
#include <cmath>
#include <cstring>
#include <limits>
#include <map>
#include <unordered_map>
#include <unordered_set>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    struct CachedWireframeGeometry
    {
        std::vector<float> positions;
        std::vector<float> vertexColors;
        std::vector<uint32_t> triangleIndices;
        std::vector<uint32_t> lineIndices;
        bool drawFaces = false;
    };
    //=============================================================================
    // Ordered by token: geometry tokens are monotonically increasing, so the
    // smallest key is always the oldest entry, which makes eviction trivial.
    using WireframeGeometryCache = std::map<uint64_t, CachedWireframeGeometry>;
    constexpr size_t MAX_WIREFRAME_GEOMETRY_CACHE_SIZE = 128;
    constexpr size_t MAX_WIREFRAME_GEOMETRY_CANDIDATES = 256;
    struct WebGlVertexKey
    {
        float x;
        float y;
        float z;
        float r;
        float g;
        float b;
        float a;

        bool
        operator==(const WebGlVertexKey& other) const
        {
            return x == other.x && y == other.y && z == other.z && r == other.r && g == other.g
                && b == other.b && a == other.a;
        }
    };
    //=============================================================================
    uint32_t
    floatBits(float value)
    {
        uint32_t bits = 0;
        std::memcpy(&bits, &value, sizeof(bits));
        return bits;
    }
    //=============================================================================
    struct WebGlVertexKeyHash
    {
        size_t
        operator()(const WebGlVertexKey& key) const
        {
            uint64_t hash = 1469598103934665603ULL;
            auto mix = [&hash](float value) {
                hash ^= static_cast<uint64_t>(floatBits(value));
                hash *= 1099511628211ULL;
            };
            mix(key.x);
            mix(key.y);
            mix(key.z);
            mix(key.r);
            mix(key.g);
            mix(key.b);
            mix(key.a);
            return static_cast<size_t>(hash);
        }
    };
    //=============================================================================
    using WebGlVertexMap = std::unordered_map<WebGlVertexKey, uint32_t, WebGlVertexKeyHash>;
    WireframeGeometryCache&
    wireframeGeometryCache()
    {
        static thread_local auto* cache = new WireframeGeometryCache;
        return *cache;
    }
    //=============================================================================
    std::unordered_set<uint64_t>&
    wireframeGeometryCandidates()
    {
        static thread_local auto* candidates = new std::unordered_set<uint64_t>;
        return *candidates;
    }
    //=============================================================================
    bool
    shouldCacheWireframeGeometry(uint64_t geometryToken)
    {
        auto& candidates = wireframeGeometryCandidates();
        auto [candidate, inserted] = candidates.insert(geometryToken);
        if (inserted) {
            if (candidates.size() > MAX_WIREFRAME_GEOMETRY_CANDIDATES) {
                candidates.clear();
                candidates.insert(geometryToken);
            }
            return false;
        }
        candidates.erase(candidate);
        return true;
    }
    //=============================================================================
    // Evict the oldest entry on insert: an animation that regenerates its geometry
    // every frame would otherwise fill the cache with dead tokens.
    void
    storeWireframeGeometry(uint64_t geometryToken, CachedWireframeGeometry&& geometry)
    {
        WireframeGeometryCache& cache = wireframeGeometryCache();
        while (cache.size() >= MAX_WIREFRAME_GEOMETRY_CACHE_SIZE) {
            cache.erase(cache.begin());
        }
        cache.emplace(geometryToken, std::move(geometry));
    }
    //=============================================================================
    void
    updateMeshBufferLengths(WebGlMesh& mesh)
    {
        mesh.positionsLength = mesh.positions.size();
        mesh.vertexColorsLength = mesh.vertexColors.size();
        mesh.triangleIndicesLength = mesh.triangleIndices.size();
        mesh.lineIndicesLength = mesh.lineIndices.size();
    }
    //=============================================================================
    void
    updateMeshBufferLengths(WebGlMesh& mesh, const CachedWireframeGeometry& cached)
    {
        mesh.positionsLength = cached.positions.size();
        mesh.vertexColorsLength = cached.vertexColors.size();
        mesh.triangleIndicesLength = cached.triangleIndices.size();
        mesh.lineIndicesLength = cached.lineIndices.size();
    }
    //=============================================================================
    uint32_t
    appendWebGlVertex(WebGlMesh& mesh, WebGlVertexMap& vertices, bool storeColor, float x, float y,
        float z, float r, float g, float b, float a)
    {
        WebGlVertexKey key { x, y, z, r, g, b, a };
        const auto existing = vertices.find(key);
        if (existing != vertices.end()) {
            return existing->second;
        }
        const uint32_t index = static_cast<uint32_t>(mesh.positions.size() / 3);
        mesh.positions.push_back(x);
        mesh.positions.push_back(y);
        mesh.positions.push_back(z);
        if (storeColor) {
            mesh.vertexColors.push_back(r);
            mesh.vertexColors.push_back(g);
            mesh.vertexColors.push_back(b);
            mesh.vertexColors.push_back(a);
        }
        vertices.emplace(key, index);
        return index;
    }
    //=============================================================================
    bool
    sameXY(const point& a, const point& b)
    {
        constexpr double eps = 1e-12;
        return std::abs(a.x - b.x) <= eps && std::abs(a.y - b.y) <= eps;
    }
    //=============================================================================
    bool
    finitePoint(const point& value)
    {
        return std::isfinite(value.x) && std::isfinite(value.y) && std::isfinite(value.z);
    }
    //=============================================================================
    bool
    flatXYRun(const std::vector<const point*>& run)
    {
        if (run.empty()) {
            return true;
        }
        const double z0 = run.front()->z;
        for (const point* vertex : run) {
            if (std::abs(vertex->z - z0) > 1e-12) {
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    bool
    concaveXYRun(std::vector<const point*> run)
    {
        if (run.size() > 1 && sameXY(*run.front(), *run.back())) {
            run.pop_back();
        }
        if (run.size() < 4 || !flatXYRun(run)) {
            return false;
        }
        bool sawPositive = false;
        bool sawNegative = false;
        constexpr double eps = 1e-12;
        for (size_t i = 0; i < run.size(); ++i) {
            const point& a = *run[i];
            const point& b = *run[(i + 1) % run.size()];
            const point& c = *run[(i + 2) % run.size()];
            const double abx = b.x - a.x;
            const double aby = b.y - a.y;
            const double bcx = c.x - b.x;
            const double bcy = c.y - b.y;
            const double cross = abx * bcy - aby * bcx;
            if (cross > eps) {
                sawPositive = true;
            } else if (cross < -eps) {
                sawNegative = true;
            }
            if (sawPositive && sawNegative) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    bool
    hasConcaveFilledXYRun(const FaceList& faces)
    {
        for (const Face& face : faces) {
            if (face.FaceColorMode == ColorMode::None) {
                continue;
            }
            std::vector<const point*> run;
            for (const point& vertex : face.vertices) {
                if (!finitePoint(vertex)) {
                    if (concaveXYRun(run)) {
                        return true;
                    }
                    run.clear();
                    continue;
                }
                run.push_back(&vertex);
            }
            if (concaveXYRun(run)) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    std::vector<unsigned char>
    cropAndOrientRGBA(const RGBAImageView& src, int sx, int sy, int sw, int sh, bool mirrorX,
        bool mirrorY, int& outW, int& outH)
    {
        const int srcStride = src.bytesPerLine > 0 ? src.bytesPerLine : src.width * 4;
        const int x0 = std::max(0, sx);
        const int y0 = std::max(0, sy);
        const int x1 = std::min(src.width, sx + sw);
        const int y1 = std::min(src.height, sy + sh);
        outW = std::max(0, x1 - x0);
        outH = std::max(0, y1 - y0);
        std::vector<unsigned char> out(
            static_cast<size_t>(std::max(1, outW)) * static_cast<size_t>(std::max(1, outH)) * 4, 0);
        for (int r = 0; r < outH; ++r) {
            const int srcRow = mirrorY ? (outH - 1 - r) : r;
            const unsigned char* srcLine = src.pixels + static_cast<size_t>(y0 + srcRow) * srcStride
                + static_cast<size_t>(x0) * 4;
            unsigned char* dstLine = out.data() + static_cast<size_t>(r) * outW * 4;
            for (int c = 0; c < outW; ++c) {
                const int srcCol = mirrorX ? (outW - 1 - c) : c;
                const unsigned char* p = srcLine + static_cast<size_t>(srcCol) * 4;
                unsigned char* q = dstLine + static_cast<size_t>(c) * 4;
                q[0] = p[0];
                q[1] = p[1];
                q[2] = p[2];
                q[3] = p[3];
            }
        }
        return out;
    }
    //=============================================================================
    void
    setRawBrowserImagePayload(WebDrawPrimitive& primitive, const RGBAImageView& pic, int stride,
        bool contentToken, bool alwaysIncludePayload)
    {
        primitive.imageWidth = pic.width;
        primitive.imageHeight = pic.height;
        renderWebRawRgbaPayload(pic.pixels, pic.width, pic.height, stride, contentToken,
            primitive.imageRgba, primitive.imageToken, primitive.imagePayload,
            alwaysIncludePayload);
    }
    //=============================================================================
    struct SurfaceVertex
    {
        double x = 0;
        double y = 0;
        double z = 0;
        double r = 0;
        double g = 0;
        double b = 0;
        double a = 1;
        double u = 0;
        double v = 0;
    };
    //=============================================================================
    // RGBA texture for FaceColor 'texturemap', sampled per fragment at (u, v).
    struct SurfaceTexture
    {
        const unsigned char* pixels = nullptr;
        int width = 0;
        int height = 0;
    };
    //=============================================================================
    // (u, v) = (0, 0) is the first column of the first row: grid vertex (row, col)
    // maps to image (row, col).
    static void
    sampleTextureBilinear(
        const SurfaceTexture& tex, double u, double v, double& r, double& g, double& b, double& a)
    {
        const double fx = std::clamp(u, 0.0, 1.0) * static_cast<double>(tex.width - 1);
        const double fy = std::clamp(v, 0.0, 1.0) * static_cast<double>(tex.height - 1);
        const int x0 = static_cast<int>(std::floor(fx));
        const int y0 = static_cast<int>(std::floor(fy));
        const int x1 = std::min(x0 + 1, tex.width - 1);
        const int y1 = std::min(y0 + 1, tex.height - 1);
        const double wx = fx - x0;
        const double wy = fy - y0;
        const unsigned char* p00 = tex.pixels + (static_cast<size_t>(y0) * tex.width + x0) * 4;
        const unsigned char* p01 = tex.pixels + (static_cast<size_t>(y0) * tex.width + x1) * 4;
        const unsigned char* p10 = tex.pixels + (static_cast<size_t>(y1) * tex.width + x0) * 4;
        const unsigned char* p11 = tex.pixels + (static_cast<size_t>(y1) * tex.width + x1) * 4;
        double out[4];
        for (int k = 0; k < 4; ++k) {
            const double top = p00[k] * (1.0 - wx) + p01[k] * wx;
            const double bottom = p10[k] * (1.0 - wx) + p11[k] * wx;
            out[k] = (top * (1.0 - wy) + bottom * wy) / 255.0;
        }
        r = out[0];
        g = out[1];
        b = out[2];
        a = out[3];
    }
    //=============================================================================
    // Drops the faces of a surface that fall behind the opaque scene depth. `depth`
    // is the figure-resolution nearest-opaque z (RenderWeb::m_sceneDepth); the
    // surface is rasterized at `ss` times that resolution, so sample (x, y) maps to
    // figure pixel (x / ss, y / ss). A null `depth` disables the test.
    struct SceneDepthCull
    {
        const double* depth = nullptr;
        int w = 0;
        int h = 0;
        int ss = 1;
        double bias = 2.0e-4;
        bool
        behind(int x, int y, double z) const
        {
            if (depth == nullptr) {
                return false;
            }
            const int fx = x / ss;
            const int fy = y / ss;
            if (fx < 0 || fy < 0 || fx >= w || fy >= h) {
                return false;
            }
            const double sceneZ = depth[static_cast<size_t>(fy) * w + fx];
            return std::isfinite(sceneZ) && z > sceneZ + bias;
        }
    };
    //=============================================================================
    static double
    edgeValue(const SurfaceVertex& a, const SurfaceVertex& b, double x, double y)
    {
        return (x - a.x) * (b.y - a.y) - (y - a.y) * (b.x - a.x);
    }
    //=============================================================================
    static unsigned char
    colorByte(double value)
    {
        return static_cast<unsigned char>(std::lround(std::clamp(value, 0.0, 1.0) * 255.0));
    }
    //=============================================================================
    static void
    hashBytes(uint64_t& hash, const void* data, size_t size)
    {
        const auto* bytes = static_cast<const unsigned char*>(data);
        for (size_t i = 0; i < size; ++i) {
            hash ^= bytes[i];
            hash *= 1099511628211ULL;
        }
    }
    //=============================================================================
    static void
    rasterizeTriangle(const SurfaceVertex& v0, const SurfaceVertex& v1, const SurfaceVertex& v2,
        bool flatColor, std::vector<double>& zbuffer, std::vector<unsigned char>& rgba, int width,
        int height, bool& drewPixel, const SceneDepthCull* cull = nullptr,
        const SurfaceTexture* texture = nullptr)
    {
        // A bound texture overrides the vertex-colour fill: the texel at the
        // interpolated (u, v) is modulated by the vertex colour (baked lighting,
        // white otherwise).
        const bool sampleTexture = texture != nullptr && texture->pixels != nullptr
            && texture->width > 0 && texture->height > 0;
        const double area = edgeValue(v0, v1, v2.x, v2.y);
        if (!std::isfinite(area) || std::abs(area) < 1e-12) {
            return;
        }
        int minX = std::max(0, static_cast<int>(std::floor(std::min({ v0.x, v1.x, v2.x }))));
        int maxX = std::min(width - 1, static_cast<int>(std::ceil(std::max({ v0.x, v1.x, v2.x }))));
        int minY = std::max(0, static_cast<int>(std::floor(std::min({ v0.y, v1.y, v2.y }))));
        int maxY
            = std::min(height - 1, static_cast<int>(std::ceil(std::max({ v0.y, v1.y, v2.y }))));
        for (int y = minY; y <= maxY; ++y) {
            for (int x = minX; x <= maxX; ++x) {
                double px = x + 0.5;
                double py = y + 0.5;
                double w0 = edgeValue(v1, v2, px, py) / area;
                double w1 = edgeValue(v2, v0, px, py) / area;
                double w2 = edgeValue(v0, v1, px, py) / area;
                if (w0 < -1e-9 || w1 < -1e-9 || w2 < -1e-9) {
                    continue;
                }
                double z = w0 * v0.z + w1 * v1.z + w2 * v2.z;
                if (cull != nullptr && cull->behind(x, y, z)) {
                    continue;
                }
                size_t offset = static_cast<size_t>(y) * width + x;
                if (z >= zbuffer[offset]) {
                    continue;
                }
                zbuffer[offset] = z;
                unsigned char* pixel = rgba.data() + offset * 4;
                if (sampleTexture) {
                    const double u = w0 * v0.u + w1 * v1.u + w2 * v2.u;
                    const double v = w0 * v0.v + w1 * v1.v + w2 * v2.v;
                    double tr, tg, tb, ta;
                    sampleTextureBilinear(*texture, u, v, tr, tg, tb, ta);
                    const double mr = w0 * v0.r + w1 * v1.r + w2 * v2.r;
                    const double mg = w0 * v0.g + w1 * v1.g + w2 * v2.g;
                    const double mb = w0 * v0.b + w1 * v1.b + w2 * v2.b;
                    // Opacity comes from the vertex alpha, not the texel: the
                    // CData->RGBA bake leaves alpha at zero for an indexed colour
                    // texture, which would make the whole surface vanish.
                    const double ma = w0 * v0.a + w1 * v1.a + w2 * v2.a;
                    pixel[0] = colorByte(tr * mr);
                    pixel[1] = colorByte(tg * mg);
                    pixel[2] = colorByte(tb * mb);
                    pixel[3] = colorByte(ma);
                } else if (flatColor) {
                    pixel[0] = colorByte(v0.r);
                    pixel[1] = colorByte(v0.g);
                    pixel[2] = colorByte(v0.b);
                    pixel[3] = colorByte(v0.a);
                } else {
                    pixel[0] = colorByte(w0 * v0.r + w1 * v1.r + w2 * v2.r);
                    pixel[1] = colorByte(w0 * v0.g + w1 * v1.g + w2 * v2.g);
                    pixel[2] = colorByte(w0 * v0.b + w1 * v1.b + w2 * v2.b);
                    pixel[3] = colorByte(w0 * v0.a + w1 * v1.a + w2 * v2.a);
                }
                drewPixel = true;
            }
        }
    }
    //=============================================================================
    // Anti-aliased depth-tested thick line in the face z-buffer. A small depth bias
    // lets the edge win over its own coplanar face while a nearer face still hides
    // it. The mean-z painter sort cannot do this for the large skirt quads of
    // mesh/meshz.
    static void
    rasterizeSurfaceEdge(const SurfaceVertex& a, const SurfaceVertex& b, double halfWidth,
        double depthBias, std::vector<double>& zbuffer, std::vector<unsigned char>& rgba, int width,
        int height, bool& drewPixel, const SceneDepthCull* cull = nullptr)
    {
        const double dx = b.x - a.x;
        const double dy = b.y - a.y;
        if (!std::isfinite(dx) || !std::isfinite(dy) || !std::isfinite(a.z)
            || !std::isfinite(b.z)) {
            return;
        }
        const double len2 = dx * dx + dy * dy;
        const double pad = halfWidth + 1.0;
        int minX = std::max(0, static_cast<int>(std::floor(std::min(a.x, b.x) - pad)));
        int maxX = std::min(width - 1, static_cast<int>(std::ceil(std::max(a.x, b.x) + pad)));
        int minY = std::max(0, static_cast<int>(std::floor(std::min(a.y, b.y) - pad)));
        int maxY = std::min(height - 1, static_cast<int>(std::ceil(std::max(a.y, b.y) + pad)));
        for (int y = minY; y <= maxY; ++y) {
            for (int x = minX; x <= maxX; ++x) {
                const double px = x + 0.5;
                const double py = y + 0.5;
                double t = len2 > 0.0 ? ((px - a.x) * dx + (py - a.y) * dy) / len2 : 0.0;
                t = std::clamp(t, 0.0, 1.0);
                const double cx = a.x + t * dx;
                const double cy = a.y + t * dy;
                const double dist = std::hypot(px - cx, py - cy);
                const double coverage = std::clamp(halfWidth + 0.5 - dist, 0.0, 1.0);
                if (coverage <= 0.0) {
                    continue;
                }
                const double z = a.z + t * (b.z - a.z) - depthBias;
                if (cull != nullptr && cull->behind(x, y, z)) {
                    continue;
                }
                const size_t offset = static_cast<size_t>(y) * width + x;
                if (z >= zbuffer[offset]) {
                    continue;
                }
                const double er = a.r + t * (b.r - a.r);
                const double eg = a.g + t * (b.g - a.g);
                const double eb = a.b + t * (b.b - a.b);
                const double ea = a.a + t * (b.a - a.a);
                const double srcA = coverage * std::clamp(ea, 0.0, 1.0);
                if (srcA <= 0.0) {
                    continue;
                }
                unsigned char* pixel = rgba.data() + offset * 4;
                const double inv = 1.0 - srcA;
                pixel[0] = static_cast<unsigned char>(
                    std::lround(pixel[0] * inv + std::clamp(er, 0.0, 1.0) * 255.0 * srcA));
                pixel[1] = static_cast<unsigned char>(
                    std::lround(pixel[1] * inv + std::clamp(eg, 0.0, 1.0) * 255.0 * srcA));
                pixel[2] = static_cast<unsigned char>(
                    std::lround(pixel[2] * inv + std::clamp(eb, 0.0, 1.0) * 255.0 * srcA));
                pixel[3] = static_cast<unsigned char>(std::lround(pixel[3] * inv + 255.0 * srcA));
                // Only fully-covered samples claim the depth slot, so anti-aliased
                // fringes never punch a hole that a farther edge could bleed into.
                if (coverage >= 0.5) {
                    zbuffer[offset] = z;
                }
                drewPixel = true;
            }
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
void
RenderWeb::pushImage(const RGBAImageView& pic, double left, double top, double right, double bottom)
{
    if (pic.pixels == nullptr || pic.width <= 0 || pic.height <= 0) {
        return;
    }
    const int stride = pic.bytesPerLine > 0 ? pic.bytesPerLine : pic.width * 4;
    WebDrawPrimitive p;
    p.kind = "image";
    p.x = std::min(left, right);
    p.y = std::min(top, bottom);
    p.w = std::fabs(right - left);
    p.h = std::fabs(bottom - top);
    setRawBrowserImagePayload(
        p, pic, stride, m_encodeImagePngPayloads, m_alwaysIncludeRawImagePayload);
    if (!m_encodeImagePngPayloads) {
        m_prims.push_back(std::move(p));
        return;
    }
    std::string pngBase64;
    if (!renderWebCachedRgbaToPngBase64(
            pic.pixels, pic.width, pic.height, stride, pngBase64, p.imageToken, p.imagePayload)) {
        return;
    }
    p.image = "data:image/png;base64," + pngBase64;
    m_prims.push_back(std::move(p));
}
//=============================================================================
bool
RenderWeb::tryPushZBufferedSurface(const std::vector<std::vector<coloredPoint>>& faces,
    bool flatLayout, bool flatShade, const std::vector<std::vector<coloredPoint>>& edges,
    bool drawEdges, meshStyle meshstyle, double lineWidth, bool webglFallback, int oversample,
    bool transparent)
{
    // Faces and wireframe edges share one z-buffer so occlusion is exact: the mean-z
    // painter sort cannot order the large adjacent quads of a meshz skirt. The
    // buffer is `oversample` times the figure resolution to keep thin edges crisp
    // after the export raster box-filters.
    const int ss = std::max(1, oversample);
    const int width = std::max(1, static_cast<int>(std::lround(m_width)) * ss);
    const int height = std::max(1, static_cast<int>(std::lround(m_height)) * ss);
    std::vector<unsigned char> rgba(static_cast<size_t>(width) * height * 4, 0);
    std::vector<double> zbuffer(
        static_cast<size_t>(width) * height, std::numeric_limits<double>::infinity());
    bool drewPixel = false;
    const double scale = static_cast<double>(ss);
    // Each pixel is tested against the opaque scene depth recorded so far, and the
    // surface image blends over it in call order. Opaque surfaces bake to separate
    // full-frame images composited in call order, so an opaque surface also tests
    // (and writes) the depth: without it a later surface drawn behind an earlier
    // one painted over it (bar3 columns). The first opaque surface finds no scene
    // depth and is never culled. A transparent surface tests but never writes.
    SceneDepthCull cull;
    const SceneDepthCull* cullPtr = nullptr;
    if (!m_sceneDepth.empty() && m_depthW > 0 && m_depthH > 0) {
        cull.depth = m_sceneDepth.data();
        cull.w = m_depthW;
        cull.h = m_depthH;
        cull.ss = ss;
        cullPtr = &cull;
    }
    const bool clipActive = m_dataClip.enabled && renderClipNeededStrips(m_dataClip, faces);
    auto mapVertex = [this, scale](const coloredPoint& source, SurfaceVertex& target) {
        target.z = mapXYZ(source.x, source.y, source.z, target.x, target.y);
        target.x *= scale;
        target.y *= scale;
        target.r = source.r;
        target.g = source.g;
        target.b = source.b;
        target.a = source.a;
        target.u = source.u;
        target.v = source.v;
        return std::isfinite(target.x) && std::isfinite(target.y) && std::isfinite(target.z);
    };
    SurfaceTexture surfaceTexture;
    if (m_surfaceTextureActive && !m_surfaceTextureRgba.empty()) {
        surfaceTexture.pixels = m_surfaceTextureRgba.data();
        surfaceTexture.width = m_surfaceTextureW;
        surfaceTexture.height = m_surfaceTextureH;
    }
    const SurfaceTexture* texturePtr = surfaceTexture.pixels ? &surfaceTexture : nullptr;
    auto drawQuad = [&](const coloredPoint& p0, const coloredPoint& p1, const coloredPoint& p2,
                        const coloredPoint& p3) {
        if (clipActive) {
            std::vector<coloredPoint> clipped = renderClipPolygon(m_dataClip, { p0, p1, p3, p2 });
            if (clipped.size() < 3) {
                return;
            }
            SurfaceVertex vbase;
            if (!mapVertex(clipped[0], vbase)) {
                return;
            }
            for (size_t i = 1; i + 1 < clipped.size(); ++i) {
                SurfaceVertex va, vb;
                if (!mapVertex(clipped[i], va) || !mapVertex(clipped[i + 1], vb)) {
                    continue;
                }
                rasterizeTriangle(vbase, va, vb, flatShade, zbuffer, rgba, width, height, drewPixel,
                    cullPtr, texturePtr);
            }
            return;
        }
        SurfaceVertex v0, v1, v2, v3;
        if (!mapVertex(p0, v0) || !mapVertex(p1, v1) || !mapVertex(p2, v2) || !mapVertex(p3, v3)) {
            return;
        }
        rasterizeTriangle(
            v0, v1, v3, flatShade, zbuffer, rgba, width, height, drewPixel, cullPtr, texturePtr);
        rasterizeTriangle(
            v0, v3, v2, flatShade, zbuffer, rgba, width, height, drewPixel, cullPtr, texturePtr);
    };
    if (flatLayout && faces.size() == 1 && faces[0].size() > 4 && (faces[0].size() % 4) == 0) {
        for (size_t j = 0; j + 3 < faces[0].size(); j += 4) {
            drawQuad(faces[0][j], faces[0][j + 1], faces[0][j + 2], faces[0][j + 3]);
        }
    } else {
        for (const auto& strip : faces) {
            for (size_t j = 2; j + 1 < strip.size(); j += 2) {
                drawQuad(strip[j - 2], strip[j - 1], strip[j], strip[j + 1]);
            }
        }
    }
    // Edges go on after the faces filled the depth buffer, so edges behind an
    // opaque face are hidden.
    if (drawEdges && !edges.empty()) {
        const double halfWidth = std::max(1.0, lineWidth) * scale * 0.5;
        // An edge lies on its own face plane, so the bias only has to beat equal
        // depth plus rounding. It must stay far below the depth gap between
        // surfaces, or a back edge would punch through a nearer face. zclip is in
        // [-1, 1]; 2e-4 clears the coplanar face.
        const double depthBias = 2.0e-4;
        auto drawEdge = [&](const coloredPoint& a, const coloredPoint& b) {
            coloredPoint ca(a);
            coloredPoint cb(b);
            if (clipActive) {
                double x0 = a.x, y0 = a.y, z0 = a.z, x1 = b.x, y1 = b.y, z1 = b.z;
                if (!renderClipSegment(
                        m_dataClip, x0, y0, z0, x1, y1, z1, x0, y0, z0, x1, y1, z1)) {
                    return;
                }
                ca.x = x0;
                ca.y = y0;
                ca.z = z0;
                cb.x = x1;
                cb.y = y1;
                cb.z = z1;
            }
            SurfaceVertex va, vb;
            if (!mapVertex(ca, va) || !mapVertex(cb, vb)) {
                return;
            }
            rasterizeSurfaceEdge(
                va, vb, halfWidth, depthBias, zbuffer, rgba, width, height, drewPixel, cullPtr);
        };
        for (const auto& strip : edges) {
            for (size_t j = 2; j + 1 < strip.size(); j += 2) {
                const coloredPoint& c0 = strip[j - 2];
                const coloredPoint& c1 = strip[j - 1];
                const coloredPoint& c2 = strip[j + 1];
                const coloredPoint& c3 = strip[j];
                if (meshstyle == meshStyle::Both || meshstyle == meshStyle::Column) {
                    drawEdge(c0, c1);
                    drawEdge(c2, c3);
                }
                if (meshstyle == meshStyle::Both || meshstyle == meshStyle::Row) {
                    drawEdge(c1, c2);
                    drawEdge(c3, c0);
                }
            }
        }
    }
    if (!drewPixel) {
        return false;
    }
    // Downsample the nearest depths into the figure-resolution scene depth so
    // markers drawn later can be depth-tested against this surface. Keep the
    // nearest sample of each ss x ss block, and the nearest across surfaces.
    const int fw = std::max(1, static_cast<int>(std::lround(m_width)));
    const int fh = std::max(1, static_cast<int>(std::lround(m_height)));
    // A transparent surface tests the opaque depth but never writes it: it must not
    // occlude later transparent layers or markers on the geometry it blends over.
    if (!transparent && m_sceneDepth.empty()) {
        m_depthW = fw;
        m_depthH = fh;
        m_sceneDepth.assign(static_cast<size_t>(fw) * fh, std::numeric_limits<double>::infinity());
    }
    if (!transparent && m_depthW == fw && m_depthH == fh) {
        for (int fy = 0; fy < fh; ++fy) {
            for (int fx = 0; fx < fw; ++fx) {
                double nearest = std::numeric_limits<double>::infinity();
                for (int sy = 0; sy < ss && fy * ss + sy < height; ++sy) {
                    for (int sx = 0; sx < ss && fx * ss + sx < width; ++sx) {
                        const double z
                            = zbuffer[static_cast<size_t>(fy * ss + sy) * width + (fx * ss + sx)];
                        if (z < nearest) {
                            nearest = z;
                        }
                    }
                }
                if (std::isfinite(nearest)) {
                    writeSceneDepth(fx, fy, nearest);
                }
            }
        }
    }
    WebDrawPrimitive primitive;
    primitive.kind = "image";
    primitive.x = 0;
    primitive.y = 0;
    // 1x screen units; the raster maps the oversampled pixels back down.
    primitive.w = static_cast<double>(width) / scale;
    primitive.h = static_cast<double>(height) / scale;
    std::string pngBase64;
    if (!renderWebCachedRgbaToPngBase64(rgba.data(), width, height, width * 4, pngBase64,
            primitive.imageToken, primitive.imagePayload)) {
        return false;
    }
    primitive.image = "data:image/png;base64," + pngBase64;
    primitive.depth = -2.0;
    // The browser draws the WebGL mesh when one was recorded; this image is a
    // fallback kept for the software capture path (getframe / saveas).
    primitive.webglFallback = webglFallback;
    m_pending3d.push_back(std::move(primitive));
    return true;
}
//=============================================================================
void
RenderWeb::drawImage(int x1, int y1, const RGBAImageView& pic)
{
    pushImage(pic, x1, y1, x1 + pic.width, y1 + pic.height);
}
//=============================================================================
void
RenderWeb::drawImage(double x1, double y1, double x2, double y2, const RGBAImageView& pic)
{
    double px1, py1, px2, py2;
    mapXY(x1, y1, 0, px1, py1);
    mapXY(x2, y2, 0, px2, py2);
    pushImage(pic, px1, py1, px2, py2);
}
//=============================================================================
void
RenderWeb::drawImage(const std::vector<double>& xp, const std::vector<double>& yp,
    const std::vector<double>& xLim, const std::vector<double>& yLim, bool xFlip, bool yFlip,
    const RGBAImageView& picView)
{
    // Expand the data range by half a pixel and clip to the axes limits before
    // mapping to screen.
    const int picW = picView.width;
    const int picH = picView.height;
    if (xp.size() < 2 || yp.size() < 2 || xLim.size() < 2 || yLim.size() < 2
        || picView.pixels == nullptr || picW <= 0 || picH <= 0) {
        return;
    }
    const double dx = picW > 1 ? (xp[1] - xp[0]) / (double)(picW - 1) : 1.0;
    const double dy = picH > 1 ? (yp[1] - yp[0]) / (double)(picH - 1) : 1.0;
    const double dataX1 = xp[0] - dx / 2.0;
    const double dataY1 = yp[0] - dy / 2.0;
    const double dataX2 = xp[1] + dx / 2.0;
    const double dataY2 = yp[1] + dy / 2.0;
    if (dataX2 == dataX1 || dataY2 == dataY1) {
        return;
    }

    // Clip against the ORDERED data interval: a descending span (YData = [hi lo],
    // as movie() uses) clamped against its raw endpoints collapses to a point and
    // drops the image. The descending axis is mirrored via mirrorX/mirrorY below.
    const double dataXlo = std::min(dataX1, dataX2);
    const double dataXhi = std::max(dataX1, dataX2);
    const double dataYlo = std::min(dataY1, dataY2);
    const double dataYhi = std::max(dataY1, dataY2);
    double visX1 = std::clamp(xLim[0], dataXlo, dataXhi);
    double visX2 = std::clamp(xLim[1], dataXlo, dataXhi);
    double visY1 = std::clamp(yLim[0], dataYlo, dataYhi);
    double visY2 = std::clamp(yLim[1], dataYlo, dataYhi);

    // Source sub-rectangle inside the axes limits. Squeezing the whole raster into
    // the visible rect is wrong once the axes are zoomed or the limits are set.
    double imgX1 = (visX1 - dataX1) * picW / (dataX2 - dataX1);
    double imgX2 = (visX2 - dataX1) * picW / (dataX2 - dataX1);
    double imgY1 = (visY1 - dataY1) * picH / (dataY2 - dataY1);
    double imgY2 = (visY2 - dataY1) * picH / (dataY2 - dataY1);

    if (yFlip) {
        visY1 = yLim[0] + yLim[1] - visY1;
        visY2 = yLim[0] + yLim[1] - visY2;
    } else {
        const double t = picH - imgY2;
        imgY2 = picH - imgY1;
        imgY1 = t;
    }
    if (xFlip) {
        visX1 = xLim[0] + xLim[1] - visX1;
        visX2 = xLim[0] + xLim[1] - visX2;
        const double t = picW - imgX2;
        imgX2 = picW - imgX1;
        imgX1 = t;
    }

    double px1, py1, px2, py2;
    mapXY(std::min(visX1, visX2), std::max(visY1, visY2), 0, px1, py1);
    mapXY(std::max(visX1, visX2), std::min(visY1, visY2), 0, px2, py2);

    const int sx = static_cast<int>(std::floor(std::min(imgX1, imgX2)));
    const int sy = static_cast<int>(std::floor(std::min(imgY1, imgY2)));
    const int sw = std::max(1, static_cast<int>(std::ceil(std::fabs(imgX2 - imgX1))));
    const int sh = std::max(1, static_cast<int>(std::ceil(std::fabs(imgY2 - imgY1))));
    // A descending span mirrors the picture along that axis (movie() flips YData
    // to put row 1 at the top).
    const bool xDescending = xp[1] < xp[0];
    const bool yDescending = yp[1] < yp[0];
    const bool mirrorX = (xFlip != xDescending);
    const bool mirrorY = ((!yFlip) != yDescending);

    int outW = 0, outH = 0;
    std::vector<unsigned char> buffer
        = cropAndOrientRGBA(picView, sx, sy, sw, sh, mirrorX, mirrorY, outW, outH);
    if (outW <= 0 || outH <= 0) {
        return;
    }
    RGBAImageView oriented;
    oriented.width = outW;
    oriented.height = outH;
    oriented.bytesPerLine = outW * 4;
    oriented.pixels = buffer.data();
    pushImage(oriented, px1, py1, px2, py2);
}
//=============================================================================
bool
RenderWeb::tryPushWebGlPatch(const FaceList& faces, const PatchRenderOptions& options)
{
    if (faces.empty()) {
        return false;
    }
    const Face& first = faces.front();
    const bool drawFaces = first.FaceColorMode != ColorMode::None;
    const bool drawEdges = options.lineStyle == L"-" && options.lineWidth > 0.0
        && first.EdgeColorMode != ColorMode::None;
    if (!drawFaces && !drawEdges) {
        return false;
    }
    if (drawFaces && hasConcaveFilledXYRun(faces)) {
        return false;
    }
    // The WebGL line program strokes every edge with one uniform color. Edge colors
    // that vary per vertex (EdgeColor 'flat'/'interp') go to the software path.
    if (drawEdges
        && (first.EdgeColorMode == ColorMode::Flat || first.EdgeColorMode == ColorMode::Interp)
        && first.edgecolors.size() >= 2) {
        const RGBAColorData& c0 = first.edgecolors.front();
        for (size_t i = 1; i < first.edgecolors.size(); ++i) {
            const RGBAColorData& ci = first.edgecolors[i];
            if (std::abs(ci.r - c0.r) > 1e-9 || std::abs(ci.g - c0.g) > 1e-9
                || std::abs(ci.b - c0.b) > 1e-9 || std::abs(ci.a - c0.a) > 1e-9) {
                return false;
            }
        }
    }
    const RGBAColorData edge = first.EdgeColor;
    WebGlMesh mesh;
    mesh.drawFaces = drawFaces;
    mesh.lineColor = { static_cast<float>(edge.r), static_cast<float>(edge.g),
        static_cast<float>(edge.b), static_cast<float>(edge.a) };
    mesh.lineWidth = options.lineWidth;
    std::copy(model, model + 16, mesh.model.begin());
    std::copy(proj, proj + 16, mesh.projection.begin());
    std::copy(viewp, viewp + 4, mesh.viewport.begin());

    // Lit colors are baked per vertex and depend on the view. GOPatch does not
    // re-token on view change, so its geometry cache (which stores colors) is
    // bypassed when lit, or a rotation would replay stale lighting.
    const bool hasLights = m_lightingScene.enabled && !m_lightingScene.lights.empty();
    const bool lightFaces = hasLights && options.faceLighting != LightingMode::None;
    const bool lightEdges = hasLights && options.edgeLighting != LightingMode::None;
    const bool useGeometryCache = !lightFaces;

    // The line program takes one uniform lineColor, so light it once from a
    // representative face normal. Set before the cache lookup and never cached, so
    // it follows view changes even on a cache hit.
    if (lightEdges && !first.normals.empty() && !first.vertices.empty()) {
        RGBAColorData litEdge = RenderLighting::computeLitColor(
            edge, first.normals[0], first.vertices[0], model, m_lightingScene, m_lightingMaterial);
        mesh.lineColor = { static_cast<float>(litEdge.r), static_cast<float>(litEdge.g),
            static_cast<float>(litEdge.b), static_cast<float>(litEdge.a) };
    }

    if (useGeometryCache && options.geometryToken != 0
        && shouldCacheWireframeGeometry(options.geometryToken)) {
        const auto cached = wireframeGeometryCache().find(options.geometryToken);
        if (cached != wireframeGeometryCache().end()) {
            updateMeshBufferLengths(mesh, cached->second);
            mesh.positions = cached->second.positions;
            mesh.vertexColors = cached->second.vertexColors;
            mesh.triangleIndices = cached->second.triangleIndices;
            mesh.lineIndices = cached->second.lineIndices;
            mesh.drawFaces = cached->second.drawFaces;
            mesh.revision = options.geometryToken;
            m_webglMeshes.push_back(std::move(mesh));
            m_has3d = true;
            return true;
        }
    }

    size_t vertexCountEstimate = 0;
    for (const Face& face : faces) {
        vertexCountEstimate += face.vertices.size();
    }
    mesh.positions.reserve(vertexCountEstimate * 3);
    if (drawFaces) {
        mesh.vertexColors.reserve(vertexCountEstimate * 4);
    }
    mesh.triangleIndices.reserve(vertexCountEstimate * 3);
    if (drawEdges) {
        mesh.lineIndices.reserve(vertexCountEstimate * 2);
    }
    WebGlVertexMap vertexMap;
    vertexMap.reserve(vertexCountEstimate);
    auto flushRun = [&](const std::vector<uint32_t>& run) {
        if (run.size() < 2) {
            return;
        }
        if (drawEdges) {
            for (size_t i = 1; i < run.size(); ++i) {
                mesh.lineIndices.push_back(run[i - 1]);
                mesh.lineIndices.push_back(run[i]);
            }
            mesh.lineIndices.push_back(run.back());
            mesh.lineIndices.push_back(run.front());
        }
        for (size_t i = 2; i < run.size(); ++i) {
            mesh.triangleIndices.push_back(run.front());
            mesh.triangleIndices.push_back(run[i - 1]);
            mesh.triangleIndices.push_back(run[i]);
        }
    };

    for (const Face& face : faces) {
        if ((face.FaceColorMode != ColorMode::None) != drawFaces
            || (drawEdges
                && (face.EdgeColorMode != first.EdgeColorMode
                    || std::abs(face.EdgeColor.r - edge.r) > 1e-9
                    || std::abs(face.EdgeColor.g - edge.g) > 1e-9
                    || std::abs(face.EdgeColor.b - edge.b) > 1e-9
                    || std::abs(face.EdgeColor.a - edge.a) > 1e-9))) {
            return false;
        }
        std::vector<uint32_t> run;
        for (size_t vertexIndex = 0; vertexIndex < face.vertices.size(); ++vertexIndex) {
            const point& vertex = face.vertices[vertexIndex];
            if (!std::isfinite(vertex.x) || !std::isfinite(vertex.y) || !std::isfinite(vertex.z)) {
                flushRun(run);
                run.clear();
                continue;
            }
            float r = 0.0;
            float g = 0.0;
            float b = 0.0;
            float a = 0.0;
            if (drawFaces) {
                RGBAColorData color = face.FaceColor;
                if ((face.FaceColorMode == ColorMode::Flat
                        || face.FaceColorMode == ColorMode::Interp)
                    && !face.vertexcolors.empty()) {
                    const size_t colorIndex = face.FaceColorMode == ColorMode::Flat
                        ? 0
                        : std::min(vertexIndex, face.vertexcolors.size() - 1);
                    color = face.vertexcolors[colorIndex];
                }
                if (lightFaces && vertexIndex < face.normals.size()) {
                    color = RenderLighting::computeLitColor(color, face.normals[vertexIndex],
                        vertex, model, m_lightingScene, m_lightingMaterial);
                }
                r = static_cast<float>(color.r);
                g = static_cast<float>(color.g);
                b = static_cast<float>(color.b);
                a = static_cast<float>(color.a);
            }
            const uint32_t index
                = appendWebGlVertex(mesh, vertexMap, drawFaces, static_cast<float>(vertex.x),
                    static_cast<float>(vertex.y), static_cast<float>(vertex.z), r, g, b, a);
            run.push_back(index);
        }
        flushRun(run);
    }
    if (mesh.positions.empty() || mesh.triangleIndices.empty()
        || (drawEdges && mesh.lineIndices.empty())) {
        return false;
    }
    if (useGeometryCache && options.geometryToken != 0) {
        storeWireframeGeometry(options.geometryToken,
            CachedWireframeGeometry { mesh.positions, mesh.vertexColors, mesh.triangleIndices,
                mesh.lineIndices, mesh.drawFaces });
        mesh.revision = options.geometryToken;
    } else {
        uint64_t hash = 1469598103934665603ULL;
        hashBytes(hash, mesh.positions.data(), mesh.positions.size() * sizeof(float));
        if (!mesh.vertexColors.empty()) {
            hashBytes(hash, mesh.vertexColors.data(), mesh.vertexColors.size() * sizeof(float));
        }
        hashBytes(
            hash, mesh.triangleIndices.data(), mesh.triangleIndices.size() * sizeof(uint32_t));
        hashBytes(hash, mesh.lineIndices.data(), mesh.lineIndices.size() * sizeof(uint32_t));
        mesh.revision = hash;
    }
    updateMeshBufferLengths(mesh);
    m_webglMeshes.push_back(std::move(mesh));
    m_has3d = true;
    return true;
}
//=============================================================================
bool
RenderWeb::tryPushWebGlSurface(const std::vector<std::vector<coloredPoint>>& faces, bool flatfaces,
    const std::vector<std::vector<coloredPoint>>& edges, meshStyle meshstyle, double lineWidth,
    const std::wstring& lineStyle, uint64_t geometryToken)
{
    if (faces.empty()) {
        return false;
    }
    // Clip, and bypass the geometry cache, only when the surface crosses the axes box.
    const bool clipActive = m_dataClip.enabled && renderClipNeededStrips(m_dataClip, faces);
    const bool drawEdges = !edges.empty() && lineStyle == L"-" && lineWidth > 0.0;
    RGBAColorData edgeColor;
    if (drawEdges) {
        bool found = false;
        for (const auto& strip : edges) {
            for (const coloredPoint& point : strip) {
                if (!std::isfinite(point.x) || !std::isfinite(point.y) || !std::isfinite(point.z)) {
                    continue;
                }
                const RGBAColorData current(point.r, point.g, point.b, point.a);
                if (!found) {
                    edgeColor = current;
                    found = true;
                } else if (std::abs(current.r - edgeColor.r) > 1e-9
                    || std::abs(current.g - edgeColor.g) > 1e-9
                    || std::abs(current.b - edgeColor.b) > 1e-9
                    || std::abs(current.a - edgeColor.a) > 1e-9) {
                    return false;
                }
            }
        }
        if (!found) {
            return false;
        }
    }

    WebGlMesh mesh;
    mesh.drawFaces = true;
    mesh.lineWidth = lineWidth;
    mesh.lineColor = { static_cast<float>(edgeColor.r), static_cast<float>(edgeColor.g),
        static_cast<float>(edgeColor.b), static_cast<float>(edgeColor.a) };
    std::copy(model, model + 16, mesh.model.begin());
    std::copy(proj, proj + 16, mesh.projection.begin());
    std::copy(viewp, viewp + 4, mesh.viewport.begin());

    // The cached geometry is unclipped; when an axes clip box is active the mesh
    // depends on the current limits, so bypass the geometry cache entirely.
    if (geometryToken != 0 && !clipActive && shouldCacheWireframeGeometry(geometryToken)) {
        const auto cached = wireframeGeometryCache().find(geometryToken);
        if (cached != wireframeGeometryCache().end()) {
            updateMeshBufferLengths(mesh, cached->second);
            mesh.positions = cached->second.positions;
            mesh.vertexColors = cached->second.vertexColors;
            mesh.triangleIndices = cached->second.triangleIndices;
            mesh.lineIndices = cached->second.lineIndices;
            mesh.drawFaces = cached->second.drawFaces;
            mesh.revision = geometryToken;
            m_webglMeshes.push_back(std::move(mesh));
            m_has3d = true;
            return true;
        }
    }

    size_t quadCount = 0;
    if (flatfaces && faces.size() == 1 && faces[0].size() > 4 && (faces[0].size() % 4) == 0) {
        quadCount = faces[0].size() / 4;
    } else {
        for (const auto& strip : faces) {
            if (strip.size() >= 4) {
                quadCount += (strip.size() - 2) / 2;
            }
        }
    }
    mesh.positions.reserve(quadCount * 12);
    mesh.vertexColors.reserve(quadCount * 16);
    mesh.triangleIndices.reserve(quadCount * 6);
    WebGlVertexMap vertexMap;
    vertexMap.reserve(quadCount * 4);
    if (drawEdges) {
        size_t edgeIndexCount = 0;
        if (meshstyle == meshStyle::Both || meshstyle == meshStyle::Column) {
            edgeIndexCount += 4;
        }
        if (meshstyle == meshStyle::Both || meshstyle == meshStyle::Row) {
            edgeIndexCount += 4;
        }
        mesh.lineIndices.reserve(quadCount * edgeIndexCount);
    }
    auto addFanVertex = [&](const coloredPoint& point) -> uint32_t {
        return appendWebGlVertex(mesh, vertexMap, true, static_cast<float>(point.x),
            static_cast<float>(point.y), static_cast<float>(point.z), static_cast<float>(point.r),
            static_cast<float>(point.g), static_cast<float>(point.b), static_cast<float>(point.a));
    };
    auto addClippedQuad = [&](const coloredPoint& p0, const coloredPoint& p1,
                              const coloredPoint& p2, const coloredPoint& p3) {
        std::vector<coloredPoint> clipped = renderClipPolygon(m_dataClip, { p0, p1, p3, p2 });
        if (clipped.size() < 3) {
            return;
        }
        std::vector<uint32_t> ring;
        ring.reserve(clipped.size());
        for (const coloredPoint& cp : clipped) {
            // Flat faces take the provoking-vertex color (p0) at every vertex.
            coloredPoint vertex = flatfaces
                ? coloredPoint(cp.x, cp.y, cp.z, p0.r, p0.g, p0.b, p0.a, cp.nx, cp.ny, cp.nz)
                : cp;
            ring.push_back(addFanVertex(vertex));
        }
        for (size_t i = 1; i + 1 < ring.size(); ++i) {
            mesh.triangleIndices.push_back(ring[0]);
            mesh.triangleIndices.push_back(ring[i]);
            mesh.triangleIndices.push_back(ring[i + 1]);
        }
        if (drawEdges) {
            for (size_t i = 0; i < ring.size(); ++i) {
                mesh.lineIndices.push_back(ring[i]);
                mesh.lineIndices.push_back(ring[(i + 1) % ring.size()]);
            }
        }
    };
    auto addQuad = [&](const coloredPoint& p0, const coloredPoint& p1, const coloredPoint& p2,
                       const coloredPoint& p3) {
        if (clipActive) {
            addClippedQuad(p0, p1, p2, p3);
            return;
        }
        const coloredPoint points[4] = { p0, p1, p2, p3 };
        for (const coloredPoint& point : points) {
            if (!std::isfinite(point.x) || !std::isfinite(point.y) || !std::isfinite(point.z)) {
                return;
            }
        }
        uint32_t indices[4];
        const coloredPoint& flatColor = points[0];
        for (size_t pointIndex = 0; pointIndex < 4; ++pointIndex) {
            const coloredPoint& point = points[pointIndex];
            const coloredPoint& color = flatfaces ? flatColor : point;
            indices[pointIndex]
                = appendWebGlVertex(mesh, vertexMap, true, static_cast<float>(point.x),
                    static_cast<float>(point.y), static_cast<float>(point.z),
                    static_cast<float>(color.r), static_cast<float>(color.g),
                    static_cast<float>(color.b), static_cast<float>(color.a));
        }
        mesh.triangleIndices.push_back(indices[0]);
        mesh.triangleIndices.push_back(indices[1]);
        mesh.triangleIndices.push_back(indices[3]);
        mesh.triangleIndices.push_back(indices[0]);
        mesh.triangleIndices.push_back(indices[3]);
        mesh.triangleIndices.push_back(indices[2]);
        if (!drawEdges) {
            return;
        }
        if (meshstyle == meshStyle::Both || meshstyle == meshStyle::Column) {
            mesh.lineIndices.push_back(indices[0]);
            mesh.lineIndices.push_back(indices[1]);
            mesh.lineIndices.push_back(indices[2]);
            mesh.lineIndices.push_back(indices[3]);
        }
        if (meshstyle == meshStyle::Both || meshstyle == meshStyle::Row) {
            mesh.lineIndices.push_back(indices[0]);
            mesh.lineIndices.push_back(indices[2]);
            mesh.lineIndices.push_back(indices[1]);
            mesh.lineIndices.push_back(indices[3]);
        }
    };
    if (flatfaces && faces.size() == 1 && faces[0].size() > 4 && (faces[0].size() % 4) == 0) {
        for (size_t index = 0; index + 3 < faces[0].size(); index += 4) {
            addQuad(faces[0][index], faces[0][index + 1], faces[0][index + 2], faces[0][index + 3]);
        }
    } else {
        for (const auto& strip : faces) {
            for (size_t index = 2; index + 1 < strip.size(); index += 2) {
                addQuad(strip[index - 2], strip[index - 1], strip[index], strip[index + 1]);
            }
        }
    }
    if (mesh.positions.empty() || mesh.triangleIndices.empty()) {
        return false;
    }
    if (geometryToken != 0 && !clipActive) {
        storeWireframeGeometry(geometryToken,
            CachedWireframeGeometry { mesh.positions, mesh.vertexColors, mesh.triangleIndices,
                mesh.lineIndices, mesh.drawFaces });
        mesh.revision = geometryToken;
    } else {
        uint64_t hash = 1469598103934665603ULL;
        hashBytes(hash, mesh.positions.data(), mesh.positions.size() * sizeof(float));
        hashBytes(hash, mesh.vertexColors.data(), mesh.vertexColors.size() * sizeof(float));
        hashBytes(
            hash, mesh.triangleIndices.data(), mesh.triangleIndices.size() * sizeof(uint32_t));
        hashBytes(hash, mesh.lineIndices.data(), mesh.lineIndices.size() * sizeof(uint32_t));
        mesh.revision = hash;
    }
    updateMeshBufferLengths(mesh);
    m_webglMeshes.push_back(std::move(mesh));
    m_has3d = true;
    return true;
}
//=============================================================================
bool
RenderWeb::tryPushZBufferedPatch(
    const FaceList& faces, const PatchRenderOptions& options, bool webglFallback)
{
    // Smooth-lit patches (isosurface, Gouraud) need per-pixel color interpolation in
    // the software capture: the single-color polygon fill shows mesh facets and a
    // hard specular polygon instead of a soft highlight. Rasterize the lit
    // per-vertex colors into a z-buffer. The browser keeps drawing the WebGL mesh,
    // so this image is a fallback.
    const bool hasLights = m_lightingScene.enabled && !m_lightingScene.lights.empty();
    const bool lightFaces = hasLights && options.faceLighting != LightingMode::None;
    const bool gouraud = lightFaces && options.faceLighting == LightingMode::Gouraud;
    // A non-lit FaceColor 'interp' patch needs the same: the flat polygon fallback
    // collapses each face to vertexcolors[0], losing the per-vertex gradient.
    bool interpFill = false;
    for (const Face& face : faces) {
        if (face.FaceColorMode == ColorMode::Interp && face.vertices.size() >= 3
            && face.vertexcolors.size() == face.vertices.size()) {
            interpFill = true;
            break;
        }
    }
    if (!gouraud && !interpFill) {
        return false;
    }
    // A Gouraud-lit patch that also draws edges is left to the flat polygon path
    // (fill + outline). A non-lit 'interp' patch rasterizes its fill here and the
    // caller strokes the edges on top.
    const bool strokeEdges
        = options.lineStyle != GO_PROPERTY_VALUE_NONE_STR && options.lineWidth > 0.;
    if (gouraud && strokeEdges && !faces.empty()
        && faces.front().EdgeColorMode != ColorMode::None) {
        return false;
    }
    bool anyOpaqueFace = false;
    for (const Face& face : faces) {
        if (face.FaceColorMode == ColorMode::None || face.vertices.size() < 3) {
            continue;
        }
        double alpha = face.FaceColor.a;
        if ((face.FaceColorMode == ColorMode::Flat || face.FaceColorMode == ColorMode::Interp)
            && !face.vertexcolors.empty()) {
            alpha = face.vertexcolors[0].a;
        }
        if (alpha < 0.999) {
            // The z-buffer overwrites and cannot blend; leave translucent patches to
            // the painter-sorted polygon path.
            return false;
        }
        anyOpaqueFace = true;
    }
    if (!anyOpaqueFace) {
        return false;
    }
    const int width = std::max(1, static_cast<int>(std::lround(m_width)));
    const int height = std::max(1, static_cast<int>(std::lround(m_height)));
    std::vector<unsigned char> rgba(static_cast<size_t>(width) * height * 4, 0);
    std::vector<double> zbuffer(
        static_cast<size_t>(width) * height, std::numeric_limits<double>::infinity());
    bool drewPixel = false;
    auto litVertex = [&](const Face& face, size_t vertexIndex, SurfaceVertex& target) -> bool {
        const point& vertex = face.vertices[vertexIndex];
        if (!std::isfinite(vertex.x) || !std::isfinite(vertex.y) || !std::isfinite(vertex.z)) {
            return false;
        }
        RGBAColorData color = face.FaceColor;
        if ((face.FaceColorMode == ColorMode::Flat || face.FaceColorMode == ColorMode::Interp)
            && !face.vertexcolors.empty()) {
            const size_t colorIndex = face.FaceColorMode == ColorMode::Flat
                ? 0
                : std::min(vertexIndex, face.vertexcolors.size() - 1);
            color = face.vertexcolors[colorIndex];
        }
        if (vertexIndex < face.normals.size()) {
            color = RenderLighting::computeLitColor(color, face.normals[vertexIndex], vertex, model,
                m_lightingScene, m_lightingMaterial);
        }
        target.z = mapXYZ(vertex.x, vertex.y, vertex.z, target.x, target.y);
        target.r = color.r;
        target.g = color.g;
        target.b = color.b;
        target.a = color.a;
        return std::isfinite(target.x) && std::isfinite(target.y) && std::isfinite(target.z);
    };
    double depthSum = 0.;
    size_t depthCount = 0;
    for (const Face& face : faces) {
        if (face.FaceColorMode == ColorMode::None || face.vertices.size() < 3) {
            continue;
        }
        // NaN vertices split a face into runs; fan-triangulate each run on its own.
        std::vector<SurfaceVertex> run;
        run.reserve(face.vertices.size());
        auto flushRun = [&]() {
            for (size_t i = 2; i < run.size(); ++i) {
                rasterizeTriangle(
                    run[0], run[i - 1], run[i], false, zbuffer, rgba, width, height, drewPixel);
            }
            run.clear();
        };
        for (size_t vertexIndex = 0; vertexIndex < face.vertices.size(); ++vertexIndex) {
            SurfaceVertex sv;
            if (!litVertex(face, vertexIndex, sv)) {
                flushRun();
                continue;
            }
            depthSum += sv.z;
            depthCount++;
            run.push_back(sv);
        }
        flushRun();
    }
    if (!drewPixel) {
        return false;
    }
    WebDrawPrimitive primitive;
    primitive.kind = "image";
    primitive.x = 0;
    primitive.y = 0;
    primitive.w = static_cast<double>(width);
    primitive.h = static_cast<double>(height);
    std::string pngBase64;
    if (!renderWebCachedRgbaToPngBase64(rgba.data(), width, height, width * 4, pngBase64,
            primitive.imageToken, primitive.imagePayload)) {
        return false;
    }
    primitive.image = "data:image/png;base64," + pngBase64;
    // A Gouraud-lit fill keeps the far -2.0 depth so it composites behind the
    // strokes. A non-lit 'interp' fill sits at the patch mean depth so the edge
    // strokes (biased toward the camera) draw on top, as in the flat polygon path.
    primitive.depth
        = gouraud ? -2.0 : (depthCount ? depthSum / static_cast<double>(depthCount) : -2.0);
    primitive.webglFallback = webglFallback;
    m_pending3d.push_back(std::move(primitive));
    return true;
}
//=============================================================================
void
RenderWeb::drawPatch(const FaceList& faces, const PatchRenderOptions& options)
{
    // Each face becomes a filled polygon plus an optional outline; NaN vertices
    // split it into sub-paths, as in RenderQt.
    const bool strokeEdges
        = options.lineStyle != GO_PROPERTY_VALUE_NONE_STR && options.lineWidth > 0.;
    const bool webglPatch = tryPushWebGlPatch(faces, options);
    if (webglPatch && !m_softwareFallbackEnabled) {
        return;
    }
    // Gouraud-lit and non-lit 'interp' patches are rasterized into a z-buffer: the
    // flat polygon fill below collapses each face to one shade. With edges, the
    // polygon path below still strokes them (fillRasterized suppresses its fill so
    // faces are not painted twice).
    const bool fillRasterized = tryPushZBufferedPatch(faces, options, webglPatch);
    if (fillRasterized && !strokeEdges) {
        return;
    }
    // The polygon fallback fills each face with one flat color (a 2D canvas polygon
    // cannot gradient-fill a face), so lighting is baked per face from a
    // representative normal.
    const bool hasLights = m_lightingScene.enabled && !m_lightingScene.lights.empty();
    const bool lightFaces = hasLights && options.faceLighting != LightingMode::None;
    const bool lightEdges = hasLights && options.edgeLighting != LightingMode::None;
    for (const Face& face : faces) {
        if (face.vertices.size() < 3) {
            continue;
        }
        RGBAColorData fill = face.FaceColor;
        if ((face.FaceColorMode == ColorMode::Flat || face.FaceColorMode == ColorMode::Interp)
            && !face.vertexcolors.empty()) {
            fill = face.vertexcolors[0];
        }
        RGBAColorData edge = face.EdgeColor;
        if (face.EdgeColorMode == ColorMode::Flat && !face.edgecolors.empty()) {
            edge = face.edgecolors[0];
        }
        if ((lightFaces || lightEdges) && !face.normals.empty()) {
            const point& nrm = face.normals[0];
            const point& vtx = face.vertices[0];
            if (lightFaces) {
                fill = RenderLighting::computeLitColor(
                    fill, nrm, vtx, model, m_lightingScene, m_lightingMaterial);
            }
            if (lightEdges) {
                edge = RenderLighting::computeLitColor(
                    edge, nrm, vtx, model, m_lightingScene, m_lightingMaterial);
            }
        }
        const bool wantFill = !fillRasterized && face.FaceColorMode != ColorMode::None;
        const bool wantEdge = strokeEdges && face.EdgeColorMode != ColorMode::None;
        if (!wantFill && !wantEdge) {
            continue;
        }
        // EdgeColor 'flat'/'interp' (waterfall row line): a single 2D stroke cannot
        // gradient, so when the per-vertex edge colors differ each edge segment is
        // emitted as its own colored line and the fill loses its outline.
        std::vector<RGBAColorData> edgeVertexColors;
        bool gradientEdge = false;
        if (wantEdge
            && (face.EdgeColorMode == ColorMode::Flat || face.EdgeColorMode == ColorMode::Interp)
            && face.edgecolors.size() == face.vertices.size() && face.edgecolors.size() >= 2) {
            edgeVertexColors.reserve(face.edgecolors.size());
            const point& nrm = face.normals.empty() ? point() : face.normals[0];
            const point& vtx = face.vertices[0];
            for (const RGBAColorData& ec : face.edgecolors) {
                RGBAColorData lit = ec;
                if (lightEdges && !face.normals.empty()) {
                    lit = RenderLighting::computeLitColor(
                        lit, nrm, vtx, model, m_lightingScene, m_lightingMaterial);
                }
                edgeVertexColors.push_back(lit);
            }
            const RGBAColorData& c0 = edgeVertexColors.front();
            for (size_t i = 1; i < edgeVertexColors.size(); ++i) {
                const RGBAColorData& ci = edgeVertexColors[i];
                if (std::abs(ci.r - c0.r) > 1e-9 || std::abs(ci.g - c0.g) > 1e-9
                    || std::abs(ci.b - c0.b) > 1e-9 || std::abs(ci.a - c0.a) > 1e-9) {
                    gradientEdge = true;
                    break;
                }
            }
        }
        const bool interpEdge = face.EdgeColorMode == ColorMode::Interp;
        std::vector<std::pair<double, double>> pts;
        std::vector<size_t> ptVertexIndex; // maps each pt back to its source vertex
        double depthSum = 0;
        size_t depthCount = 0;
        // NB: cannot be named `emit`; that is a Qt keyword macro.
        auto emitSubPath = [&]() {
            if (pts.size() < 3) {
                pts.clear();
                ptVertexIndex.clear();
                return;
            }
            const double depth = depthCount ? depthSum / (double)depthCount : 0.0;
            // No baked-in outline when the edge is color-graded: the per-segment
            // lines below carry it.
            if (wantFill) {
                WebDrawPrimitive p;
                p.kind = "polygon";
                p.points = pts;
                p.color = colorString({ fill.r, fill.g, fill.b, fill.a });
                if (wantEdge && !gradientEdge) {
                    p.stroke = colorString({ edge.r, edge.g, edge.b, edge.a });
                    p.width = options.lineWidth;
                }
                p.depth = depth;
                p.webglFallback = webglPatch;
                (m_inDirect ? m_prims : m_pending3d).push_back(std::move(p));
            }
            if (wantEdge && gradientEdge) {
                auto segmentColor = [&](size_t a, size_t b) -> RGBAColorData {
                    const RGBAColorData& ca = edgeVertexColors[a];
                    if (!interpEdge) {
                        return ca; // flat: segment takes its start vertex color
                    }
                    const RGBAColorData& cb = edgeVertexColors[b];
                    return RGBAColorData(0.5 * (ca.r + cb.r), 0.5 * (ca.g + cb.g),
                        0.5 * (ca.b + cb.b), 0.5 * (ca.a + cb.a));
                };
                const size_t count = pts.size();
                for (size_t i = 0; i < count; ++i) {
                    const size_t j = (i + 1) % count;
                    const RGBAColorData c = segmentColor(ptVertexIndex[i], ptVertexIndex[j]);
                    WebDrawPrimitive seg;
                    seg.kind = "line";
                    seg.x1 = pts[i].first;
                    seg.y1 = pts[i].second;
                    seg.x2 = pts[j].first;
                    seg.y2 = pts[j].second;
                    seg.color = colorString({ c.r, c.g, c.b, c.a });
                    seg.width = options.lineWidth;
                    seg.depth = depth;
                    seg.webglFallback = webglPatch;
                    (m_inDirect ? m_prims : m_pending3d).push_back(std::move(seg));
                }
            } else if (wantEdge && !wantFill) {
                WebDrawPrimitive p;
                p.kind = "polyline";
                p.points = pts;
                p.points.push_back(pts.front());
                p.color = colorString({ edge.r, edge.g, edge.b, edge.a });
                p.width = options.lineWidth;
                p.depth = depth;
                p.webglFallback = webglPatch;
                (m_inDirect ? m_prims : m_pending3d).push_back(std::move(p));
            }
            pts.clear();
            ptVertexIndex.clear();
        };
        for (size_t vi = 0; vi < face.vertices.size(); ++vi) {
            const point& v = face.vertices[vi];
            if (std::isnan(v.x) || std::isnan(v.y) || std::isnan(v.z)) {
                emitSubPath();
                depthSum = 0;
                depthCount = 0;
                continue;
            }
            double px, py;
            depthSum += mapXYZ(v.x, v.y, v.z, px, py);
            depthCount++;
            pts.emplace_back(px, py);
            ptVertexIndex.push_back(vi);
        }
        emitSubPath();
    }
}
//=============================================================================
void
RenderWeb::flush3d()
{
    if (m_pending3d.empty()) {
        return;
    }
    // Painter's algorithm, farthest first. stable_sort keeps call order for equal
    // depths, so flat 2D plots (everything at z=0) come out in drawing order.
    //
    // Depth is the mean clip-space z of the vertices. Coplanar primitives with
    // different vertex counts differ by ~1e-16 rounding noise, which ordered them by
    // noise instead of call order (the polar-axes background sorted over the grid).
    // The key is quantized to a grain coarser than the noise and finer than any real
    // 3D layer separation.
    auto depthKey = [](double d) -> long long { return std::llround(d * 1.0e9); };
    // A data stroke lying exactly on a filled surface shares the surface's mean
    // depth, and a neighbouring fill polygon can paint over it and fragment a mesh
    // line into dashes. Bias such strokes a hair toward the camera, far below the
    // depth gap across a surface so occluded back strokes stay hidden. Grid and box
    // "segments" are excluded: they sit behind the data.
    constexpr double strokeDepthBias = 3.0e-3;
    auto sortDepth = [&](const WebDrawPrimitive& p) -> double {
        // A solidEdge stroke outlines an opaque 3D solid (histogram2 bar) and must be
        // occluded by nearer faces: a bias would let a far bar's edge show through a
        // nearer bar's front face.
        if (!p.solidEdge && (p.kind == "polyline" || p.kind == "line")) {
            return p.depth - strokeDepthBias;
        }
        return p.depth;
    };
    std::stable_sort(m_pending3d.begin(), m_pending3d.end(), [&](const auto& a, const auto& b) {
        return depthKey(sortDepth(a)) > depthKey(sortDepth(b));
    });
    for (auto& p : m_pending3d) {
        m_prims.push_back(std::move(p));
    }
    m_pending3d.clear();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
