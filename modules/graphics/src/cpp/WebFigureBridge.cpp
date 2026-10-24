//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
//
// extern "C" bridge so nelson-webview (modules/web_gui) can enumerate and render
// figures WITHOUT linking Qt. nlsWeb_gui loads these dynamically. They MUST be
// invoked on the GUI/interpreter thread; nlsWeb_gui marshals the call through
// its interpreter-thread task queue, so grabFrameImage() renders inline (its
// own callGuiSync would deadlock in web mode, where the main thread does not
// spin a Qt event loop).
//=============================================================================
#include "GOFiguresManager.hpp"
#include "GOFigureHost.hpp"
#include "GOFigure.hpp"
#include "GOAxis.hpp"
#include "GOUIHTML.h"
#include "GOUIControl.h"
#include "GOUIComponent.h"
#include "GOGObjectsProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOMenu.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "RenderWeb.hpp"
#include "WebFigureAnimation.hpp"
#include "WebFigureBinary.hpp"
#include "WebFigureBridgeExports.hpp"
#include "WebFigureRenderMutex.hpp"
#include "WebFigureAxesMetadata.hpp"
#include "WebFigureRaster.hpp"
#include "Types.hpp"
#include "characters_encoding.hpp"
#include "nlsGraphics_exports.h"
#include <nlohmann/json.hpp>
#include <chrono>
#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <unordered_map>
#include <cstdlib>
#include <fstream>
#include <string>
#include <vector>
//=============================================================================
static constexpr size_t WEB_DISPLAY_LIST_RASTER_THRESHOLD = 2000;
//=============================================================================
namespace Nelson {
std::recursive_mutex&
webFigureRenderMutex()
{
    static auto* mutex = new std::recursive_mutex;
    return *mutex;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
static nlohmann::json
buildMenuChildren(Nelson::GraphicsObject* parent);
//=============================================================================
using WebProfileClock = std::chrono::steady_clock;
//=============================================================================
static double
elapsedMilliseconds(WebProfileClock::time_point start, WebProfileClock::time_point stop)
{
    return std::chrono::duration<double, std::milli>(stop - start).count();
}
//=============================================================================
static std::string
webProfilePath()
{
#ifdef _MSC_VER
    char* path = nullptr;
    size_t pathLength = 0;
    if (_dupenv_s(&path, &pathLength, "NELSON_WEB_PROFILE_DISPLAYLIST") != 0 || path == nullptr) {
        return {};
    }
    std::string result(path);
    free(path);
    return result;
#else
    const char* path = std::getenv("NELSON_WEB_PROFILE_DISPLAYLIST");
    return path == nullptr ? std::string() : std::string(path);
#endif
}
//=============================================================================
static bool
figureHasVisibleAxes(Nelson::GOFigure* fig)
{
    if (fig == nullptr) {
        return false;
    }
    auto* childProperty = dynamic_cast<Nelson::GOGObjectsProperty*>(
        fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (childProperty == nullptr) {
        return false;
    }
    for (int64_t handle : childProperty->data()) {
        auto* axis = dynamic_cast<Nelson::GOAxis*>(Nelson::findGraphicsObject(handle, false));
        if (axis != nullptr
            && axis->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static void
appendWebProfileLine(const std::string& path, const std::string& line)
{
    if (path.empty()) {
        return;
    }
    std::ofstream out(path, std::ios::binary | std::ios::app);
    if (out.is_open()) {
        out << line << '\n';
    }
}
//=============================================================================
static void
encodeIndexPayload(std::string& payload, std::string& type, const std::vector<uint32_t>& indices)
{
    bool fitsUint16 = true;
    for (uint32_t index : indices) {
        if (index > 65535u) {
            fitsUint16 = false;
            break;
        }
    }
    if (fitsUint16) {
        std::vector<uint16_t> compact;
        compact.reserve(indices.size());
        for (uint32_t index : indices) {
            compact.push_back(static_cast<uint16_t>(index));
        }
        payload = Nelson::webFigureBase64Encode(compact.data(), compact.size() * sizeof(uint16_t));
        type = "uint16";
        return;
    }
    payload = Nelson::webFigureBase64Encode(indices.data(), indices.size() * sizeof(uint32_t));
    type = "uint32";
}
//=============================================================================
// Recursively describe a uimenu subtree {handle,text,separator,checked,
// disabled,hasCallback,children} for the web menu bar / context menus.
static nlohmann::json
buildMenuJson(Nelson::GraphicsObject* menu)
{
    nlohmann::json node;
    node["handle"] = static_cast<long long>(Nelson::findGraphicsObjectHandle(menu));
    node["text"] = Nelson::wstring_to_utf8(menu->findStringProperty(GO_TEXT_PROPERTY_NAME_STR));
    if (menu->stringCheck(GO_SEPARATOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        node["separator"] = true;
    }
    if (menu->stringCheck(GO_CHECKED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        node["checked"] = true;
    }
    if (!menu->stringCheck(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        node["disabled"] = true;
    }
    auto* callback = dynamic_cast<Nelson::GOCallbackProperty*>(
        menu->findProperty(GO_MENU_SELECTED_FCN_PROPERTY_NAME_STR, false));
    if (callback != nullptr && !callback->get().isEmpty()) {
        node["hasCallback"] = true;
    }
    nlohmann::json children = buildMenuChildren(menu);
    if (!children.empty()) {
        node["children"] = children;
    }
    return node;
}
//=============================================================================
// The uimenu children of `parent` (a figure, a uimenu, or a uicontextmenu), in
// creation order (they are stored newest-first).
static nlohmann::json
buildMenuChildren(Nelson::GraphicsObject* parent)
{
    nlohmann::json children = nlohmann::json::array();
    auto* childProperty = dynamic_cast<Nelson::GOGObjectsProperty*>(
        parent->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (childProperty == nullptr) {
        return children;
    }
    std::vector<Nelson::int64> handles = childProperty->data();
    for (auto it = handles.rbegin(); it != handles.rend(); ++it) {
        Nelson::GraphicsObject* child = Nelson::findGraphicsObject(*it, false);
        if (child != nullptr && child->getType() == GO_PROPERTY_VALUE_UIMENU_STR) {
            children.push_back(buildMenuJson(child));
        }
    }
    return children;
}
//=============================================================================
// The context menu (its uimenu items) attached to `object` via its ContextMenu
// property, or an empty array if none.
static nlohmann::json
buildContextMenuJson(Nelson::GraphicsObject* object)
{
    auto* contextProperty = dynamic_cast<Nelson::GOGObjectsProperty*>(
        object->findProperty(GO_CONTEXT_MENU_PROPERTY_NAME_STR, false));
    if (contextProperty == nullptr || contextProperty->data().empty()) {
        return nlohmann::json::array();
    }
    Nelson::GraphicsObject* contextMenu
        = Nelson::findGraphicsObject(contextProperty->data()[0], false);
    if (contextMenu == nullptr || contextMenu->getType() != GO_PROPERTY_VALUE_UICONTEXTMENU_STR) {
        return nlohmann::json::array();
    }
    return buildMenuChildren(contextMenu);
}
//=============================================================================
// Internal binary-encoding helpers. These have C++ linkage on purpose (they
// return std::string) and must stay OUT of the extern "C" block below.
namespace {
//=============================================================================
// Encode a vertex list (polyline, polygon, segments) as a base64 little-endian float32 blob
// (x0,y0,x1,y1,...). The binary form is 2-3x smaller than a JSON [[x,y],...] array and cheaper to
// produce and parse.
std::string
encodePointPairsBase64(const std::vector<std::pair<double, double>>& points)
{
    std::string buffer(points.size() * 8, '\0');
    for (size_t i = 0; i < points.size(); i++) {
        const float x = static_cast<float>(points[i].first);
        const float y = static_cast<float>(points[i].second);
        std::memcpy(buffer.data() + i * 8, &x, 4);
        std::memcpy(buffer.data() + i * 8 + 4, &y, 4);
    }
    return Nelson::webFigureBase64Encode(buffer.data(), buffer.size());
}
//=============================================================================
// IEEE 754 float32 -> float16 (half), round-to-nearest, with inf/nan/subnormal
// handling. Used to halve the WebGL mesh vertex-position payload; positions are
// normalized into [0,1] first so half's ~2^-11 relative precision applies to
// the box extent and no value can overflow half's ~65504 range.
uint16_t
floatToHalf(float value)
{
    uint32_t x;
    std::memcpy(&x, &value, 4);
    const uint32_t sign = (x >> 16) & 0x8000u;
    const uint32_t expBits = (x >> 23) & 0xFFu;
    const uint32_t mant = x & 0x7FFFFFu;
    if (expBits == 0xFFu) { // inf / nan
        return static_cast<uint16_t>(sign | 0x7C00u | (mant ? 0x200u : 0u));
    }
    const int32_t exp = static_cast<int32_t>(expBits) - 127 + 15;
    if (exp >= 0x1F) { // overflow -> inf
        return static_cast<uint16_t>(sign | 0x7C00u);
    }
    if (exp <= 0) { // subnormal / underflow to zero
        if (exp < -10) {
            return static_cast<uint16_t>(sign);
        }
        uint32_t m = (mant | 0x800000u) >> (1 - exp);
        if (m & 0x1000u) {
            m += 0x2000u; // round to nearest
        }
        return static_cast<uint16_t>(sign | (m >> 13));
    }
    uint16_t half = static_cast<uint16_t>(sign | (static_cast<uint32_t>(exp) << 10) | (mant >> 13));
    if (mant & 0x1000u) {
        half = static_cast<uint16_t>(half + 1); // round to nearest
    }
    return half;
}
//=============================================================================
// Encode WebGL mesh positions (xyz triplets) as base64 float16, normalized
// per-axis into [0,1] using the bounding box returned in `box` (minx,miny,minz,
// maxx,maxy,maxz). The client restores full-range float32 coordinates from the
// box, so the GPU pipeline is unchanged; only the wire payload halves.
std::string
encodeMeshPositionsHalf(const std::vector<float>& positions, std::array<double, 6>& box)
{
    double mn[3] = { 1e300, 1e300, 1e300 };
    double mx[3] = { -1e300, -1e300, -1e300 };
    const size_t n = positions.size() / 3;
    for (size_t i = 0; i < n; i++) {
        for (int a = 0; a < 3; a++) {
            const double v = positions[i * 3 + a];
            mn[a] = std::min(mn[a], v);
            mx[a] = std::max(mx[a], v);
        }
    }
    for (int a = 0; a < 3; a++) {
        if (n == 0) {
            mn[a] = 0.0;
            mx[a] = 0.0;
        }
        box[a] = mn[a];
        box[3 + a] = mx[a];
    }
    std::string buffer(positions.size() * 2, '\0');
    for (size_t i = 0; i < n; i++) {
        for (int a = 0; a < 3; a++) {
            const double ext = mx[a] - mn[a];
            const float norm
                = ext > 0.0 ? static_cast<float>((positions[i * 3 + a] - mn[a]) / ext) : 0.0f;
            const uint16_t h = floatToHalf(norm);
            std::memcpy(buffer.data() + (i * 3 + a) * 2, &h, 2);
        }
    }
    return Nelson::webFigureBase64Encode(buffer.data(), buffer.size());
}
//=============================================================================
// Encode WebGL mesh vertex colours (RGBA in [0,1]) as base64 uint8. Colours are
// 8-bit on screen, so this is lossless for display and a quarter of the float32
// payload.
std::string
encodeMeshColorsUint8(const std::vector<float>& colors)
{
    std::string buffer(colors.size(), '\0');
    for (size_t i = 0; i < colors.size(); i++) {
        double v = colors[i];
        v = v < 0.0 ? 0.0 : (v > 1.0 ? 1.0 : v);
        buffer[i] = static_cast<char>(static_cast<unsigned char>(std::lround(v * 255.0)));
    }
    return Nelson::webFigureBase64Encode(buffer.data(), buffer.size());
}
//=============================================================================
// Cache of encoded WebGL mesh payloads, keyed by a content hash of the raw geometry buffers. A
// cosmetic edit leaves the geometry identical, so the float16/base64 re-encoding (hundreds of ms
// for a dense surface) is skipped. Thread-local; the display-list build runs under
// webFigureRenderMutex().
struct CachedEncodedMesh
{
    std::string positionsF16;
    std::array<double, 6> positionBox {};
    bool drawFaces = false;
    std::string vertexColorsU8;
    std::string triangleIndicesBase64;
    std::string triangleIndexType;
    std::string lineIndicesBase64;
    std::string lineIndexType;
    uint64_t lastUse = 0;
};
//=============================================================================
static uint64_t
fnv1aBytes(const void* data, size_t byteCount, uint64_t hash)
{
    const auto* bytes = static_cast<const uint8_t*>(data);
    for (size_t i = 0; i < byteCount; i++) {
        hash ^= bytes[i];
        hash *= 1099511628211ull;
    }
    return hash;
}
//=============================================================================
static uint64_t
meshGeometryHash(const Nelson::WebGlMesh& mesh)
{
    uint64_t hash = 1469598103934665603ull;
    hash = fnv1aBytes(mesh.positions.data(), mesh.positions.size() * sizeof(float), hash);
    hash = fnv1aBytes(mesh.vertexColors.data(), mesh.vertexColors.size() * sizeof(float), hash);
    hash = fnv1aBytes(
        mesh.triangleIndices.data(), mesh.triangleIndices.size() * sizeof(uint32_t), hash);
    hash = fnv1aBytes(mesh.lineIndices.data(), mesh.lineIndices.size() * sizeof(uint32_t), hash);
    hash ^= mesh.drawFaces ? 0x9E3779B97F4A7C15ull : 0ull;
    return hash;
}
//=============================================================================
static const CachedEncodedMesh&
encodeMeshCached(const Nelson::WebGlMesh& mesh)
{
    static thread_local std::unordered_map<uint64_t, CachedEncodedMesh> cache;
    static thread_local uint64_t clock = 0;
    const uint64_t key = meshGeometryHash(mesh);
    const uint64_t use = ++clock;
    auto found = cache.find(key);
    if (found != cache.end()) {
        found->second.lastUse = use;
        return found->second;
    }
    constexpr size_t MAX_ENTRIES = 64;
    if (cache.size() >= MAX_ENTRIES) {
        auto oldest = cache.begin();
        for (auto it = cache.begin(); it != cache.end(); ++it) {
            if (it->second.lastUse < oldest->second.lastUse) {
                oldest = it;
            }
        }
        cache.erase(oldest);
    }
    CachedEncodedMesh encoded;
    encoded.positionsF16 = encodeMeshPositionsHalf(mesh.positions, encoded.positionBox);
    encoded.drawFaces = mesh.drawFaces;
    if (mesh.drawFaces) {
        encoded.vertexColorsU8 = encodeMeshColorsUint8(mesh.vertexColors);
    }
    encodeIndexPayload(
        encoded.triangleIndicesBase64, encoded.triangleIndexType, mesh.triangleIndices);
    encodeIndexPayload(encoded.lineIndicesBase64, encoded.lineIndexType, mesh.lineIndices);
    encoded.lastUse = use;
    return cache.emplace(key, std::move(encoded)).first->second;
}
//=============================================================================
} // namespace
//=============================================================================
extern "C"
{
    //=============================================================================
    // Close a figure from the web desktop. The figure close-request callback decides whether
    // destruction continues.
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiCloseFigure(int64_t handle)
    {
        return Nelson::closeFigure(static_cast<Nelson::int64>(handle), true);
    }
    //=============================================================================
    // Reset the current figure to "none" so the next plotting command (in the
    // next Notebook cell) creates its own figure instead of drawing into the
    // previous cell's kept-alive figure.
    NLSGRAPHICS_IMPEXP void
    nlsWebGuiClearCurrentFigure()
    {
        Nelson::notifyCurrentFigureChanged(NO_FIGURE);
    }
    //=============================================================================
    NLSGRAPHICS_IMPEXP int
    nlsWebGuiListFigures(int64_t* out, int maxCount)
    {
        if (out == nullptr || maxCount <= 0) {
            return 0;
        }
        std::vector<Nelson::int64> handles = Nelson::getFigureGraphicsObjects();
        int n = 0;
        for (Nelson::int64 h : handles) {
            if (n >= maxCount) {
                break;
            }
            out[n++] = static_cast<int64_t>(h);
        }
        return n;
    }
    //=============================================================================
    // The figure's WindowStyle as an int enum (0 normal, 1 modal, 2 docked, 3 alwaysontop; -1 if
    // the figure is gone). The web desktop uses it to pick a native window or a docked panel.
    NLSGRAPHICS_IMPEXP int
    nlsWebGuiGetFigureWindowStyle(int64_t handle)
    {
        Nelson::GOFigureHost* win = Nelson::getFigure(static_cast<Nelson::int64>(handle));
        if (win == nullptr) {
            return -1;
        }
        Nelson::GOFigure* fig = win->hostFigure();
        if (fig == nullptr) {
            return -1;
        }
        const std::wstring style = fig->findStringProperty(GO_WINDOW_STYLE_PROPERTY_NAME_STR);
        if (style == GO_PROPERTY_VALUE_DOCKED_STR) {
            return 2;
        }
        if (style == GO_PROPERTY_VALUE_MODAL_STR) {
            return 1;
        }
        if (style == GO_PROPERTY_VALUE_ALWAYS_ON_TOP_STR) {
            return 3;
        }
        return 0;
    }
    //=============================================================================
    // The figure's on-screen size (Position width/height, pixels) so a separate
    // native window can open at the figure's own size instead of a fixed default.
    // Returns false (and leaves out* untouched) if the figure or its size is gone.
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiGetFigureSize(int64_t handle, int* outWidth, int* outHeight)
    {
        Nelson::GOFigureHost* win = Nelson::getFigure(static_cast<Nelson::int64>(handle));
        if (win == nullptr) {
            return false;
        }
        Nelson::GOFigure* fig = win->hostFigure();
        if (fig == nullptr) {
            return false;
        }
        std::vector<double> position = fig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (position.size() < 4) {
            return false;
        }
        const int w = static_cast<int>(position[2]);
        const int h = static_cast<int>(position[3]);
        if (w <= 0 || h <= 0) {
            return false;
        }
        if (outWidth != nullptr) {
            *outWidth = w;
        }
        if (outHeight != nullptr) {
            *outHeight = h;
        }
        return true;
    }
    //=============================================================================
    // The figure's display title (honoring NumberTitle / Name), copied into out
    // (UTF-8, null-terminated). Returns the length written.
    NLSGRAPHICS_IMPEXP int
    nlsWebGuiGetFigureTitle(int64_t handle, char* out, int maxLen)
    {
        if (out == nullptr || maxLen <= 0) {
            return 0;
        }
        const std::string title = Nelson::wstring_to_utf8(
            Nelson::computeFigureTitle(static_cast<Nelson::int64>(handle)));
        int n = static_cast<int>(title.size());
        if (n >= maxLen) {
            n = maxLen - 1;
        }
        title.copy(out, static_cast<size_t>(n));
        out[n] = '\0';
        return n;
    }
    //=============================================================================
    // Render figure `handle` into a RenderWeb (a RenderInterface backend that records 2D draw
    // primitives) and hand the JSON {width,height,primitives} to sink.
    static bool
    renderFigureDisplayList(int64_t handle, void (*sink)(void*, const char*, int), void* ctx,
        bool softwareFallbackEnabled, bool compactAnimationEnabled, bool interactiveViewOnly,
        void (*attachmentSink)(void*, const char*, const unsigned char*, int) = nullptr,
        void* attachmentCtx = nullptr)
    {
        const std::lock_guard<std::recursive_mutex> lock(Nelson::webFigureRenderMutex());
        Nelson::GOFigureHost* win = Nelson::getFigure(static_cast<Nelson::int64>(handle));
        if (win == nullptr) {
            return false;
        }
        Nelson::GOFigure* fig = win->hostFigure();
        if (fig == nullptr) {
            return false;
        }
        // Render size comes from the GOFigure itself (Qt-free); fall back to the Qt
        // render widget only if the figure has no size yet.
        int rw = fig->getWidth();
        int rh = fig->getHeight();
        if (rw <= 0 || rh <= 0) {
            // headless hosts have no Qt render widget: fall back to the default
            // figure size until the client pushes one.
            std::vector<double> defaultPosition = Nelson::defaultFigurePosition();
            if (defaultPosition.size() >= 4) {
                rw = static_cast<int>(defaultPosition[2]);
                rh = static_cast<int>(defaultPosition[3]);
            }
        }
        if (rw <= 0 || rh <= 0) {
            return false;
        }
        // Matrix-only animation frames are safe only when no visible axes need
        // their view-dependent labels, ticks and grid repainted.  In particular,
        // drawnow after camzoom/view can leave a visible axes in a transient
        // handle-array state while the compact renderer is assembling a frame.
        // Keep the compact path for hidden-axis WebGL animations, and use the
        // regular display list for visible axes.
        if (compactAnimationEnabled && figureHasVisibleAxes(fig)) {
            compactAnimationEnabled = false;
        }
        const std::string profilePath = webProfilePath();
        const auto profileStart = WebProfileClock::now();
        if (compactAnimationEnabled && !softwareFallbackEnabled
            && Nelson::tryRenderWebFigureViewOnlyFrame(
                handle, fig, rw, rh, sink, ctx, interactiveViewOnly)) {
            return true;
        }

        Nelson::RenderWeb renderer(0, 0, rw, rh, softwareFallbackEnabled, false);
        // A full (non-animation) display list is fetched statelessly by
        // figure.getDisplayList: the client that receives it may never have seen
        // the earlier frame that first defined a raw image token (fresh page load,
        // resize re-fetch), so the pixels must travel with every such list.
        // Streaming animation frames keep the dedup: their client is persistent.
        renderer.setAlwaysIncludeRawImagePayload(!compactAnimationEnabled);
        fig->paintMe(renderer);
        renderer.flush3d(); // emit any 3D primitives still buffered after the paint
        const auto profilePaintDone = WebProfileClock::now();

        nlohmann::json prims = nlohmann::json::array();
        const std::vector<Nelson::WebDrawPrimitive>& primitiveList = renderer.primitives();
        const bool hasWebGlMeshes = !renderer.webglMeshes().empty();
        const bool needsDisplaySignature = !compactAnimationEnabled || hasWebGlMeshes;
        const uint64_t displaySignature
            = needsDisplaySignature ? renderer.displaySignature(!compactAnimationEnabled) : 0;
        const uint64_t layoutSignature = renderer.layoutSignature();
        const bool tracksAnimationState = compactAnimationEnabled || hasWebGlMeshes;
        Nelson::WebFigureAnimationFrameState animationFrame;
        if (tracksAnimationState) {
            animationFrame = Nelson::beginWebFigureAnimationFrame(handle, fig, rw, rh,
                displaySignature, layoutSignature, renderer.webglMeshes(), softwareFallbackEnabled);
        }
        const bool matrixOnly = compactAnimationEnabled && animationFrame.matrixOnly;
        // Keep interactive controls in streamed frames. A script waiting on a button needs the
        // browser to show it, and a compact frame without uicontrols leaves the user unable to
        // continue the script. Controls are small compared with image and mesh payloads.
        const bool hasInteractiveControls = !renderer.controls().empty();
        const bool reuseStableLayout
            = compactAnimationEnabled && !animationFrame.refreshLayout && !hasInteractiveControls;
        const bool rasterFrame
            = !hasWebGlMeshes && primitiveList.size() > WEB_DISPLAY_LIST_RASTER_THRESHOLD;
        if (!matrixOnly && rasterFrame) {
            std::string base64;
            if (!Nelson::rasterizeWebFigureBase(primitiveList, rw, rh, base64)) {
                return false;
            }
            prims.push_back({ { "kind", "image" }, { "x", 0 }, { "y", 0 }, { "w", rw }, { "h", rh },
                { "image", "data:image/png;base64," + base64 } });
        }
        if (!matrixOnly) {
            for (const Nelson::WebDrawPrimitive& p : primitiveList) {
                if (rasterFrame && !Nelson::isWebFigureAnnotation(p)) {
                    continue;
                }
                if (hasWebGlMeshes && p.webglFallback) {
                    continue;
                }
                if (p.kind == "rect") {
                    prims.push_back({ { "kind", "rect" }, { "x", p.x }, { "y", p.y }, { "w", p.w },
                        { "h", p.h }, { "fill", p.color } });
                } else if (p.kind == "line") {
                    nlohmann::json seg = { { "kind", "line" }, { "x1", p.x1 }, { "y1", p.y1 },
                        { "x2", p.x2 }, { "y2", p.y2 }, { "stroke", p.color },
                        { "width", std::max(1.0, p.width) } };
                    if (!p.dash.empty()) {
                        seg["dash"] = p.dash;
                    }
                    prims.push_back(seg);
                } else if (p.kind == "segments") {
                    nlohmann::json segs = { { "kind", "segments" },
                        { "count", static_cast<uint64_t>(p.points.size()) },
                        { "pointsB64", encodePointPairsBase64(p.points) }, { "stroke", p.color },
                        { "width", std::max(1.0, p.width) } };
                    if (!p.dash.empty()) {
                        segs["dash"] = p.dash;
                    }
                    prims.push_back(std::move(segs));
                } else if (p.kind == "polyline") {
                    nlohmann::json poly = { { "kind", "polyline" },
                        { "count", static_cast<uint64_t>(p.points.size()) },
                        { "pointsB64", encodePointPairsBase64(p.points) }, { "stroke", p.color },
                        { "width", std::max(1.0, p.width) } };
                    if (!p.dash.empty()) {
                        poly["dash"] = p.dash;
                    }
                    prims.push_back(std::move(poly));
                } else if (p.kind == "polygon") {
                    nlohmann::json poly = { { "kind", "polygon" },
                        { "count", static_cast<uint64_t>(p.points.size()) },
                        { "pointsB64", encodePointPairsBase64(p.points) }, { "fill", p.color } };
                    if (!p.stroke.empty()) {
                        poly["stroke"] = p.stroke;
                        poly["width"] = std::max(1.0, p.width);
                    }
                    prims.push_back(std::move(poly));
                } else if (p.kind == "clip") {
                    prims.push_back({ { "kind", "clip" }, { "x", p.x }, { "y", p.y }, { "w", p.w },
                        { "h", p.h } });
                } else if (p.kind == "clipSave") {
                    prims.push_back({ { "kind", "clipSave" } });
                } else if (p.kind == "clipRestore") {
                    prims.push_back({ { "kind", "clipRestore" } });
                } else if (p.kind == "image") {
                    nlohmann::json imagePrim = { { "kind", "image" }, { "x", p.x }, { "y", p.y },
                        { "w", p.w }, { "h", p.h } };
                    if (!p.imageToken.empty()) {
                        imagePrim["imageId"] = p.imageToken;
                    }
                    if (!p.imageRgba.empty()) {
                        if (attachmentSink != nullptr) {
                            const std::string attachmentId = "rgba:" + std::to_string(handle) + ":"
                                + std::to_string(prims.size());
                            imagePrim["rgbaAttachment"] = attachmentId;
                            attachmentSink(attachmentCtx, attachmentId.c_str(), p.imageRgba.data(),
                                static_cast<int>(p.imageRgba.size()));
                        } else {
                            imagePrim["rgba"] = Nelson::webFigureBase64Encode(
                                p.imageRgba.data(), p.imageRgba.size());
                        }
                        imagePrim["imageWidth"] = p.imageWidth;
                        imagePrim["imageHeight"] = p.imageHeight;
                    } else if (p.imagePayload || p.imageToken.empty()) {
                        imagePrim["image"] = p.image;
                    }
                    prims.push_back(std::move(imagePrim));
                } else if (p.kind == "text") {
                    nlohmann::json textPrim = { { "kind", "text" }, { "x", p.x }, { "y", p.y },
                        { "text", p.text }, { "align", p.align }, { "color", p.color },
                        { "size", p.size }, { "rotation", p.rotation } };
                    if (p.fontFamily != "sans-serif") {
                        textPrim["font"] = p.fontFamily;
                    }
                    if (p.fontBold) {
                        textPrim["bold"] = true;
                    }
                    if (p.fontItalic) {
                        textPrim["italic"] = true;
                    }
                    prims.push_back(textPrim);
                } else if (p.kind == "points") {
                    // Marker batch: N markers of one shape packed as a little-endian blob
                    // (x,y,radius as float32, then fill RGBA and outline RGBA as four bytes each =
                    // 20 bytes per point), base64-encoded. One primitive replaces N per-marker
                    // polygons. hasFace / hasEdge say which colour blocks are meaningful.
                    const size_t pointCount = p.pointRadii.size();
                    const bool hasFace = p.pointColors.size() == pointCount * 4;
                    const bool hasEdge = p.pointEdgeColors.size() == pointCount * 4;
                    std::string buffer(pointCount * 20, '\0');
                    for (size_t i = 0; i < pointCount; i++) {
                        const float x = p.pointCenters[i * 2];
                        const float y = p.pointCenters[i * 2 + 1];
                        const float r = p.pointRadii[i];
                        char* out = buffer.data() + i * 20;
                        std::memcpy(out, &x, 4);
                        std::memcpy(out + 4, &y, 4);
                        std::memcpy(out + 8, &r, 4);
                        for (int k = 0; k < 4; k++) {
                            out[12 + k] = hasFace ? static_cast<char>(p.pointColors[i * 4 + k]) : 0;
                            out[16 + k]
                                = hasEdge ? static_cast<char>(p.pointEdgeColors[i * 4 + k]) : 0;
                        }
                    }
                    prims.push_back({ { "kind", "points" }, { "shape", p.pointShape },
                        { "hasFace", hasFace }, { "hasEdge", hasEdge },
                        { "edgeWidth", std::max(1.0, p.pointEdgeWidth) },
                        { "count", static_cast<uint64_t>(pointCount) },
                        { "data", Nelson::webFigureBase64Encode(buffer.data(), buffer.size()) } });
                } else if (p.kind == "rects") {
                    // Rectangle batch (bar/histogram): N axis-aligned rects packed
                    // as a little-endian binary blob (x,y,w,h as float32 = 16 bytes
                    // per rect), base64-encoded, with one shared fill/outline.
                    const size_t rectCount = p.rectXYWH.size() / 4;
                    std::string buffer(rectCount * 16, '\0');
                    for (size_t i = 0; i < rectCount; i++) {
                        std::memcpy(buffer.data() + i * 16, &p.rectXYWH[i * 4], 16);
                    }
                    nlohmann::json rectPrim = { { "kind", "rects" },
                        { "count", static_cast<uint64_t>(rectCount) },
                        { "data", Nelson::webFigureBase64Encode(buffer.data(), buffer.size()) } };
                    if (!p.color.empty()) {
                        rectPrim["fill"] = p.color;
                    }
                    if (!p.stroke.empty()) {
                        rectPrim["stroke"] = p.stroke;
                        rectPrim["width"] = std::max(1.0, p.width);
                    }
                    prims.push_back(std::move(rectPrim));
                }
            }
        }
        nlohmann::json controls = nlohmann::json::array();
        if (!matrixOnly && !reuseStableLayout) {
            for (const Nelson::WebUIControl& c : renderer.controls()) {
                nlohmann::json item = { { "handle", c.handle }, { "style", c.style },
                    { "text", c.text }, { "x", c.x }, { "y", c.y }, { "w", c.w }, { "h", c.h },
                    { "value", c.value }, { "min", c.minimum }, { "max", c.maximum },
                    { "enabled", c.enabled }, { "fontSize", c.fontSize },
                    { "hAlign", c.horizontalAlignment }, { "vAlign", c.verticalAlignment } };
                if (c.inactive) {
                    item["inactive"] = true;
                }
                if (!c.units.empty()) {
                    item["units"] = c.units;
                }
                if (!c.items.empty()) {
                    item["items"] = c.items;
                }
                if (!c.tooltip.empty()) {
                    item["tooltip"] = c.tooltip;
                }
                if (!c.background.empty()) {
                    item["background"] = c.background;
                }
                if (!c.foreground.empty()) {
                    item["foreground"] = c.foreground;
                }
                if (!c.extraJson.empty()) {
                    item["data"] = nlohmann::json::parse(c.extraJson, nullptr, false);
                }
                Nelson::GraphicsObject* controlObject
                    = Nelson::findGraphicsObject(static_cast<Nelson::int64>(c.handle), false);
                if (controlObject != nullptr) {
                    nlohmann::json contextMenu = buildContextMenuJson(controlObject);
                    if (!contextMenu.empty()) {
                        item["contextMenu"] = contextMenu;
                    }
                }
                controls.push_back(item);
            }
        }
        nlohmann::json webglMeshes = nlohmann::json::array();
        std::string softwareFallbackImage;
        if (hasWebGlMeshes) {
            for (size_t meshIndex = 0; meshIndex < renderer.webglMeshes().size(); ++meshIndex) {
                const Nelson::WebGlMesh& mesh = renderer.webglMeshes()[meshIndex];
                nlohmann::json webglMesh = { { "id", meshIndex },
                    { "revision", std::to_string(mesh.revision) }, { "model", mesh.model },
                    { "projection", mesh.projection }, { "viewport", mesh.viewport },
                    { "lineColor", mesh.lineColor }, { "lineWidth", mesh.lineWidth },
                    { "drawFaces", mesh.drawFaces }, { "positionsLength", mesh.positionsLength },
                    { "vertexColorsLength", mesh.vertexColorsLength },
                    { "triangleIndicesLength", mesh.triangleIndicesLength },
                    { "lineIndicesLength", mesh.lineIndicesLength } };
                const bool changed
                    = Nelson::webFigureAnimationMeshChanged(handle, meshIndex, mesh.revision);
                const bool geometryPayloadAvailable = mesh.positions.size() == mesh.positionsLength
                    && mesh.vertexColors.size() == mesh.vertexColorsLength
                    && mesh.triangleIndices.size() == mesh.triangleIndicesLength
                    && mesh.lineIndices.size() == mesh.lineIndicesLength;
                if (geometryPayloadAvailable
                    && (!compactAnimationEnabled || animationFrame.refreshGeometry || changed)) {
                    // Positions as normalized float16 (half the float32 payload) with the box to
                    // restore them, and vertex colours as uint8 (lossless for the 8-bit display).
                    // The client decodes both back to float32 for the GPU pipeline. The payloads
                    // are cached by geometry content, so a cosmetic edit reuses them.
                    const CachedEncodedMesh& encoded = encodeMeshCached(mesh);
                    webglMesh["positionsF16"] = encoded.positionsF16;
                    webglMesh["positionBBox"] = encoded.positionBox;
                    if (encoded.drawFaces) {
                        webglMesh["vertexColorsU8"] = encoded.vertexColorsU8;
                    }
                    webglMesh["triangleIndicesBase64"] = encoded.triangleIndicesBase64;
                    webglMesh["triangleIndexType"] = encoded.triangleIndexType;
                    webglMesh["lineIndicesBase64"] = encoded.lineIndicesBase64;
                    webglMesh["lineIndexType"] = encoded.lineIndexType;
                    if (mesh.triangleIndices.size() <= 64) {
                        webglMesh["triangleIndices"] = mesh.triangleIndices;
                    }
                }
                webglMeshes.push_back(std::move(webglMesh));
            }
            Nelson::endWebFigureAnimationFrame(
                handle, rw, rh, displaySignature, layoutSignature, renderer.webglMeshes());
            if (softwareFallbackEnabled) {
                std::string base64;
                if (Nelson::rasterizeWebPrimitivesToPngBase64(primitiveList, rw, rh, base64)) {
                    softwareFallbackImage = "data:image/png;base64," + base64;
                }
            }
        }
        if (compactAnimationEnabled && !hasWebGlMeshes) {
            Nelson::endWebFigureAnimationFrame(
                handle, rw, rh, displaySignature, layoutSignature, renderer.webglMeshes());
        }
        nlohmann::json result = { { "figureHandle", handle }, { "width", rw }, { "height", rh },
            { "primitives", prims } };
        if (!reuseStableLayout) {
            result["controls"] = std::move(controls);
        }
        if (matrixOnly) {
            result["matrixOnly"] = true;
        } else if (needsDisplaySignature) {
            result["displaySignature"] = std::to_string(displaySignature);
            result["layoutSignature"] = std::to_string(layoutSignature);
        } else {
            result["layoutSignature"] = std::to_string(layoutSignature);
        }
        if (!webglMeshes.empty()) {
            result["webglMeshes"] = std::move(webglMeshes);
            if (softwareFallbackEnabled) {
                result["softwareFallbackImage"] = std::move(softwareFallbackImage);
            }
        }
        if (matrixOnly) {
            const std::string dump = result.dump();
            if (sink != nullptr) {
                sink(ctx, dump.data(), static_cast<int>(dump.size()));
            }
            return true;
        }
        if (!reuseStableLayout) {
            std::vector<double> figurePosition
                = fig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
            if (figurePosition.size() >= 4) {
                result["controlLayoutWidth"] = figurePosition[2];
                result["controlLayoutHeight"] = figurePosition[3];
            }
        }

        if (!reuseStableLayout) {
            nlohmann::json menus = buildMenuChildren(fig);
            if (!menus.empty()) {
                result["menus"] = menus;
            }
            // Figure-level context menu (right-click the canvas).
            nlohmann::json figureContext = buildContextMenuJson(fig);
            if (!figureContext.empty()) {
                result["contextMenu"] = figureContext;
            }
        }

        // Include every subplot so 2D pan and 3D rotation share hit testing.
        Nelson::appendWebFigureAxesMetadata(fig, rw, rh, result);
        const auto profileJsonBuilt = WebProfileClock::now();

        const std::string dump = result.dump();
        const auto profileDumpDone = WebProfileClock::now();
        // Debug aid for comparing the web draw list against the Qt render: set
        // NELSON_WEB_DUMP_DISPLAYLIST to a file path to capture the last one.
#ifdef _MSC_VER
        char* dumpPath = nullptr;
        size_t dumpPathLen = 0;
        if (_dupenv_s(&dumpPath, &dumpPathLen, "NELSON_WEB_DUMP_DISPLAYLIST") == 0 && dumpPath) {
#else
        if (const char* dumpPath = std::getenv("NELSON_WEB_DUMP_DISPLAYLIST")) {
#endif
            // One file per figure handle: the client refreshes every open figure,
            // so a single shared path would only ever keep the last one.
            std::string dumpFile(dumpPath);
            const size_t dot = dumpFile.rfind('.');
            const std::string suffix = "." + std::to_string(handle);
            dumpFile = dot == std::string::npos
                ? dumpFile + suffix
                : dumpFile.substr(0, dot) + suffix + dumpFile.substr(dot);
            std::ofstream out(dumpFile, std::ios::binary | std::ios::trunc);
            if (out.is_open()) {
                out << result.dump(1);
            }
#ifdef _MSC_VER
            free(dumpPath);
#endif
        }
        if (sink != nullptr) {
            sink(ctx, dump.data(), static_cast<int>(dump.size()));
        }
        const auto profileSinkDone = WebProfileClock::now();
        if (!profilePath.empty()) {
            appendWebProfileLine(profilePath,
                "handle=" + std::to_string(handle) + " size=" + std::to_string(rw) + "x"
                    + std::to_string(rh) + " primitive_input="
                    + std::to_string(primitiveList.size()) + " primitive_json="
                    + std::to_string(prims.size()) + " controls=" + std::to_string(controls.size())
                    + " webgl=" + std::to_string(webglMeshes.size())
                    + " reuse_layout=" + std::to_string(reuseStableLayout ? 1 : 0)
                    + " matrix_only=" + std::to_string(matrixOnly ? 1 : 0)
                    + " bytes=" + std::to_string(dump.size()) + " paint_ms="
                    + std::to_string(elapsedMilliseconds(profileStart, profilePaintDone))
                    + " json_ms="
                    + std::to_string(elapsedMilliseconds(profilePaintDone, profileJsonBuilt))
                    + " dump_ms="
                    + std::to_string(elapsedMilliseconds(profileJsonBuilt, profileDumpDone))
                    + " sink_ms="
                    + std::to_string(elapsedMilliseconds(profileDumpDone, profileSinkDone))
                    + " total_ms="
                    + std::to_string(elapsedMilliseconds(profileStart, profileSinkDone)));
        }
        return true;
    }
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiRenderFigureDisplayList(
        int64_t handle, void (*sink)(void*, const char*, int), void* ctx)
    {
        return renderFigureDisplayList(handle, sink, ctx, true, false, false);
    }
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiRenderFigureDisplayListNoFallback(
        int64_t handle, void (*sink)(void*, const char*, int), void* ctx)
    {
        return renderFigureDisplayList(handle, sink, ctx, false, false, false);
    }
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiRenderFigureAnimationFrame(
        int64_t handle, void (*sink)(void*, const char*, int), void* ctx)
    {
        // Keep the retained-mesh fast path restricted to axes that are hidden.
        // Visible axes still need the regular display-list renderer: its labels,
        // ticks and grid are view-dependent and the projection-only path can
        // expose handle-array conversions while a figure is being refreshed.
        return renderFigureDisplayList(handle, sink, ctx, false, true, false);
    }
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiRenderFigureInteractiveFrame(
        int64_t handle, void (*sink)(void*, const char*, int), void* ctx)
    {
        return renderFigureDisplayList(handle, sink, ctx, false, true, false);
    }
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiRenderFigureAnimationFrameWithAttachments(int64_t handle,
        void (*sink)(void*, const char*, int), void* ctx,
        void (*attachmentSink)(void*, const char*, const unsigned char*, int), void* attachmentCtx)
    {
        return renderFigureDisplayList(
            handle, sink, ctx, false, true, false, attachmentSink, attachmentCtx);
    }
    //=============================================================================
    NLSGRAPHICS_IMPEXP void
    nlsWebGuiMarkFigureViewOnlyFrame(int64_t handle)
    {
        Nelson::markWebFigureViewOnlyFrame(handle);
    }
    //=============================================================================
    // Pan the 2D axes by a fraction of their current range. The web client sends the drag delta as
    // a fraction of the panel size (dxFrac, dyFrac). Screen y grows downward, so a downward drag
    // raises YLim. Switches the limit modes to manual, like interactive pan. Returns false if no
    // axes were found.
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiPanAxes(int64_t handle, double dxFrac, double dyFrac)
    {
        Nelson::GOFigureHost* win = Nelson::getFigure(static_cast<Nelson::int64>(handle));
        if (win == nullptr) {
            return false;
        }
        Nelson::GOFigure* fig = win->hostFigure();
        if (fig == nullptr) {
            return false;
        }
        auto* childProp = dynamic_cast<Nelson::GOGObjectsProperty*>(
            fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (childProp == nullptr) {
            return false;
        }
        bool done = false;
        for (Nelson::int64 h : childProp->data()) {
            auto* ax = dynamic_cast<Nelson::GOAxis*>(Nelson::findGraphicsObject(h, false));
            if (ax == nullptr) {
                continue;
            }
            std::vector<double> xlim = ax->findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR);
            std::vector<double> ylim = ax->findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR);
            if (xlim.size() < 2 || ylim.size() < 2) {
                continue;
            }
            const double dx = dxFrac * (xlim[1] - xlim[0]);
            const double dy = dyFrac * (ylim[1] - ylim[0]);
            ax->setRestrictedStringDefault(GO_X_LIM_MODE_PROPERTY_NAME_STR, L"manual");
            ax->setRestrictedStringDefault(GO_Y_LIM_MODE_PROPERTY_NAME_STR, L"manual");
            // Drag right (dxFrac > 0) reveals content to the left --> limits move left.
            ax->setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, xlim[0] - dx, xlim[1] - dx);
            // Drag down (dyFrac > 0) reveals content above --> limits move up.
            ax->setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, ylim[0] + dy, ylim[1] + dy);
            ax->updateState();
            done = true;
        }
        return done;
    }
    //=============================================================================
    // The user moved/resized a separate native figure window: write the new
    // geometry back onto the figure Position (pixels, bottom-origin, drawable
    // area), like the Qt desktop. No-op unless Units are pixels (the default), so
    // a normalized Position is never overwritten with pixel values.
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiSetFigurePosition(int64_t handle, int x, int y, int width, int height)
    {
        if (width <= 0 || height <= 0) {
            return false;
        }
        Nelson::GOFigureHost* win = Nelson::getFigure(static_cast<Nelson::int64>(handle));
        if (win == nullptr) {
            return false;
        }
        Nelson::GOFigure* fig = win->hostFigure();
        if (fig == nullptr) {
            return false;
        }
        if (!fig->stringCheck(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PIXELS_STR)) {
            return false;
        }
        return fig->applyWebWindowGeometry(x, y, width, height);
    }
    //=============================================================================
    // Hand the frontend the page state of one uihtml component: its Data as
    // JSON, the revision stamp it must not go back from, and any engine -> page
    // events queued since the last pull, which this call drains.
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiUiHtmlPull(int64_t handle, void (*sink)(void*, const char*, int), void* ctx)
    {
        Nelson::GraphicsObject* object
            = Nelson::findGraphicsObject(static_cast<Nelson::int64>(handle), false);
        auto* component = dynamic_cast<Nelson::GOUIHTML*>(object);
        if (component == nullptr || sink == nullptr) {
            return false;
        }
        // This is an extern "C" boundary: an escaping exception would reach the
        // server thread that called it and terminate the process. json::dump
        // throws on invalid UTF-8, and allocation can throw anywhere.
        try {
            nlohmann::json payload;
            payload["source"] = Nelson::wstring_to_utf8(
                component->findStringProperty(GO_HTML_SOURCE_PROPERTY_NAME_STR));
            payload["dataJson"] = component->publishedData();
            payload["revision"] = component->dataRevision();
            nlohmann::json events = nlohmann::json::array();
            for (const auto& queued : component->drainEventsToPage()) {
                events.push_back({ { "name", queued.first }, { "data", queued.second } });
            }
            payload["events"] = events;
            const std::string dump = payload.dump();
            sink(ctx, dump.data(), static_cast<int>(dump.size()));
            return true;
        } catch (...) {
            return false;
        }
    }
    //=============================================================================
    // A web uicontrol was interacted with: write the new state onto the graphics
    // object and run its callback, exactly as the Qt widget signal would.
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiUIControlAction(int64_t handle, double value, const char* text)
    {
        Nelson::GraphicsObject* object
            = Nelson::findGraphicsObject(static_cast<Nelson::int64>(handle), false);
        const std::string actionText = text != nullptr ? std::string(text) : std::string();
        if (auto* control = dynamic_cast<Nelson::GOUIControl*>(object)) {
            control->applyWebAction(value, actionText);
            return true;
        }
        if (auto* component = dynamic_cast<Nelson::GOUIComponent*>(object)) {
            return component->applyWebAction(value, actionText);
        }
        // uimenu / uicontextmenu items: enqueue MenuSelectedFcn like the Qt QAction
        // trigger does (executeMenuCallback in GOMenuQt).
        if (auto* menu = dynamic_cast<Nelson::GOMenu*>(object)) {
            auto* callback = dynamic_cast<Nelson::GOCallbackProperty*>(
                menu->findProperty(GO_MENU_SELECTED_FCN_PROPERTY_NAME_STR, false));
            if (callback != nullptr && !callback->get().isEmpty()) {
                Nelson::setCurrentObject(Nelson::findGraphicsObjectHandle(menu));
                callback->pushEvent(menu, L"ActionData", L"Action");
            }
            return true;
        }
        return false;
    }
    //=============================================================================
}
//=============================================================================
