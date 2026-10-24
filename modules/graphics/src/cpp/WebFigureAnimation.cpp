//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebFigureAnimation.hpp"
#include "WebFigureRenderMutex.hpp"
#include "RenderWeb.hpp"
#include "GOFigure.hpp"
#include "GOAxis.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include <array>
#include <charconv>
#include <chrono>
#include <cstdio>
#include <string>
#include <unordered_map>
//=============================================================================
namespace Nelson {
//=============================================================================
struct CachedAnimationMesh
{
    uint64_t revision = 0;
};
//=============================================================================
struct AnimationMeshState
{
    // Identity guard only, never dereferenced: the pointed-to figure may already
    // be destroyed. forgetWebFigureAnimation() is the real lifecycle hook.
    const GOFigure* figure = nullptr;
    uint64_t frame = 0;
    uint64_t displaySignature = 0;
    uint64_t layoutSignature = 0;
    int width = 0;
    int height = 0;
    bool viewOnlyPending = false;
    std::vector<uint64_t> revisions;
    std::vector<CachedAnimationMesh> meshes;
};
//=============================================================================
static std::unordered_map<int64_t, AnimationMeshState> animationMeshStates;
//=============================================================================
static GOAxis*
singleAxisForFastView(GOFigure* fig)
{
    auto* childProperty = dynamic_cast<GOGObjectsProperty*>(
        fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (childProperty == nullptr) {
        return nullptr;
    }
    GOAxis* axis = nullptr;
    for (int64 handle : childProperty->data()) {
        auto* current = dynamic_cast<GOAxis*>(findGraphicsObject(handle, false));
        if (current == nullptr) {
            continue;
        }
        if (axis != nullptr) {
            return nullptr;
        }
        axis = current;
    }
    // A matrix-only frame moves the WebGL meshes but leaves every 2D primitive
    // (axes box, grid, ticks, labels) exactly where it was, so it is only
    // correct when the axes draws nothing view-dependent, i.e. 'axis off'.
    // A visible axes must repaint fully so it rotates with the scene, exactly
    // as the Qt renderer does; the retained mesh geometry keeps that cheap.
    if (axis != nullptr
        && !axis->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return nullptr;
    }
    return axis;
}
//=============================================================================
static void
appendFloat(std::string& out, float value)
{
    char buffer[32];
    auto [ptr, ec]
        = std::to_chars(buffer, buffer + sizeof(buffer), value, std::chars_format::general, 9);
    if (ec == std::errc()) {
        out.append(buffer, ptr);
        return;
    }
    const int count = std::snprintf(buffer, sizeof(buffer), "%.9g", static_cast<double>(value));
    if (count > 0) {
        out.append(buffer, static_cast<size_t>(count));
    }
}
//=============================================================================
template <typename T, size_t N>
static void
appendArray(std::string& out, const std::array<T, N>& values)
{
    out += '[';
    for (size_t k = 0; k < values.size(); ++k) {
        if (k != 0) {
            out += ',';
        }
        appendFloat(out, static_cast<float>(values[k]));
    }
    out += ']';
}
//=============================================================================
static void
appendIntArray(std::string& out, const std::array<int, 4>& values)
{
    out += '[';
    for (size_t k = 0; k < values.size(); ++k) {
        if (k != 0) {
            out += ',';
        }
        out += std::to_string(values[k]);
    }
    out += ']';
}
//=============================================================================
static void
appendMatrixOnlyMeshJson(std::string& out, size_t meshIndex, const CachedAnimationMesh& cached,
    const WebGlMesh& transform)
{
    out += "{\"id\":";
    out += std::to_string(meshIndex);
    out += ",\"revision\":\"";
    out += std::to_string(cached.revision);
    out += "\",\"model\":";
    appendArray(out, transform.model);
    out += ",\"projection\":";
    appendArray(out, transform.projection);
    out += ",\"viewport\":";
    appendIntArray(out, transform.viewport);
    out += '}';
}
//=============================================================================
static std::string
buildMatrixOnlyFrameJson(int width, int height, const std::vector<CachedAnimationMesh>& meshes,
    const WebGlMesh& transform)
{
    std::string out;
    out.reserve(256 + meshes.size() * 512);
    out += "{\"width\":";
    out += std::to_string(width);
    out += ",\"height\":";
    out += std::to_string(height);
    out += ",\"primitives\":[],\"controls\":[],\"matrixOnly\":true,\"webglMeshes\":[";
    for (size_t meshIndex = 0; meshIndex < meshes.size(); ++meshIndex) {
        if (meshIndex != 0) {
            out += ',';
        }
        appendMatrixOnlyMeshJson(out, meshIndex, meshes[meshIndex], transform);
    }
    out += "]}";
    return out;
}
//=============================================================================
bool
tryRenderWebFigureViewOnlyFrame(int64_t handle, GOFigure* fig, int width, int height,
    void (*sink)(void*, const char*, int), void* ctx, bool allowVisibleAxis)
{
    auto it = animationMeshStates.find(handle);
    if (it == animationMeshStates.end() || !it->second.viewOnlyPending) {
        return false;
    }
    AnimationMeshState& state = it->second;
    state.viewOnlyPending = false;
    if (state.figure != fig || state.meshes.empty() || state.width != width
        || state.height != height) {
        return false;
    }
    GOAxis* axis = singleAxisForFastView(fig);
    if (allowVisibleAxis && axis == nullptr) {
        auto* childProperty = dynamic_cast<GOGObjectsProperty*>(
            fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (childProperty != nullptr) {
            for (int64 handle : childProperty->data()) {
                auto* current = dynamic_cast<GOAxis*>(findGraphicsObject(handle, false));
                if (current == nullptr) {
                    continue;
                }
                if (axis != nullptr) {
                    axis = nullptr;
                    break;
                }
                axis = current;
            }
        }
    }
    if (axis == nullptr) {
        return false;
    }

    RenderWeb renderer(0, 0, width, height, false);
    axis->setupProjectionOnly(renderer);
    WebGlMesh transform;
    renderer.currentWebGlTransform(transform);

    const std::string dump = buildMatrixOnlyFrameJson(width, height, state.meshes, transform);
    if (sink != nullptr) {
        sink(ctx, dump.data(), static_cast<int>(dump.size()));
    }
    return true;
}
//=============================================================================
WebFigureAnimationFrameState
beginWebFigureAnimationFrame(int64_t handle, const GOFigure* fig, int width, int height,
    uint64_t displaySignature, uint64_t layoutSignature, const std::vector<WebGlMesh>& meshes,
    bool softwareFallbackEnabled)
{
    WebFigureAnimationFrameState frameState;
    if (softwareFallbackEnabled) {
        return frameState;
    }
    AnimationMeshState& animationState = animationMeshStates[handle];
    if (animationState.figure != fig) {
        animationState = AnimationMeshState {};
        animationState.figure = fig;
    }
    ++animationState.frame;
    frameState.refreshLayout = animationState.frame == 1
        || animationState.layoutSignature != layoutSignature || animationState.width != width
        || animationState.height != height;
    if (meshes.empty()) {
        return frameState;
    }
    // Refresh geometry only when it actually changes (a mesh revision differs)
    // or on the first frame. A time-based periodic refresh would re-serialize
    // the whole mesh (megabytes) mid-rotation and stutter every interval, while
    // the client already caches decoded geometry by revision.
    bool sameRevisions = meshes.size() == animationState.revisions.size();
    for (size_t meshIndex = 0; sameRevisions && meshIndex < meshes.size(); ++meshIndex) {
        sameRevisions = animationState.revisions[meshIndex] == meshes[meshIndex].revision;
    }
    frameState.refreshGeometry = animationState.frame == 1 || !sameRevisions;
    frameState.matrixOnly = sameRevisions && animationState.displaySignature == displaySignature
        && animationState.width == width && animationState.height == height;
    return frameState;
}
//=============================================================================
bool
webFigureAnimationMeshChanged(int64_t handle, size_t meshIndex, uint64_t revision)
{
    auto it = animationMeshStates.find(handle);
    if (it == animationMeshStates.end() || meshIndex >= it->second.revisions.size()) {
        return true;
    }
    return it->second.revisions[meshIndex] != revision;
}
//=============================================================================
void
endWebFigureAnimationFrame(int64_t handle, int width, int height, uint64_t displaySignature,
    uint64_t layoutSignature, const std::vector<WebGlMesh>& meshes)
{
    AnimationMeshState& animationState = animationMeshStates[handle];
    animationState.revisions.resize(meshes.size());
    animationState.meshes.resize(meshes.size());
    for (size_t meshIndex = 0; meshIndex < meshes.size(); ++meshIndex) {
        const WebGlMesh& mesh = meshes[meshIndex];
        animationState.revisions[meshIndex] = mesh.revision;
        animationState.meshes[meshIndex].revision = mesh.revision;
    }
    animationState.displaySignature = displaySignature;
    animationState.layoutSignature = layoutSignature;
    animationState.width = width;
    animationState.height = height;
    animationState.viewOnlyPending = false;
}
//=============================================================================
void
markWebFigureViewOnlyFrame(int64_t handle)
{
    AnimationMeshState& state = animationMeshStates[handle];
    state.viewOnlyPending = true;
}
//=============================================================================
static std::unordered_map<int64_t, std::chrono::steady_clock::time_point> lastFramePushTimes;
//=============================================================================
void
forgetWebFigureAnimation(int64_t handle)
{
    // Called from the figure teardown path, which does not already hold the
    // render lock protecting animationMeshStates.
    const std::lock_guard<std::recursive_mutex> lock(webFigureRenderMutex());
    animationMeshStates.erase(handle);
    lastFramePushTimes.erase(handle);
}
//=============================================================================
void
noteWebFigureFramePushed(int64_t handle)
{
    const std::lock_guard<std::recursive_mutex> lock(webFigureRenderMutex());
    lastFramePushTimes[handle] = std::chrono::steady_clock::now();
}
//=============================================================================
bool
webFigureFramePushedWithin(int64_t handle, long long microseconds)
{
    const std::lock_guard<std::recursive_mutex> lock(webFigureRenderMutex());
    const auto found = lastFramePushTimes.find(handle);
    if (found == lastFramePushTimes.end()) {
        return false;
    }
    const auto elapsed = std::chrono::duration_cast<std::chrono::microseconds>(
        std::chrono::steady_clock::now() - found->second)
                             .count();
    return elapsed < microseconds;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
