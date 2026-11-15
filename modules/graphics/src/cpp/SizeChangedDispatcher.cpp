//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <chrono>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include "SizeChangedDispatcher.hpp"
#include "CallbackQueue.hpp"
#include "GOCallbackProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static int scopeDepth = 0;
static int deferralDepth = 0;
// What a tab group resized between two callback processings, in raise order.
struct DeferredBatch
{
    std::vector<int64> handles;
    // Page size each uifigure tab had when the batch was closed.
    std::vector<std::pair<int64, std::pair<double, double>>> tabPageSizes;
    bool delayStarted = false;
    std::chrono::steady_clock::time_point dueTime;
};
// The reference raises a batch once events have been processed for 20 to 45 ms after its resize:
// the delay starts at the first callback processing, and a later resize opens a batch of its own.
static const std::chrono::milliseconds DEFERRED_DELAY(20);
static std::vector<DeferredBatch> deferredBatches;
static bool deferredRunQueued = false;
// Handles raised inside a scope, in raise order.
static std::vector<int64> scopedHandles;
// Objects raised while their figure, themselves or a container holding them were invisible.
static std::unordered_set<GraphicsObject*> heldUntilShown;
// Position size of each figure at its last report.
static std::unordered_map<GraphicsObject*, std::pair<double, double>> reportedFigureSizes;
// Last Position origin of the tracked uifigure containers.
static std::unordered_map<GraphicsObject*, std::pair<double, double>> containerOrigins;
// Page size the uifigure tabs still report while a deferred batch is waiting.
static std::unordered_map<GraphicsObject*, std::pair<double, double>> reportedTabSizes;
// Last pixel size of the containers laid out without a widget (web figures).
static std::unordered_map<GraphicsObject*, std::pair<double, double>> containerPixelSizes;
//=============================================================================
static GOCallbackProperty*
activeSizeChangedCallback(GraphicsObject* go);
static GOFigure*
owningFigure(GraphicsObject* go);
static bool
isFigureVisible(GOFigure* fig);
static bool
isContainerVisible(GraphicsObject* go);
static GraphicsObject*
parentContainer(GraphicsObject* go);
static bool
isShownInFigure(GraphicsObject* go);
static int
containerDepth(GraphicsObject* go);
static void
collectHeld(GraphicsObject* go, int depth, std::vector<int64>& handles);
static void
releaseHeld(GraphicsObject* ancestor);
static void
runScopedHandles();
static void
deferSizeChanged(GraphicsObject* go);
static void
queueDeferredRun();
static DeferredBatch&
openDeferredBatch();
static void
deferTabPageSize(GraphicsObject* tab, double width, double height);
static void
reportTabPageSizes(const DeferredBatch& batch);
static void
runDeferredBatch(const std::vector<int64>& handles);
static int64
owningTabHandle(GraphicsObject* go);
static bool
recordFigureSize(GOFigure* fig);
//=============================================================================
SizeChangedScope::SizeChangedScope() { ++scopeDepth; }
//=============================================================================
SizeChangedScope::~SizeChangedScope()
{
    if (--scopeDepth == 0 && !scopedHandles.empty()) {
        try {
            runScopedHandles();
        } catch (...) {
        }
    }
}
//=============================================================================
SizeChangedDeferral::SizeChangedDeferral() { ++deferralDepth; }
//=============================================================================
SizeChangedDeferral::~SizeChangedDeferral() { --deferralDepth; }
//=============================================================================
void
notifySizeChanged(GraphicsObject* go)
{
    if (!go || !go->haveProperty(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR)) {
        return;
    }
    GOFigure* fig = owningFigure(go);
    if (fig && !isFigureVisible(fig)) {
        heldUntilShown.insert(go);
        return;
    }
    // A container hidden by its own Visible or by a container holding it stays silent until shown.
    if (go != fig && !isShownInFigure(go)) {
        heldUntilShown.insert(go);
        return;
    }
    GOCallbackProperty* callback = activeSizeChangedCallback(go);
    if (!callback) {
        return;
    }
    if (deferralDepth > 0) {
        deferSizeChanged(go);
        return;
    }
    if (scopeDepth == 0) {
        callback->pushEvent(go, L"SizeChangedData", L"SizeChanged");
        return;
    }
    const int64 handle = findGraphicsObjectHandle(go);
    if (handle != -1
        && std::find(scopedHandles.begin(), scopedHandles.end(), handle) == scopedHandles.end()) {
        scopedHandles.push_back(handle);
    }
}
//=============================================================================
void
notifyFigureSizeReported(GOFigure* fig, bool sizeChanged)
{
    if (!fig) {
        return;
    }
    if (!isFigureVisible(fig)) {
        heldUntilShown.insert(fig);
        return;
    }
    releaseHeld(fig);
    const bool figureHeld = heldUntilShown.erase(fig) > 0;
    // One Position change can resize the drawing area twice: report it once.
    const bool newSize = recordFigureSize(fig);
    if (figureHeld || (sizeChanged && newSize)) {
        notifySizeChanged(fig);
    }
}
//=============================================================================
void
notifyFigureShown(GOFigure* fig)
{
    if (!fig || !isFigureVisible(fig)) {
        return;
    }
    // Children first, the figure last.
    releaseHeld(fig);
    const bool figureHeld = heldUntilShown.erase(fig) > 0;
    const bool firstReport = reportedFigureSizes.find(fig) == reportedFigureSizes.end();
    recordFigureSize(fig);
    if (figureHeld || firstReport) {
        notifySizeChanged(fig);
    }
}
//=============================================================================
void
notifyContainerShown(GraphicsObject* go)
{
    if (!go || heldUntilShown.empty() || !isShownInFigure(go)) {
        return;
    }
    GOFigure* fig = owningFigure(go);
    if (!fig || fig == go || !isFigureVisible(fig)) {
        return;
    }
    releaseHeld(go);
}
//=============================================================================
void
notifyTabPageResized(GraphicsObject* tab, double width, double height)
{
    if (!tab || !tab->haveProperty(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR)) {
        return;
    }
    auto previous = containerPixelSizes.find(tab);
    const bool firstSize = previous == containerPixelSizes.end();
    const std::pair<double, double> previousSize
        = firstSize ? std::pair<double, double>(width, height) : previous->second;
    if (!recordContainerPixelSize(tab, width, height)) {
        return;
    }
    if (firstSize) {
        notifySizeChanged(tab);
        return;
    }
    GOFigure* fig = owningFigure(tab);
    if (fig && isUiFigure(fig)) {
        // The reference keeps the old Position of a uifigure tab until its deferred event.
        reportedTabSizes.emplace(tab, previousSize);
        deferTabPageSize(tab, width, height);
    }
    SizeChangedDeferral deferral;
    notifySizeChanged(tab);
}
//=============================================================================
bool
reportedTabPageSize(GraphicsObject* tab, double& width, double& height)
{
    auto it = reportedTabSizes.find(tab);
    if (it == reportedTabSizes.end()) {
        return false;
    }
    width = it->second.first;
    height = it->second.second;
    return true;
}
//=============================================================================
void
runDeferredSizeChanged()
{
    deferredRunQueued = false;
    const auto now = std::chrono::steady_clock::now();
    std::vector<DeferredBatch> due;
    std::vector<DeferredBatch> waiting;
    for (DeferredBatch& batch : deferredBatches) {
        if (!batch.delayStarted) {
            batch.delayStarted = true;
            batch.dueTime = now + DEFERRED_DELAY;
        }
        (now < batch.dueTime ? waiting : due).push_back(batch);
    }
    deferredBatches.swap(waiting);
    if (!deferredBatches.empty()) {
        // Too early for these: look again at the next callback processing.
        queueDeferredRun();
    }
    for (const DeferredBatch& batch : due) {
        reportTabPageSizes(batch);
        runDeferredBatch(batch.handles);
    }
}
//=============================================================================
bool
consumeContainerMove(GraphicsObject* go)
{
    if (!go || !go->haveProperty(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR)
        || !go->haveProperty(GO_POSITION_PROPERTY_NAME_STR)) {
        return false;
    }
    GOFigure* fig = owningFigure(go);
    if (!fig || fig == go || !isUiFigure(fig)) {
        return false;
    }
    // Panels and button groups report a move, a tab group does not.
    if (go->isType(GO_PROPERTY_VALUE_UITABGROUP_STR)) {
        return false;
    }
    std::vector<double> position = go->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (position.size() < 2) {
        return false;
    }
    const std::pair<double, double> origin(position[0], position[1]);
    auto it = containerOrigins.find(go);
    if (it == containerOrigins.end()) {
        containerOrigins.emplace(go, origin);
        return false;
    }
    if (it->second == origin) {
        return false;
    }
    it->second = origin;
    return true;
}
//=============================================================================
void
warnSizeChangedFcnDisabled(GraphicsObject* go)
{
    if (!go || !go->haveProperty(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR)
        || !go->haveProperty(GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR)) {
        return;
    }
    if (!go->hasChanged(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR)
        && !go->hasChanged(GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR)) {
        return;
    }
    auto* callback = dynamic_cast<GOCallbackProperty*>(
        go->findProperty(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR, false));
    if (!callback || callback->get().isEmpty()
        || !go->stringCheck(GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        return;
    }
    RaiseWarning(L"Nelson:graphics:sizeChangedFcnDisabled",
        _W("SizeChangedFcn is not called while AutoResizeChildren is 'on'."));
}
//=============================================================================
void
forgetSizeChangedState(GraphicsObject* go)
{
    heldUntilShown.erase(go);
    reportedFigureSizes.erase(go);
    containerOrigins.erase(go);
    containerPixelSizes.erase(go);
    reportedTabSizes.erase(go);
}
//=============================================================================
bool
isSizeChangedFcnActive(GraphicsObject* go)
{
    return go && go->haveProperty(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR)
        && activeSizeChangedCallback(go) != nullptr;
}
//=============================================================================
bool
recordContainerPixelSize(GraphicsObject* go, double width, double height)
{
    const std::pair<double, double> size(width, height);
    auto it = containerPixelSizes.find(go);
    if (it == containerPixelSizes.end()) {
        containerPixelSizes.emplace(go, size);
        return true;
    }
    if (it->second == size) {
        return false;
    }
    it->second = size;
    return true;
}
//=============================================================================
// The callback to run, nullptr when empty or disabled by AutoResizeChildren.
GOCallbackProperty*
activeSizeChangedCallback(GraphicsObject* go)
{
    auto* callback = dynamic_cast<GOCallbackProperty*>(
        go->findProperty(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR, false));
    if (!callback || callback->get().isEmpty()) {
        return nullptr;
    }
    if (go->haveProperty(GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR)
        && go->stringCheck(GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        return nullptr;
    }
    return callback;
}
//=============================================================================
GOFigure*
owningFigure(GraphicsObject* go)
{
    if (go->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
        return static_cast<GOFigure*>(go);
    }
    return go->getParentFigure();
}
//=============================================================================
bool
isFigureVisible(GOFigure* fig)
{
    return !fig->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
}
//=============================================================================
bool
isContainerVisible(GraphicsObject* go)
{
    return !go->haveProperty(GO_VISIBLE_PROPERTY_NAME_STR)
        || !go->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
}
//=============================================================================
GraphicsObject*
parentContainer(GraphicsObject* go)
{
    auto* parent
        = dynamic_cast<GOGObjectsProperty*>(go->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parent || parent->data().empty() || parent->data()[0] < HANDLE_OFFSET_OBJECT) {
        return nullptr;
    }
    return findGraphicsObject(parent->data()[0], false);
}
//=============================================================================
// True when neither the container nor a container holding it is invisible.
bool
isShownInFigure(GraphicsObject* go)
{
    int guard = 0;
    for (GraphicsObject* current = go; current && guard < 64;
        current = parentContainer(current), ++guard) {
        if (!isContainerVisible(current)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
int
containerDepth(GraphicsObject* go)
{
    int depth = 0;
    for (GraphicsObject* current = parentContainer(go); current && depth < 64;
        current = parentContainer(current)) {
        ++depth;
    }
    return depth;
}
//=============================================================================
// Held objects under go, then go itself: children before their parent, the newest child first.
void
collectHeld(GraphicsObject* go, int depth, std::vector<int64>& handles)
{
    auto* children
        = dynamic_cast<GOGObjectsProperty*>(go->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (children && depth < 64) {
        // Stored in creation order.
        const std::vector<int64> childHandles = children->data();
        for (auto it = childHandles.rbegin(); it != childHandles.rend(); ++it) {
            GraphicsObject* child = findGraphicsObject(*it, false);
            if (child) {
                collectHeld(child, depth + 1, handles);
            }
        }
    }
    if (heldUntilShown.find(go) != heldUntilShown.end()) {
        handles.push_back(findGraphicsObjectHandle(go));
    }
}
//=============================================================================
// True when the Position size differs from the last recorded one.
bool
recordFigureSize(GOFigure* fig)
{
    std::vector<double> position = fig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (position.size() < 4) {
        return true;
    }
    const std::pair<double, double> size(position[2], position[3]);
    auto it = reportedFigureSizes.find(fig);
    if (it == reportedFigureSizes.end()) {
        reportedFigureSizes.emplace(fig, size);
        return true;
    }
    if (it->second == size) {
        return false;
    }
    it->second = size;
    return true;
}
//=============================================================================
// Raises again what was held in ancestor, in the reference order; what is still hidden stays held.
void
releaseHeld(GraphicsObject* ancestor)
{
    std::vector<int64> handles;
    collectHeld(ancestor, 0, handles);
    const bool isFigure = ancestor->isType(GO_PROPERTY_VALUE_FIGURE_STR);
    // Showing a container is a statement of its own: nothing it releases is deferred.
    const int savedDeferralDepth = deferralDepth;
    deferralDepth = 0;
    for (int64 handle : handles) {
        GraphicsObject* go = handle != -1 ? findGraphicsObject(handle, false) : nullptr;
        // The figure is raised by its own caller, after its children.
        if (go && !(isFigure && go == ancestor) && heldUntilShown.erase(go) > 0) {
            notifySizeChanged(go);
        }
    }
    deferralDepth = savedDeferralDepth;
}
//=============================================================================
// Handle of the tab holding go (go itself when it is a tab), -1 without one.
int64
owningTabHandle(GraphicsObject* go)
{
    int guard = 0;
    for (GraphicsObject* current = go; current && guard < 64;
        current = parentContainer(current), ++guard) {
        if (current->isType(GO_PROPERTY_VALUE_UITAB_STR)) {
            return findGraphicsObjectHandle(current);
        }
    }
    return -1;
}
//=============================================================================
void
deferSizeChanged(GraphicsObject* go)
{
    const int64 handle = findGraphicsObjectHandle(go);
    if (handle == -1) {
        return;
    }
    std::vector<int64>& handles = openDeferredBatch().handles;
    if (std::find(handles.begin(), handles.end(), handle) == handles.end()) {
        handles.push_back(handle);
    }
}
//=============================================================================
// The batch still open, a new one otherwise, with its run queued.
DeferredBatch&
openDeferredBatch()
{
    // Resizes not separated by a callback processing share one batch.
    if (deferredBatches.empty() || deferredBatches.back().delayStarted) {
        deferredBatches.emplace_back();
    }
    // One queued run serves every batch; a cleared queue lost it.
    if (!deferredRunQueued || CallbackQueue::getInstance()->isEmpty()) {
        queueDeferredRun();
    }
    return deferredBatches.back();
}
//=============================================================================
void
deferTabPageSize(GraphicsObject* tab, double width, double height)
{
    const int64 handle = findGraphicsObjectHandle(tab);
    if (handle == -1) {
        return;
    }
    auto& sizes = openDeferredBatch().tabPageSizes;
    for (auto& entry : sizes) {
        if (entry.first == handle) {
            entry.second = { width, height };
            return;
        }
    }
    sizes.push_back({ handle, { width, height } });
}
//=============================================================================
// The tabs of a batch now report the size they had when it was closed.
void
reportTabPageSizes(const DeferredBatch& batch)
{
    for (const auto& entry : batch.tabPageSizes) {
        GraphicsObject* tab = findGraphicsObject(entry.first, false);
        if (!tab) {
            continue;
        }
        auto actual = containerPixelSizes.find(tab);
        if (actual != containerPixelSizes.end() && actual->second == entry.second) {
            reportedTabSizes.erase(tab);
        } else {
            reportedTabSizes[tab] = entry.second;
        }
    }
}
//=============================================================================
void
runDeferredBatch(const std::vector<int64>& handles)
{
    // Tab after tab in creation order, nested containers first, whatever the host raised first.
    std::vector<std::pair<std::pair<int64, int>, int64>> ordered;
    for (int64 handle : handles) {
        GraphicsObject* go = findGraphicsObject(handle, false);
        if (go) {
            ordered.push_back({ { owningTabHandle(go), -containerDepth(go) }, handle });
        }
    }
    std::stable_sort(ordered.begin(), ordered.end(),
        [](const auto& left, const auto& right) { return left.first < right.first; });
    // What is hidden by now is held, the rest runs before this call returns.
    SizeChangedScope scope;
    for (const auto& entry : ordered) {
        notifySizeChanged(findGraphicsObject(entry.second, false));
    }
}
//=============================================================================
void
queueDeferredRun()
{
    deferredRunQueued = true;
    CallbackQueue::getInstance()->add(GraphicCallback(
        true, BUSY_ACTION::QUEUE, ArrayOf::characterArray(L"__size_changed_deferred__();")));
}
//=============================================================================
void
runScopedHandles()
{
    // A callback can resize again: its own scopes run what it raises.
    std::vector<int64> handles;
    handles.swap(scopedHandles);
    for (int64 handle : handles) {
        GraphicsObject* go = handle >= HANDLE_OFFSET_OBJECT
            ? findGraphicsObject(handle, false)
            : static_cast<GraphicsObject*>(findGOFigure(handle));
        if (!go) {
            continue;
        }
        GOCallbackProperty* callback = activeSizeChangedCallback(go);
        if (callback) {
            callback->executeNow(go, L"SizeChangedData", L"SizeChanged");
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
