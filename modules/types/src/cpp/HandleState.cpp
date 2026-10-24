//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HandleState.hpp"
#include <algorithm>
#include <deque>
#include <utility>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    struct FinalizerQueue
    {
        std::deque<std::pair<nelson_handle, HandleGenericObject*>> entries;
        bool closed = false;

        ~FinalizerQueue();
    };
    //=============================================================================
    FinalizerQueue&
    threadFinalizerQueue()
    {
        static thread_local FinalizerQueue queue;
        return queue;
    }
    //=============================================================================
} // namespace
//=============================================================================
std::atomic<size_t> pendingHandleFinalizerCount { 0 };
//=============================================================================
HandleState::HandleState(nelson_handle id, HandleGenericObject* object, bool owns)
    : handleId(id)
    , categoryName(object == nullptr ? std::string(NLS_HANDLE_STR) : object->category())
    , scoped(object != nullptr && object->isScoped())
    , ownsObject(owns)
    , valid(object != nullptr)
    , objectPtr(object)
{
}
//=============================================================================
HandleState::HandleState(nelson_handle id, const std::string& category)
    : handleId(id)
    , categoryName(category.empty() ? std::string(NLS_HANDLE_STR) : category)
    , scoped(false)
    , ownsObject(true)
    , valid(false)
    , objectPtr(nullptr)
{
}
//=============================================================================
HandleState::~HandleState()
{
    if (!ownsObject) {
        objectPtr.release();
        return;
    }
    // This is the exact last-reference point of the handle: no Evaluator is
    // reachable from a destructor, so an object with a user-level destructor
    // is parked for the interpreter to finalize at its next safe point.
    if (valid && objectPtr != nullptr && objectPtr->wantsDeferredFinalization()) {
        PendingHandleFinalizers::enqueue(handleId, objectPtr.release());
    }
}
//=============================================================================
nelson_handle
HandleState::id() const
{
    return handleId;
}
//=============================================================================
bool
HandleState::isValid() const
{
    return valid && objectPtr != nullptr;
}
//=============================================================================
std::string
HandleState::category() const
{
    return categoryName.empty() ? std::string(NLS_HANDLE_STR) : categoryName;
}
//=============================================================================
std::string
HandleState::className() const
{
    std::lock_guard<std::mutex> lock(mutex);
    return objectPtr == nullptr ? category() : objectPtr->className();
}
//=============================================================================
bool
HandleState::isScoped() const
{
    return scoped;
}
//=============================================================================
void*
HandleState::nativePointer() const
{
    std::lock_guard<std::mutex> lock(mutex);
    return objectPtr == nullptr ? nullptr : objectPtr->pointer();
}
//=============================================================================
HandleGenericObject*
HandleState::object() const
{
    std::lock_guard<std::mutex> lock(mutex);
    return valid ? objectPtr.get() : nullptr;
}
//=============================================================================
void
HandleState::invalidate(Evaluator* eval)
{
    HandleGenericObject* object = nullptr;
    {
        std::lock_guard<std::mutex> lock(mutex);
        if (!valid) {
            return;
        }
        object = objectPtr.get();
    }
    if (object != nullptr) {
        object->onDelete(eval);
    }

    std::unique_ptr<HandleGenericObject> objectToDelete;
    {
        std::lock_guard<std::mutex> lock(mutex);
        if (!valid) {
            return;
        }
        valid = false;
        if (ownsObject && objectPtr != nullptr && objectPtr->destroyOnInvalidate()) {
            objectToDelete = std::move(objectPtr);
        } else if (!ownsObject) {
            objectPtr.release();
        }
    }
}
//=============================================================================
HandleRegistry&
HandleRegistry::instance()
{
    static HandleRegistry registry;
    return registry;
}
//=============================================================================
HandleRef
HandleRegistry::add(HandleGenericObject* object)
{
    std::lock_guard<std::mutex> lock(mutex);
    if (object == nullptr) {
        return {};
    }
    void* native = object->pointer();
    if (native != nullptr) {
        auto found = handlesByNative.find(native);
        if (found != handlesByNative.end()) {
            HandleRef current = found->second.lock();
            if (current && current->isValid() && current->nativePointer() == native) {
                return current;
            }
            handlesByNative.erase(found);
        }
    }
    maybePurgeLocked();
    HandleRef ref
        = std::make_shared<HandleState>(++nextId, object, object->useSharedHandleOwnership());
    handles.push_back(ref);
    handlesById.emplace(ref->id(), ref);
    if (native != nullptr) {
        handlesByNative[native] = ref;
    }
    return ref;
}
//=============================================================================
HandleRef
HandleRegistry::addInvalid(const std::string& categoryName)
{
    std::lock_guard<std::mutex> lock(mutex);
    maybePurgeLocked();
    HandleRef ref = std::make_shared<HandleState>(++nextId, categoryName);
    handles.push_back(ref);
    handlesById.emplace(ref->id(), ref);
    return ref;
}
//=============================================================================
HandleRef
HandleRegistry::find(nelson_handle id)
{
    std::lock_guard<std::mutex> lock(mutex);
    auto found = handlesById.find(id);
    if (found == handlesById.end()) {
        return {};
    }
    HandleRef current = found->second.lock();
    if (!current || !current->isValid()) {
        handlesById.erase(found);
        return {};
    }
    return current;
}
//=============================================================================
HandleRef
HandleRegistry::findByPointerValue(void* ptr)
{
    std::lock_guard<std::mutex> lock(mutex);
    if (ptr == nullptr) {
        return {};
    }
    auto found = handlesByNative.find(ptr);
    if (found == handlesByNative.end()) {
        return {};
    }
    HandleRef current = found->second.lock();
    if (!current || !current->isValid() || current->nativePointer() != ptr) {
        handlesByNative.erase(found);
        return {};
    }
    return current;
}
//=============================================================================
std::vector<HandleRef>
HandleRegistry::getAllHandlesOfCategory(const std::string& category)
{
    std::lock_guard<std::mutex> lock(mutex);
    purgeExpiredLocked();
    std::vector<HandleRef> result;
    for (auto& weak : handles) {
        HandleRef current = weak.lock();
        if (current && current->isValid() && current->category() == category) {
            result.push_back(current);
        }
    }
    return result;
}
//=============================================================================
HandleRef
HandleRegistry::adopt(nelson_handle id, HandleGenericObject* object)
{
    std::lock_guard<std::mutex> lock(mutex);
    if (object == nullptr) {
        return {};
    }
    maybePurgeLocked();
    HandleRef ref = std::make_shared<HandleState>(id, object, object->useSharedHandleOwnership());
    handles.push_back(ref);
    handlesById[id] = ref;
    void* native = object->pointer();
    if (native != nullptr) {
        handlesByNative[native] = ref;
    }
    return ref;
}
//=============================================================================
bool
HandleRegistry::remove(nelson_handle id)
{
    std::lock_guard<std::mutex> lock(mutex);
    auto found = handlesById.find(id);
    if (found == handlesById.end()) {
        return false;
    }
    HandleRef current = found->second.lock();
    if (current) {
        void* native = current->nativePointer();
        if (native != nullptr) {
            auto byNative = handlesByNative.find(native);
            if (byNative != handlesByNative.end()) {
                HandleRef nativeRef = byNative->second.lock();
                if (!nativeRef || nativeRef->id() == id) {
                    handlesByNative.erase(byNative);
                }
            }
        }
    }
    handlesById.erase(found);
    // The matching weak_ptr left in `handles` expires with the HandleState and
    // is reclaimed by the next purge. Erasing it here by scanning the vector
    // would make every removal O(number of live handles), so churning N handles
    // (each creation drops a temporary) would be O(n^2); the maps keep removal
    // O(1).
    return true;
}
//=============================================================================
void
HandleRegistry::clear()
{
    std::lock_guard<std::mutex> lock(mutex);
    handles.clear();
    handlesById.clear();
    handlesByNative.clear();
}
//=============================================================================
void
HandleRegistry::purgeExpiredLocked()
{
    handles.erase(std::remove_if(handles.begin(), handles.end(),
                      [](const std::weak_ptr<HandleState>& weak) {
                          HandleRef current = weak.lock();
                          return !current || !current->isValid();
                      }),
        handles.end());
    for (auto it = handlesById.begin(); it != handlesById.end();) {
        HandleRef current = it->second.lock();
        if (!current || !current->isValid()) {
            it = handlesById.erase(it);
        } else {
            ++it;
        }
    }
    for (auto it = handlesByNative.begin(); it != handlesByNative.end();) {
        HandleRef current = it->second.lock();
        if (!current || !current->isValid()) {
            it = handlesByNative.erase(it);
        } else {
            ++it;
        }
    }
}
//=============================================================================
void
HandleRegistry::maybePurgeLocked()
{
    if (handles.size() < purgeWatermark) {
        return;
    }
    purgeExpiredLocked();
    purgeWatermark = std::max<size_t>(64, handles.size() * 2);
}
//=============================================================================
void
PendingHandleFinalizers::enqueue(nelson_handle id, HandleGenericObject* object)
{
    if (object == nullptr) {
        return;
    }
    FinalizerQueue& queue = threadFinalizerQueue();
    if (queue.closed) {
        delete object;
        return;
    }
    queue.entries.emplace_back(id, object);
    pendingHandleFinalizerCount.fetch_add(1, std::memory_order_relaxed);
}
//=============================================================================
bool
PendingHandleFinalizers::takeAll(std::vector<Entry>& batch)
{
    FinalizerQueue& queue = threadFinalizerQueue();
    if (queue.entries.empty()) {
        return false;
    }
    batch.assign(queue.entries.begin(), queue.entries.end());
    pendingHandleFinalizerCount.fetch_sub(queue.entries.size(), std::memory_order_relaxed);
    queue.entries.clear();
    return true;
}
//=============================================================================
void
PendingHandleFinalizers::requeueFront(const std::vector<Entry>& entries)
{
    if (entries.empty()) {
        return;
    }
    FinalizerQueue& queue = threadFinalizerQueue();
    if (queue.closed) {
        for (const Entry& entry : entries) {
            delete entry.second;
        }
        return;
    }
    queue.entries.insert(queue.entries.begin(), entries.begin(), entries.end());
    pendingHandleFinalizerCount.fetch_add(entries.size(), std::memory_order_relaxed);
}
//=============================================================================
void
PendingHandleFinalizers::discardAll()
{
    std::vector<Entry> batch;
    // Destroying an object can release nested handles and queue more entries.
    while (takeAll(batch)) {
        for (const Entry& entry : batch) {
            delete entry.second;
        }
    }
}
//=============================================================================
namespace {
    //=============================================================================
    FinalizerQueue::~FinalizerQueue()
    {
        closed = true;
        while (!entries.empty()) {
            HandleGenericObject* object = entries.front().second;
            entries.pop_front();
            pendingHandleFinalizerCount.fetch_sub(1, std::memory_order_relaxed);
            delete object;
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
} // namespace Nelson
//=============================================================================
