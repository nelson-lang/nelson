//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <atomic>
#include <memory>
#include <mutex>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>
#include "HandleGenericObject.hpp"
#include "Types.hpp"
#include "nlsTypes_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Shared state for one Nelson handle id. Handle arrays hold shared references
 * to it, so invalidation is visible through every copy.
 */
class NLSTYPES_IMPEXP HandleState
{
public:
    explicit HandleState(nelson_handle id, HandleGenericObject* object, bool ownsObject = true);
    HandleState(nelson_handle id, const std::string& categoryName);
    ~HandleState();

    HandleState(const HandleState&) = delete;
    HandleState&
    operator=(const HandleState&)
        = delete;

    nelson_handle
    id() const;

    bool
    isValid() const;

    std::string
    category() const;

    std::string
    className() const;

    bool
    isScoped() const;

    void*
    nativePointer() const;

    /**
     * Return the wrapped object, or nullptr after invalidation.
     */
    HandleGenericObject*
    object() const;

    /**
     * Mark the handle invalid and run the wrapped object's cleanup hook.
     */
    void
    invalidate(Evaluator* eval);

    /**
     * Invalidate only when category and dynamic type match; the deleter takes
     * ownership of the typed object.
     */
    template <typename T, typename Deleter>
    bool
    invalidateAs(const std::string& expectedCategory, Deleter deleter)
    {
        std::lock_guard<std::mutex> lock(mutex);
        if (!valid || objectPtr == nullptr) {
            return false;
        }
        if (categoryName != expectedCategory) {
            return false;
        }
        auto* typed = dynamic_cast<T*>(objectPtr.get());
        if (typed == nullptr) {
            return false;
        }
        valid = false;
        objectPtr.release();
        deleter(typed);
        return true;
    }

private:
    nelson_handle handleId;
    std::string categoryName;
    bool scoped;
    bool ownsObject;
    mutable std::mutex mutex;
    std::atomic_bool valid;
    std::unique_ptr<HandleGenericObject> objectPtr;
};
//=============================================================================
/**
 * Process-local registry of live handle states. It stores weak references, so
 * ArrayOf values keep handles alive; expired entries are purged on use.
 */
class NLSTYPES_IMPEXP HandleRegistry
{
public:
    static HandleRegistry&
    instance();

    HandleRef
    add(HandleGenericObject* object);

    HandleRef
    addInvalid(const std::string& categoryName);

    HandleRef
    find(nelson_handle id);

    HandleRef
    findByPointerValue(void* ptr);

    std::vector<HandleRef>
    getAllHandlesOfCategory(const std::string& category);

    /**
     * Re-register a native object under its previous handle id, so a deferred
     * user-level destructor can reach it through a valid handle.
     */
    HandleRef
    adopt(nelson_handle id, HandleGenericObject* object);

    bool
    remove(nelson_handle id);

    void
    clear();

private:
    HandleRegistry() = default;

    void
    purgeExpiredLocked();

    void
    maybePurgeLocked();

    std::mutex mutex;
    nelson_handle nextId = 1;
    std::vector<std::weak_ptr<HandleState>> handles;
    // id and native-pointer indexes keep add/find O(1). Entries go stale when
    // the last shared reference drops: every hit is validity-checked and stale
    // entries are erased on sight or during amortized purges.
    std::unordered_map<nelson_handle, std::weak_ptr<HandleState>> handlesById;
    std::unordered_map<void*, std::weak_ptr<HandleState>> handlesByNative;
    size_t purgeWatermark = 64;
};
//=============================================================================
/**
 * Number of objects waiting in PendingHandleFinalizers queues, all threads
 * included. Read with a relaxed load as a fast gate before draining.
 */
extern NLSTYPES_IMPEXP std::atomic<size_t> pendingHandleFinalizerCount;
//=============================================================================
/**
 * Per-thread queue of native objects whose last handle reference was released
 * while their user-level destructor still has to run. The interpreter drains
 * it at the next safe point (statement boundary, function exit, shutdown)
 * where an Evaluator exists. Objects still queued when the thread ends are
 * destroyed without running user code.
 */
class NLSTYPES_IMPEXP PendingHandleFinalizers
{
public:
    static bool
    hasPending()
    {
        return pendingHandleFinalizerCount.load(std::memory_order_relaxed) != 0;
    }

    /**
     * Queue an object (ownership transferred) under its former handle id.
     */
    static void
    enqueue(nelson_handle id, HandleGenericObject* object);

    using Entry = std::pair<nelson_handle, HandleGenericObject*>;

    /**
     * Move every object queued on the current thread into batch (oldest
     * first). Returns false when nothing was pending.
     */
    static bool
    takeAll(std::vector<Entry>& batch);

    /**
     * Put entries back at the head of the current thread's queue, keeping
     * their order (used when a drain is interrupted before finishing).
     */
    static void
    requeueFront(const std::vector<Entry>& entries);

    /**
     * Destroy every object queued on the current thread without running
     * user code.
     */
    static void
    discardAll();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
