//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <thread>
#include "BackgroundPoolObject.hpp"
#include "FevalQueueObject.hpp"
#include "HandleManager.hpp"
#include "WaitFutures.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
FevalQueueObject* FevalQueueObject::m_pInstance = nullptr;
//=============================================================================
FevalQueueObject*
FevalQueueObject::getInstance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new FevalQueueObject();
    }
    return m_pInstance;
}
//=============================================================================
void
FevalQueueObject::destroy()
{
    if (m_pInstance != nullptr) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
bool
FevalQueueObject::isInitialized()
{
    return (m_pInstance != nullptr);
}
//=============================================================================
FevalQueueObject::FevalQueueObject()
    : HandleGenericObject(NLS_HANDLE_FEVALQUEUE_CATEGORY_STR, this, false)
{
    propertiesNames = { L"QueuedFutures", L"RunningFutures" };
}
//=============================================================================
void
FevalQueueObject::display(Interface* io)
{
#define BLANKS_AT_BOL std::wstring(L"   ")

    if (io) {
        size_t nbQueued = BackgroundPoolObject::getInstance()->getTasksQueued();
        size_t nbRunning = BackgroundPoolObject::getInstance()->getTasksRunning();
        if (nbQueued) {
            io->outputMessage(BLANKS_AT_BOL
                + L"QueuedFutures"
                  L" [1x"
                + std::to_wstring(nbQueued) + L" FevalFuture]" + L"\n");
        } else {
            io->outputMessage(BLANKS_AT_BOL + L" QueuedFutures [0x0 FevalFuture]" + L"\n");
        }
        if (nbRunning) {
            io->outputMessage(BLANKS_AT_BOL + L"RunningFutures" + L" [1x"
                + std::to_wstring(nbRunning) + L" FevalFuture]" + L"\n");
        } else {
            io->outputMessage(BLANKS_AT_BOL + L" RunningFutures [0x0 FevalFuture]" + L"\n");
        }
    }
}
//=============================================================================
FevalQueueObject::~FevalQueueObject()
{
    if (m_pInstance == this) {
        m_pInstance = nullptr;
    }
    fEvalQueue.clear();
}
//=============================================================================
void
FevalQueueObject::add(const HandleRef& fevalFutureObject)
{
    refreshQueue();
    fEvalQueue.push_back(fevalFutureObject);
}
//=============================================================================
std::vector<nelson_handle>
FevalQueueObject::searchThreadsByState(THREAD_STATE stateDesired)
{
    refreshQueue();
    std::vector<nelson_handle> handles;
    for (auto& weak : fEvalQueue) {
        HandleRef ref = weak.lock();
        auto* k = ref ? dynamic_cast<FevalFutureObject*>(ref->object()) : nullptr;
        if (k == nullptr) {
            continue;
        }
        FutureStateGuardRead guard(k->stateMutex);
        if (k->state == stateDesired) {
            nelson_handle asNelsonHandle = ref->id();
            nelson_handle nh = asNelsonHandle;
            handles.push_back(nh);
        }
    }
    refreshQueue();
    return handles;
}
//=============================================================================
ArrayOf
FevalQueueObject::getThreadsByState(THREAD_STATE stateDesired)
{
    std::vector<nelson_handle> fEvalFutureHandles = searchThreadsByState(stateDesired);
    HandleRef* nh = nullptr;
    indexType len = 0;
    Dimensions dims(fEvalFutureHandles.empty() ? 0 : 1, fEvalFutureHandles.size());
    len = dims.elementCount();
    nh = static_cast<HandleRef*>(ArrayOf::allocateArrayOf(NLS_HANDLE, len, stringVector(), false));

    for (indexType k = 0; k < len; k++) {
        nh[k] = HandleManager::getInstance()->getHandleRef(fEvalFutureHandles[k]);
    }
    return ArrayOf(NLS_HANDLE, dims, (void*)nh);
}
//=============================================================================
bool
FevalQueueObject::get(const std::wstring& propertyName, ArrayOf& result)
{
    if (propertyName == L"QueuedFutures") {
        result = getThreadsByState(THREAD_STATE::QUEUED);
        return true;
    }
    if (propertyName == L"RunningFutures") {
        result = getThreadsByState(THREAD_STATE::RUNNING);
        return true;
    }
    return false;
}
//=============================================================================
bool
FevalQueueObject::isMethod(const std::wstring& methodName)
{
    return std::find(propertiesNames.begin(), propertiesNames.end(), methodName)
        != propertiesNames.end();
}
//=============================================================================
void
FevalQueueObject::refreshQueue()
{
    if (fEvalQueue.empty()) {
        return;
    }
    fEvalQueue.erase(std::remove_if(fEvalQueue.begin(), fEvalQueue.end(),
                         [](const std::weak_ptr<HandleState>& weak) -> bool {
                             HandleRef ref = weak.lock();
                             auto* f
                                 = ref ? dynamic_cast<FevalFutureObject*>(ref->object()) : nullptr;
                             if (f == nullptr) {
                                 return true;
                             }
                             FutureStateGuardRead guard(f->stateMutex);
                             THREAD_STATE state = f->state.load();
                             bool isInactive = (state != THREAD_STATE::QUEUED)
                                 && (state != THREAD_STATE::RUNNING);
                             return isInactive;
                         }),
        fEvalQueue.end());
}
//=============================================================================
void
FevalQueueObject::reset()
{
    for (auto& weak : fEvalQueue) {
        HandleRef ref = weak.lock();
        auto* k = ref ? dynamic_cast<FevalFutureObject*>(ref->object()) : nullptr;
        if (k != nullptr) {
            k->cancel();
        }
    }
    refreshQueue();
    fEvalQueue.clear();
}
//=============================================================================
void
FevalQueueObject::cleanupFinishedFutures()
{
    refreshQueue();
    if (fEvalQueue.empty() && BackgroundPoolObject::isInitialized()) {
        BackgroundPoolObject::getInstance()->purgeQueuedTasks();
    }
}
//=============================================================================
void
FevalQueueObject::cancelAll()
{
    for (auto& weak : fEvalQueue) {
        HandleRef ref = weak.lock();
        auto* k = ref ? dynamic_cast<FevalFutureObject*>(ref->object()) : nullptr;
        if (k != nullptr) {
            k->cancel();
        }
    }
    refreshQueue();
    if (BackgroundPoolObject::isInitialized()) {
        BackgroundPoolObject* backgroundPool = BackgroundPoolObject::getInstance();
        backgroundPool->purgeQueuedTasks();
        backgroundPool->waitForTasksDone();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
