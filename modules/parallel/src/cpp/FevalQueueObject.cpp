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
FevalQueueObject::~FevalQueueObject() = default;
//=============================================================================
void
FevalQueueObject::add(FevalFutureObject* fevalFutureObject)
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
    for (auto& k : fEvalQueue) {
        if (k->state == stateDesired) {
            nelson_handle asNelsonHandle = k->asNelsonHandle;
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
    std::vector<nelson_handle> fEvalFuturHandles = searchThreadsByState(stateDesired);
    nelson_handle* nh = nullptr;
    indexType len = 0;
    Dimensions dims(fEvalFuturHandles.empty() ? 0 : 1, fEvalFuturHandles.size());
    len = dims.getElementCount();
    nh = static_cast<nelson_handle*>(
        ArrayOf::allocateArrayOf(NLS_HANDLE, len, stringVector(), false));

    for (indexType k = 0; k < len; k++) {
        nh[k] = fEvalFuturHandles[k];
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
                         [](FevalFutureObject* f) {
                             return f->state != THREAD_STATE::QUEUED
                                 && f->state != THREAD_STATE::RUNNING;
                         }),
        fEvalQueue.end());
}
//=============================================================================
void
FevalQueueObject::reset()
{
    for (auto& k : fEvalQueue) {
        k->cancel();
    }
    refreshQueue();
    fEvalQueue.clear();
}
//=============================================================================
void
FevalQueueObject::cancelAll()
{
    BackgroundPoolObject::getInstance()->resetThreadPool();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
