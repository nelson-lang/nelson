//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
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
    : HandleGenericObject(std::wstring(FEVALQUEUE_CATEGORY_STR), this, false)
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
FevalQueueObject::~FevalQueueObject() {}
//=============================================================================
void
FevalQueueObject::add(FevalFutureObject* fevalFutureObject)
{
    fEvalQueue.push_back(fevalFutureObject);
    fEvalQueueStates.push_back(fevalFutureObject->state);
}
//=============================================================================
std::vector<FevalFutureObject*>
FevalQueueObject::getQueue()
{
    return fEvalQueue;
}
//=============================================================================
size_t
FevalQueueObject::getQueueLength()
{
    return fEvalQueue.size();
}
//=============================================================================
std::vector<nelson_handle>
FevalQueueObject::searchThreadsByState(THREAD_STATE stateDesired)
{
    std::vector<nelson_handle> handles;
    for (size_t k = 0; k < fEvalQueue.size(); k++) {
        if (fEvalQueue[k]->state == stateDesired) {
            nelson_handle asNelsonHandle = fEvalQueue[k]->asNelsonHandle;
            nelson_handle nh = asNelsonHandle;
            handles.push_back(nh);
        }
    }
    return handles;
}
//=============================================================================
ArrayOf
FevalQueueObject::getThreadsByState(THREAD_STATE stateDesired)
{
    std::vector<nelson_handle> fEvalFuturHandles = searchThreadsByState(stateDesired);
    nelson_handle* nh = nullptr;
    Dimensions dims;
    indexType len = 0;
    if (fEvalFuturHandles.size() == 0) {
        dims = Dimensions(0, 0);
    } else {
        dims = Dimensions(1, fEvalFuturHandles.size());
    }
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
    for (auto name : propertiesNames) {
        if (name == methodName) {
            return true;
        }
    }
    return false;
}
//=============================================================================

} // namespace Nelson
//=============================================================================
