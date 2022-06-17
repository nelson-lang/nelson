//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
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
        size_t nbQueued = 0;
        size_t nbRunning = 0;

        for (size_t k = 0; k < fEvalQueue.size(); k++) {
            if (fEvalQueue[k]->getState() == THREAD_STATE::QUEUED) {
                nbQueued++;
            }
            if (fEvalQueue[k]->getState() == THREAD_STATE::RUNNING) {
                nbRunning++;
            }
        }
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
bool
FevalQueueObject::get(const std::wstring& propertyName, ArrayOf& result)
{
    if (propertyName == L"QueuedFutures") {
        std::vector<FevalFutureObject*> queued;
        for (size_t k = 0; k < fEvalQueue.size(); k++) {
            if (fEvalQueue[k]->getState() == THREAD_STATE::QUEUED) {
                queued.push_back((FevalFutureObject*)fEvalQueue[k]->getPointer());
            }
        }
        if (queued.size() == 0) {
            Dimensions dims(0, 0);
            nelson_handle* nh = static_cast<nelson_handle*>(
                ArrayOf::allocateArrayOf(NLS_HANDLE, 0, stringVector(), false));
            result = ArrayOf(NLS_HANDLE, dims, (void*)nh);
        } else {
            Dimensions dims(1, queued.size());
            nelson_handle* nh = static_cast<nelson_handle*>(
                ArrayOf::allocateArrayOf(NLS_HANDLE, queued.size(), stringVector(), false));
            for (size_t k = 0; k < queued.size(); k++) {
                nelson_handle fnh = HandleManager::getInstance()->findByPointerValue(queued[k]);
                nh[k] = fnh;
            }
            result = ArrayOf(NLS_HANDLE, dims, (void*)nh);
        }
        return true;
    }
    if (propertyName == L"RunningFutures") {
        std::vector<FevalFutureObject*> running;
        for (size_t k = 0; k < fEvalQueue.size(); k++) {
            if (fEvalQueue[k]->getState() == THREAD_STATE::RUNNING) {
                running.push_back((FevalFutureObject*)fEvalQueue[k]->getPointer());
            }
        }
        if (running.size() == 0) {
            Dimensions dims(0, 0);
            nelson_handle* nh = static_cast<nelson_handle*>(
                ArrayOf::allocateArrayOf(NLS_HANDLE, 0, stringVector(), false));
            result = ArrayOf(NLS_HANDLE, dims, (void*)nh);
        } else {
            Dimensions dims(1, running.size());

            nelson_handle* nh = static_cast<nelson_handle*>(
                ArrayOf::allocateArrayOf(NLS_HANDLE, running.size(), stringVector(), false));
            for (size_t k = 0; k < running.size(); k++) {
                nelson_handle fnh = HandleManager::getInstance()->findByPointerValue(running[k]);
                nh[k] = fnh;
            }
            result = ArrayOf(NLS_HANDLE, dims, (void*)nh);
        }
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
