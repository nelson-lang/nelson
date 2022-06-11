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
            io->outputMessage(
                BLANKS_AT_BOL + 
              L"RunningFutures" +
              L" [1x" + std::to_wstring(nbRunning) + L" FevalFuture]" + L"\n");
        } else {
            io->outputMessage(BLANKS_AT_BOL + L" RunningFutures [0x0 FevalFuture]" + L"\n");
        }

    }
}
//=============================================================================
FevalQueueObject::~FevalQueueObject() { }
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
} // namespace Nelson
//=============================================================================
