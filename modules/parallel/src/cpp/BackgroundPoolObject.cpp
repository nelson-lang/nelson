//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <tuple>
#include "BackgroundPoolObject.hpp"
#include "characters_encoding.hpp"
#include "FevalQueueObject.hpp"
#include "TimeHelpers.hpp"
#include "NelsonConfiguration.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleState.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
BackgroundPoolObject* BackgroundPoolObject::m_pInstance = nullptr;
//=============================================================================
BackgroundPoolObject*
BackgroundPoolObject::getInstance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new BackgroundPoolObject();
    }
    return m_pInstance;
}
//=============================================================================
bool
BackgroundPoolObject::isInitialized()
{
    return (m_pInstance != nullptr);
}
//=============================================================================
BackgroundPoolObject::~BackgroundPoolObject()
{
    if (m_pInstance == this) {
        m_pInstance = nullptr;
    }
    if (threadPool) {
        resetThreadPool();
        delete threadPool;
        threadPool = nullptr;
    }
}
//=============================================================================
void
BackgroundPoolObject::destroy()
{
    if (m_pInstance != nullptr) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
size_t
BackgroundPoolObject::getTasksQueued()
{
    return threadPool->get_tasks_queued();
}
//=============================================================================
size_t
BackgroundPoolObject::getTasksRunning()
{
    return threadPool->get_tasks_running();
}
//=============================================================================
size_t
BackgroundPoolObject::getNumberOfThreads()
{
    return threadPool->get_thread_count();
}
//=============================================================================
BackgroundPoolObject::BackgroundPoolObject()
    : HandleGenericObject(NLS_HANDLE_BACKGROUNDPOOL_CATEGORY_STR, this, false)
{
    threadPool = new BS::pause_thread_pool(NelsonConfiguration::getInstance()->maxNumCompThreads());
    propertiesNames = { L"NumWorkers", L"Busy", L"FevalQueue" };
}
//=============================================================================
wstringVector
BackgroundPoolObject::properties()
{
    return propertiesNames;
}
//=============================================================================
wstringVector
BackgroundPoolObject::methods()
{
    return {};
}
//=============================================================================
void
BackgroundPoolObject::display(Interface* io)
{
    if (io) {
        bool busy = (threadPool->get_tasks_running() != 0);
        size_t numWorkers = threadPool->get_thread_count();
        size_t nbFevalQueue = threadPool->get_tasks_running() + threadPool->get_tasks_queued();
#define BLANKS_AT_BOL std::wstring(L"   ")
        io->outputMessage(L"\n");
        io->outputMessage(BLANKS_AT_BOL + L"NumWorkers: " + std::to_wstring(numWorkers) + L"\n");
        std::wstring busyString = busy ? L"true" : L"false";
        io->outputMessage(BLANKS_AT_BOL + L"Busy: " + busyString + L"\n");

        if (nbFevalQueue > 0) {
            std::wstring msg;
            if (nbFevalQueue == 1) {
                msg = BLANKS_AT_BOL + L"FevalQueue: " + std::to_wstring(nbFevalQueue) + L" "
                    + _W("outstanding future.");
            } else {
                msg = BLANKS_AT_BOL + L"FevalQueue: " + std::to_wstring(nbFevalQueue) + L" "
                    + _W("outstanding futures.");
            }
            io->outputMessage(msg + L"\n");
        }
    }
}
//=============================================================================
bool
BackgroundPoolObject::get(const std::wstring& propertyName, ArrayOf& result)
{
    if (propertyName == L"NumWorkers") {
        size_t numWorkers = threadPool->get_thread_count();
        result = ArrayOf::doubleScalar((double)numWorkers);
        return true;
    }
    if (propertyName == L"Busy") {
        bool busy = (threadPool->get_tasks_running() != 0);
        result = ArrayOf::logicalScalar(busy);
        return true;
    }
    if (propertyName == L"FevalQueue") {
        result = ArrayOf::handle(FevalQueueObject::getInstance());
        return true;
    }
    return false;
}
//=============================================================================
ArrayOf
BackgroundPoolObject::feval(
    const ArrayOf& functionHandle, FunctionDef* fptr, int nLhs, const ArrayOfVector& argIn)
{
    FevalFutureObject* retFuture = nullptr;
    try {
        if (fptr->type() == NLS_ANONYMOUS_MACRO_FUNCTION) {
            AnonymousMacroFunctionDef* afptr = (AnonymousMacroFunctionDef*)fptr;
            afptr->updateCode(nLhs);
            retFuture = new FevalFutureObject(utf8_to_wstring(afptr->getDefinition()));
        } else {
            retFuture = new FevalFutureObject(L"@" + utf8_to_wstring(fptr->getName()));
        }
    } catch (std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    ArrayOf result = ArrayOf::handle(retFuture);
    HandleRef* qp = (HandleRef*)(result.dataPointer());
    HandleRef futureRef = qp[0];
    retFuture->asNelsonHandle = futureRef ? futureRef->id() : 0;
    auto future
        = threadPool->submit_task([futureRef, retFuture, functionHandle, fptr, nLhs, argIn]() {
              retFuture->evaluateFunction(fptr, nLhs, argIn, true);
          });
    FevalQueueObject::getInstance()->add(futureRef);
    return result;
}
//=============================================================================
wstringVector
BackgroundPoolObject::fieldnames()
{
    return propertiesNames;
}
//=============================================================================
void
BackgroundPoolObject::resetThreadPool()
{
    if (threadPool) {
        threadPool->pause();
        if (FevalQueueObject::isInitialized()) {
            FevalQueueObject::getInstance()->reset();
        }
        threadPool->purge();
        threadPool->reset(NelsonConfiguration::getInstance()->maxNumCompThreads());
        threadPool->unpause();
    }
}
//=============================================================================
void
BackgroundPoolObject::purgeQueuedTasks()
{
    if (threadPool) {
        threadPool->purge();
    }
}
//=============================================================================
void
BackgroundPoolObject::waitForTasksDone()
{
    if (threadPool) {
        threadPool->wait();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
