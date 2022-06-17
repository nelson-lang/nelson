//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <tuple>
#include <ctime>
#include "BackgroundPoolObject.hpp"
#include "Evaluator.hpp"
#include "characters_encoding.hpp"
#include "FevalQueueObject.hpp"
#include "NelsonConfiguration.hpp"
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
BackgroundPoolObject::~BackgroundPoolObject()
{
    if (threadPool) {
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
    : HandleGenericObject(std::wstring(BACKGROUNDPOOL_CATEGORY_STR), this, false)
{
    threadPool = new BS::thread_pool(NelsonConfiguration::getInstance()->getMaxNumCompThreads());
    propertiesNames = { L"NumWorkers", L"Busy", L"FevalQueue" };
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
        result = ArrayOf::doubleConstructor((double)numWorkers);
        return true;
    }
    if (propertyName == L"Busy") {
        bool busy = (threadPool->get_tasks_running() != 0);
        result = ArrayOf::logicalConstructor(busy);
        return true;
    }
    if (propertyName == L"FevalQueue") {
        result = ArrayOf::handleConstructor(FevalQueueObject::getInstance());
        return true;
    }
    return false;
}
//=============================================================================
static uint64
getEpoch()
{
    return (uint64)std::time(nullptr);
}
//=============================================================================
static std::tuple<ArrayOfVector, Exception>
FunctionEvalInternal(FunctionDef* fptr, int nLhs, const ArrayOfVector& argIn,
    std::atomic<THREAD_STATE>* s, std::atomic<uint64>* startRunningDate,
    std::atomic<uint64>* endRunningDate)
{
    *startRunningDate = getEpoch();
    *endRunningDate = (uint64)0;
    *s = THREAD_STATE::RUNNING;

    Context* context = new Context;
    Evaluator* eval = new Evaluator(context, nullptr, false);
    ArrayOfVector retValues;
    Exception retException;
    try {
        retValues = fptr->evaluateFunction(eval, argIn, nLhs);
        *s = THREAD_STATE::FINISHED;
    } catch (Exception& e) {
        retException = e;
        *s = THREAD_STATE::FAILED;
    }
    *endRunningDate = (uint64)getEpoch();
    return std::make_tuple(retValues, retException);
}
//=============================================================================
FevalFutureObject*
BackgroundPoolObject::feval(FunctionDef* fptr, int nLhs, const ArrayOfVector& argIn)
{
    FevalFutureObject* retFuture = new FevalFutureObject(utf8_to_wstring(fptr->getName()));
    std::future<std::tuple<ArrayOfVector, Exception>> f = threadPool->submit(FunctionEvalInternal,
        fptr, nLhs, argIn, &retFuture->state, &retFuture->startDateTime, &retFuture->endDateTime);
    retFuture->setFuture(std::move(f));
    FevalQueueObject::getInstance()->add(retFuture);
    return retFuture;
}
//=============================================================================
wstringVector
BackgroundPoolObject::fieldnames()
{
    return propertiesNames;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
