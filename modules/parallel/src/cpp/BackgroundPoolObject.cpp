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
#include "BackgroundPoolObject.hpp"
#include "Evaluator.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
BackgroundPoolObject::BackgroundPoolObject()
    : HandleGenericObject(std::wstring(BACKGROUNDPOOL_CATEGORY_STR), this, false)
{
    propertiesNames = { L"NumWorkers", L"Busy", L"FevalQueue" };
}
//=============================================================================
void
BackgroundPoolObject::display(Interface* io)
{
    if (io) {
        bool busy = (threadPool.get_tasks_running() != 0);
        size_t numWorkers = threadPool.get_thread_count();
        size_t nbFevalQueue = fEvalQueue.size();
#define BLANKS_AT_BOL std::wstring(L"   ")
        io->outputMessage(L"\n");
        io->outputMessage(BLANKS_AT_BOL + L"NumWorkers: " + std::to_wstring(numWorkers) + L"\n");
        std::wstring busyString;
        if (busy) {
            busyString = L"true";
        } else {
            busyString = L"false";
        }
        io->outputMessage(BLANKS_AT_BOL + L"Busy: " + busyString + L"\n");
        if (nbFevalQueue > 0) {
            io->outputMessage(
                BLANKS_AT_BOL + L"FevalQueue: " + std::to_wstring(nbFevalQueue) + L"\n");
        }
    }
}
//=============================================================================
BackgroundPoolObject::~BackgroundPoolObject() { }
//=============================================================================
static std::tuple<ArrayOfVector, Exception>
FunctionEvalInternal(FunctionDef* fptr, int nLhs, const ArrayOfVector& argIn)
{
    Context *context = new Context;
    Evaluator* eval = new Evaluator(context, nullptr, false);
    ArrayOfVector retValues;
    Exception retException;
    try {
        retValues = fptr->evaluateFunction(eval, argIn, nLhs);
    } catch (Exception& e) {
        retException = e;
    }
    return std::make_tuple(retValues, retException);
}
//=============================================================================
FevalFutureObject *
BackgroundPoolObject::feval(FunctionDef* fptr, int nLhs, const ArrayOfVector& argIn)
{
    std::future<std::tuple<ArrayOfVector, Exception>> f
        = threadPool.submit(FunctionEvalInternal, fptr, nLhs, argIn);
    FevalFutureObject* retFuture = new FevalFutureObject(std::move(f), utf8_to_wstring(fptr->getName()));
    fEvalQueue.push_back(retFuture);
    return retFuture;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
