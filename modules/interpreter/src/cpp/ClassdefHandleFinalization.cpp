//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <unordered_set>
#include <vector>
#include "ClassdefHandleFinalization.hpp"
#include "ClassdefHandleObject.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefParserConstants.hpp"
#include "ClassdefParserHelpers.hpp"
#include "ClassdefParserRegistry.hpp"
#include "Context.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FunctionDef.hpp"
#include "HandleManager.hpp"
#include "NelsonException.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// A destructor that keeps releasing objects can nest drains; beyond this depth
// the remaining entries wait for the outermost drain to pick them up.
#define CLASSDEF_FINALIZATION_MAX_NESTING 64
//=============================================================================
static void
collectUserDeleteChain(const std::string& className, std::vector<std::string>& chain,
    std::unordered_set<std::string>& visited);
static void
invokeUserDelete(Evaluator* eval, const std::string& functionName, nelson_handle id);
//=============================================================================
bool
RunClassdefDelete(Evaluator* eval, const HandleRef& ref, bool containErrors)
{
    if (eval == nullptr || !ref || !ref->isValid()) {
        return false;
    }
    auto* object = dynamic_cast<ClassdefHandleObject*>(ref->object());
    if (object == nullptr || object->deleteInvoked()) {
        return false;
    }
    object->markDeleteInvoked();
    const nelson_handle id = ref->id();
    const std::string className = object->category();
    auto report = [&](const NelsonException& e) {
        if (!containErrors || eval->getState() == NLS_STATE_ABORT
            || eval->isQuitOrForceQuitState()) {
            if (ref->isValid()) {
                ref->invalidate(eval);
            }
            throw;
        }
        Warning(L"Nelson:classdef:destructorError",
            L"The following error was caught while executing '%s' class destructor: %s",
            utf8_to_wstring(className), e.message());
    };
    // a failing listener does not prevent the delete methods from running
    try {
        object->notifyEvent(eval, id, CLASSDEF_EVENT_OBJECT_BEING_DESTROYED, ArrayOf::empty());
    } catch (const NelsonException& e) {
        report(e);
    }
    try {
        std::vector<std::string> chain;
        std::unordered_set<std::string> visited;
        collectUserDeleteChain(className, chain, visited);
        for (const std::string& functionName : chain) {
            if (!ref->isValid()) {
                break;
            }
            invokeUserDelete(eval, functionName, id);
        }
    } catch (const NelsonException& e) {
        report(e);
    }
    if (ref->isValid()) {
        ref->invalidate(eval);
    }
    return true;
}
//=============================================================================
void
DrainPendingHandleFinalizers(Evaluator* eval)
{
    if (eval == nullptr || eval->getContext() == nullptr) {
        return;
    }
    static thread_local int nesting = 0;
    if (nesting >= CLASSDEF_FINALIZATION_MAX_NESTING) {
        return;
    }
    struct NestingGuard
    {
        int& depth;
        explicit NestingGuard(int& d) : depth(d) { ++depth; }
        ~NestingGuard() { --depth; }
        NestingGuard(const NestingGuard&) = delete;
        NestingGuard&
        operator=(const NestingGuard&)
            = delete;
    };
    NestingGuard guard(nesting);
    // Objects released together (a frame's locals, a cleared workspace, an
    // object array) are finalized one after the other in release order: the
    // whole batch is taken at once so that the nested safe points inside a
    // destructor body only see objects released by that destructor.
    struct Batch
    {
        std::vector<PendingHandleFinalizers::Entry> entries;
        size_t next = 0;
        ~Batch()
        {
            if (next < entries.size()) {
                // interrupted (error or abort): keep the remaining objects
                // pending instead of leaking them
                PendingHandleFinalizers::requeueFront(std::vector<PendingHandleFinalizers::Entry>(
                    entries.begin() + static_cast<std::ptrdiff_t>(next), entries.end()));
            }
        }
    };
    Batch batch;
    while (PendingHandleFinalizers::takeAll(batch.entries)) {
        batch.next = 0;
        while (batch.next < batch.entries.size()) {
            const PendingHandleFinalizers::Entry entry = batch.entries[batch.next];
            ++batch.next;
            // The object is registered again under its former id so that the
            // user delete addresses it through a valid handle; the state is
            // dropped (and the object destroyed) once the protocol completed.
            HandleRef ref = HandleManager::getInstance()->adoptHandleRef(entry.first, entry.second);
            if (!ref) {
                delete entry.second;
                continue;
            }
            RunClassdefDelete(eval, ref, true);
            if (ref->isValid()) {
                ref->invalidate(eval);
            }
        }
    }
}
//=============================================================================
static void
collectUserDeleteChain(const std::string& className, std::vector<std::string>& chain,
    std::unordered_set<std::string>& visited)
{
    if (className.empty() || !visited.insert(className).second) {
        return;
    }
    const ClassdefDefinition* definition = classdef_detail::findClassdefDefinition(className);
    if (definition == nullptr) {
        return;
    }
    // only the class's own delete: superclass ones follow, most derived first
    for (const auto& method : definition->methods) {
        if (!method.isStatic
            && classdef_detail::equalsIgnoreCase(method.name, CLASSDEF_FUNCTION_DELETE)) {
            chain.push_back(CLASSDEF_GENERATED_FUNCTION_PREFIX + className + "_"
                + CLASSDEF_FUNCTION_DELETE + CLASSDEF_GENERATED_FUNCTION_SUFFIX);
            break;
        }
    }
    for (const std::string& superclass : definition->superclasses) {
        collectUserDeleteChain(superclass, chain, visited);
    }
}
//=============================================================================
static void
invokeUserDelete(Evaluator* eval, const std::string& functionName, nelson_handle id)
{
    FunctionDef* funcDef = nullptr;
    if (!eval->getContext()->lookupFunction(functionName, funcDef) || funcDef == nullptr) {
        return;
    }
    if (funcDef->type() != NLS_MACRO_FUNCTION && funcDef->type() != NLS_BUILT_IN_FUNCTION) {
        return;
    }
    ArrayOfVector args;
    args << ArrayOf::handle(id);
    funcDef->evaluateFunction(eval, args, 0);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
