//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MacroFunctionDef.hpp"
#include <atomic>
#include <mutex>
#include <unordered_map>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::unordered_map<MacroFunctionDef*, std::shared_ptr<MacroFunctionDef>>&
transientScriptOwners();
//=============================================================================
void
registerTransientScriptOwner(const std::shared_ptr<MacroFunctionDef>& owner)
{
    if (owner != nullptr) {
        transientScriptOwners()[owner.get()] = owner;
    }
}
//=============================================================================
void
unregisterTransientScriptOwner(MacroFunctionDef* owner)
{
    if (owner != nullptr) {
        transientScriptOwners().erase(owner);
    }
}
//=============================================================================
std::shared_ptr<MacroFunctionDef>
lookupTransientScriptOwner(MacroFunctionDef* head)
{
    auto& owners = transientScriptOwners();
    auto it = owners.find(head);
    if (it == owners.end()) {
        return nullptr;
    }
    return it->second;
}
//=============================================================================
static std::unordered_map<MacroFunctionDef*, std::shared_ptr<MacroFunctionDef>>&
transientScriptOwners()
{
    // Per-thread: a transient owner runs, and every capture of its definitions
    // happens, on the same thread. Holding the shared_ptr here keeps the owner
    // alive while it runs so captures can take their own reference; once the run
    // ends and the owner is unregistered, only capturing handles keep it alive.
    static thread_local std::unordered_map<MacroFunctionDef*, std::shared_ptr<MacroFunctionDef>>
        owners;
    return owners;
}
//=============================================================================
namespace {
    //=============================================================================
    struct RetainedMacroState
    {
        // Function handles currently pointing at this definition.
        size_t retainCount = 0;
        // Set when the function tables asked for the file that owns this definition
        // to be freed while it was still retained: the last release of the chain
        // frees that owner (which frees this definition with it).
        MacroFunctionDef* droppedOwner = nullptr;
    };
    //=============================================================================
} // namespace
//=============================================================================
static std::mutex&
retainedMacrosMutex();
//=============================================================================
static std::unordered_map<MacroFunctionDef*, RetainedMacroState>&
retainedMacros();
//=============================================================================
static std::atomic<size_t>&
retainedMacrosCount();
//=============================================================================
// Whether any definition of the chain owned by `head` is still retained. Called
// with retainedMacrosMutex() held.
static bool
isChainRetained(const std::unordered_map<MacroFunctionDef*, RetainedMacroState>& macros,
    MacroFunctionDef* head);
//=============================================================================
void
retainMacroFunctionDef(MacroFunctionDef* definition)
{
    if (definition == nullptr) {
        return;
    }
    std::lock_guard<std::mutex> lock(retainedMacrosMutex());
    RetainedMacroState& state = retainedMacros()[definition];
    if (state.retainCount++ == 0) {
        retainedMacrosCount().fetch_add(1, std::memory_order_relaxed);
    }
}
//=============================================================================
void
releaseMacroFunctionDef(MacroFunctionDef* definition)
{
    if (definition == nullptr || retainedMacrosCount().load(std::memory_order_relaxed) == 0) {
        return;
    }
    MacroFunctionDef* toFree = nullptr;
    {
        std::lock_guard<std::mutex> lock(retainedMacrosMutex());
        auto& macros = retainedMacros();
        auto it = macros.find(definition);
        if (it == macros.end() || it->second.retainCount == 0) {
            return;
        }
        if (--it->second.retainCount != 0) {
            return;
        }
        MacroFunctionDef* droppedOwner = it->second.droppedOwner;
        macros.erase(it);
        retainedMacrosCount().fetch_sub(1, std::memory_order_relaxed);
        if (droppedOwner != nullptr && !isChainRetained(macros, droppedOwner)) {
            toFree = droppedOwner;
        }
    }
    // Freeing a definition destroys the values it holds, which may release
    // other retained definitions and re-enter here: free it unlocked.
    if (toFree != nullptr) {
        delete toFree;
    }
}
//=============================================================================
void
destroyMacroFunctionDef(MacroFunctionDef* head)
{
    if (head == nullptr) {
        return;
    }
    // Nothing retained anywhere is the common case: one relaxed load.
    if (retainedMacrosCount().load(std::memory_order_relaxed) != 0) {
        std::lock_guard<std::mutex> lock(retainedMacrosMutex());
        auto& macros = retainedMacros();
        bool deferred = false;
        // ~MacroFunctionDef frees the whole nextFunction chain, so a retain on
        // any of the local functions keeps the file alive.
        for (MacroFunctionDef* node = head; node != nullptr; node = node->nextFunction) {
            auto it = macros.find(node);
            if (it != macros.end() && it->second.retainCount != 0) {
                it->second.droppedOwner = head;
                deferred = true;
            }
        }
        if (deferred) {
            return;
        }
    }
    delete head;
}
//=============================================================================
static std::mutex&
retainedMacrosMutex()
{
    // Function handles can be released by another translation unit's static
    // teardown. Keep the registry alive for the process lifetime so that
    // cross-unit destruction order cannot invalidate its synchronization.
    static auto* mutex = new std::mutex;
    return *mutex;
}
//=============================================================================
// The definitions function handles keep alive, keyed by the exact definition a
// handle points at. Not per-thread: a handle may be created on the interpreter
// thread and released from another one.
static std::unordered_map<MacroFunctionDef*, RetainedMacroState>&
retainedMacros()
{
    static auto* macros = new std::unordered_map<MacroFunctionDef*, RetainedMacroState>;
    return *macros;
}
//=============================================================================
// Number of definitions currently retained. Zero unless a handle to a local
// function is alive, so the usual destroy path never takes the lock.
static std::atomic<size_t>&
retainedMacrosCount()
{
    static auto* count = new std::atomic<size_t> { 0 };
    return *count;
}
//=============================================================================
static bool
isChainRetained(
    const std::unordered_map<MacroFunctionDef*, RetainedMacroState>& macros, MacroFunctionDef* head)
{
    for (MacroFunctionDef* node = head; node != nullptr; node = node->nextFunction) {
        auto it = macros.find(node);
        if (it != macros.end() && it->second.retainCount != 0) {
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
