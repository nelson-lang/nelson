//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <atomic>
#include <functional>
#include <unordered_map>
#include <vector>
#include "FunctionDef.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP FunctionsInMemory
{
private:
    //=============================================================================
    std::unordered_map<std::string, FunctionDefPtr> _macroFunctionsInMemory;
    std::unordered_map<std::string, FunctionDefPtr> _mexFunctionsInMemory;
    std::unordered_map<std::string, FunctionDefPtr> _builtinFunctionInMemory;
    //=============================================================================
    std::unordered_map<std::string, FunctionDefPtr> _lastFunctionsInMemory;
    //=============================================================================
    std::unordered_map<std::string, bool> _notExistingFunctionsInMemory;
    //=============================================================================
    // Definitions let go of by retireIfFileRemoved(), kept alive until the tables
    // are cleared.
    std::vector<FunctionDefPtr> _retiredFunctions;
    //=============================================================================
    // Incremented whenever the function tables change; lets callers cache
    // negative lookup results (e.g. missing overloads) safely.
    std::atomic<uint64> _generation { 0 };
    //=============================================================================
    // Cleanup callback installed by the optional 'mex' module to free the
    // globally registered persistent MEX memory. Empty when no MEX runtime is
    // loaded. See registerMexMemoryCleanup / freeRegisteredMexMemory.
    std::function<void()> _mexMemoryCleanup;
    //=============================================================================
    FunctionsInMemory();
    //=============================================================================
    ~FunctionsInMemory();
    //=============================================================================
    static FunctionsInMemory* m_pInstance;
    //=============================================================================
    bool
    findMex(const std::string& functionName, FunctionDefPtr& function);
    //=============================================================================
    bool
    findMacro(const std::string& functionName, FunctionDefPtr& function);
    //=============================================================================
    bool
    findBuiltin(const std::string& functionName, FunctionDefPtr& function);
    //=============================================================================
    void
    declareAsNotExistingFunction(const std::string& functionName);
    //=============================================================================
    void
    destroyRetiredFunctions();

public:
    //=============================================================================
    enum FIND_FUNCTION_TYPE
    {
        ALL,
        BUILTIN,
        MEX,
        MACRO
    };
    //=============================================================================
    static FunctionsInMemory*
    getInstance();
    //=============================================================================
    void
    destroy();
    //=============================================================================
    void
    add(const std::string& functionName, FunctionDefPtr function);
    //=============================================================================
    bool
    deleteMFunction(const std::string& functionName);
    //=============================================================================
    bool
    deleteMexFunction(const std::string& functionName);
    //=============================================================================
    // Drop cached references to a builtin (the BuiltInFunctionDefManager owns
    // and deletes the definition itself); required before deleting a builtin,
    // otherwise lookups follow a dangling pointer.
    bool
    deleteBuiltinFunctionReference(const std::string& functionName);
    //=============================================================================
    void
    deleteAllBuiltinFunctionReferences();
    //=============================================================================
    // Lets go of a name whose file has been removed, so that it stops resolving.
    // The definition is not destroyed here: a frame may be running it and the
    // interpreter caches hold raw pointers to it, so it is retired and destroyed
    // when the tables are cleared.
    bool
    retireIfFileRemoved(const std::string& functionName);
    //=============================================================================
    // Lets go of a name whose cached definition no longer matches the file it now
    // resolves to (current directory changed, or a reordered path), so the next
    // lookup reloads the right file. Retired like in retireIfFileRemoved().
    bool
    retireIfFileReplaced(const std::string& functionName, const std::wstring& resolvedFilename);
    //=============================================================================
    // Names (table keys) of the macro and MEX definitions currently held, used
    // to reconcile the tables when the set of resolvable files changes.
    stringVector
    getMacroAndMexNames();
    //=============================================================================
    void
    deleteAllMFunctions();
    //=============================================================================
    bool
    deleteAllMexFunctions();
    //=============================================================================
    // Solver-neutral seam for 'clear mex' / 'clear all': the optional 'mex'
    // module registers a callback (mexFreeAllRegisteredPointer) at load, and
    // the memory_manager builtin triggers it through here. This keeps the
    // always-installed memory_manager from linking the optional mex module,
    // which is absent from a math/headless install. No-op when no MEX runtime
    // registered a cleanup.
    void
    registerMexMemoryCleanup(std::function<void()> cleanup);
    //=============================================================================
    void
    freeRegisteredMexMemory();
    //=============================================================================
    bool
    find(const std::string& functionName, FunctionDefPtr& function,
        FIND_FUNCTION_TYPE functionType = FIND_FUNCTION_TYPE::ALL);
    //=============================================================================
    void
    clear(stringVector exceptedFunctions = stringVector());
    //=============================================================================
    void
    clearMapCache();
    //=============================================================================
    wstringVector
    getMacroInMemory(bool withCompleteNames);
    //=============================================================================
    wstringVector
    getMexInMemory(bool withCompleteNames);
    //=============================================================================
    bool
    isNotExistingFunction(const std::string& functionName);
    //=============================================================================
    uint64
    generation() const
    {
        return _generation.load(std::memory_order_relaxed);
    }
    //=============================================================================
    void
    bumpGeneration()
    {
        _generation.fetch_add(1, std::memory_order_relaxed);
    }
    //=============================================================================
};
//=============================================================================
class Context;
//=============================================================================
// Resolve a mangled overload name ("@class/function") through the context.
// Such names cannot be shadowed by scope-local functions or imports, so the
// resolution only depends on the function tables: successful lookups are
// memoized against their generation, skipping the per-call scope walk and
// file-watcher probes. Non-mangled names fall through uncached.
NLSINTERPRETER_IMPEXP bool
lookupOverloadFunctionCached(
    Context* context, const std::string& overloadName, FunctionDefPtr& function);
//=============================================================================
} // namespace Nelson
//=============================================================================
