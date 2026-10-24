//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cstdarg>
#include "ArrayOf.hpp"
#include "Scope.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
struct ScopeStack
{
public:
    Scope* data;
    ScopeStack* next;
};
//=============================================================================
/**
 * A Context is a stack of scopes with the (peculiar) property that
 * the top and bottom of the stack (global and local scopes respectively)
 * are searched regularly.  The context is responsible for determining
 * if variables and functions exist, and if so, for returning their
 * values and accepting modifications to them.  A context also keeps
 * track of loop depth.
 */
class NLSINTERPRETER_IMPEXP Context
{
    std::vector<Scope*> scopestack;
    // scopes hidden by bypassScope()
    std::vector<Scope*> bypassstack;

    size_t currentrecursiondepth;
    size_t variablesClearedEpoch = 0;

private:
    std::string
    getMangledPrivateFunction(const std::wstring& directory, const std::string& functionName);

    bool
    isInMacroFile();

    bool
    lookupFunctionWithoutImports(const std::string& funcName, FunctionDefPtr& val);
    bool
    resolveImportedFunction(const std::string& funcName, FunctionDefPtr& val);
    bool
    resolveImportCandidate(const std::string& importSpec, FunctionDefPtr& val);

public:
    Context();
    ~Context();
    void
    pushScope(const std::string& name);

    void
    pushScope(const std::string& name, const std::wstring& fullfilename);

    /**
     * Pop the bottom scope off of the scope stack.  The scope is
     * deleted.
     * Throws an NelsonException if the global scope is popped.
     */
    void
    popScope();
    /**
     * Insert the given variable into the right scope - the global
     * scope if the array is in the global list, and mangled in
     * global list if it is persistent.
     */
    bool
    insertVariable(const std::string& varName, const ArrayOf& /*var*/);
    bool
    insertVariableWithoutNameValidation(const std::string& varName, const ArrayOf& /*var*/);
    /**
     * Insert a variable into the local scope - do not check the
     * global list.
     */
    void
    insertVariableLocally(const std::string& varName, const ArrayOf& /*var*/);
    /**
     * Remove a variable from the local scope only, without touching the
     * global/persistent redirection. Used to tear down a transient
     * caller-context sync snapshot.
     */
    void
    removeVariableLocally(const std::string& varName);
    /**
     * Return a pointer to the given variable.  The search
     * logic is:
     *   - If the variable is persistent in the current scope
     *     (at the bottom of the scope stack), mangle its name
     *     using the scope, and look for it in the global scope.
     *   - If the variable is global in the current scope (at the
     *     bottom of the scope stack, look for it in the global
     *     scope.
     *   - Look for the variable in the local scope.
     * If the variable is not found, an empty variable is constructed
     * with the given name, and inserted into the scope that was
     * searched.  A pointer to this newly created variable is returned.
     */
    ArrayOf*
    lookupVariable(const std::string& varName);
    bool
    lookupVariable(const std::string& varName, ArrayOf& var);
    bool
    lookupVariableLocally(const std::string& varName, ArrayOf& var);
    void insertMacroFunctionLocally(FunctionDefPtr /*f*/);
    void
    removeMacroFunctionLocally(const std::string& funcName);
    bool
    lookupFunction(const std::string& funcName, FunctionDefPtr& val, bool builtinOnly = false);
    bool
    lookupFunction(const std::wstring& wfuncName, FunctionDefPtr& val, bool builtinOnly = false);

    bool
    addImport(const std::string& importSpec);
    void
    clearImports();
    stringVector
    getImports();
    bool
    resolveImportedClassName(const std::string& className, std::string& resolvedClassName);

    /**
     * Add a persistent variable to the local stack.  This involves
     * two steps:
     *   - the name of the variable is added to the persistent variable list
     *     in the current scope.
     *   - the global scope is checked for the mangled name of the
     *     persistent variable.  If the variable does not exist in the
     *     global scope, then an empty variable is inserted.
     */
    void
    addPersistentVariable(const std::string& var);
    /**
     * Add a variable name into the global variable list of the current
     * scope.  If the variable does not exist in the global scope, an
     * empty variable is added.
     */
    void
    addGlobalVariable(const std::string& var);
    /**
     * Delete a variable if its defined.  Handles global and persistent
     * variables also.
     */
    void
    deleteVariable(const std::string& var);
    void
    notifyVariablesCleared()
    {
        ++variablesClearedEpoch;
    }
    size_t
    getVariablesClearedEpoch() const
    {
        return variablesClearedEpoch;
    }
    inline Scope*
    getGlobalScope()
    {
        return scopestack.empty() ? nullptr : scopestack.front();
    }
    inline Scope*
    getCurrentScope()
    {
        return scopestack.empty() ? nullptr : scopestack.back();
    }
    inline size_t
    getScopeStackSize()
    {
        return scopestack.size();
    }
    inline size_t
    getBypassedScopeCount()
    {
        return bypassstack.size();
    }
    void
    printMe();
    inline void
    enterLoop()
    {
        scopestack.back()->enterLoop();
    }
    inline void
    exitLoop()
    {
        scopestack.back()->exitLoop();
    }
    inline bool
    inLoop()
    {
        return scopestack.back()->inLoop();
    }
    bool
    isVariableGlobal(const std::string& varName);
    bool
    isVariablePersistent(const std::string& varName);

    void
    bypassScope(int count);

    void
    restoreBypassedScopes();

    void
    restoreBypassedScopes(int count);

    Scope*
    getCallerScope();
    Scope*
    getBaseScope();

    stringVector
    getLockedVariables();
    bool
    isLockedVariable(const std::string& varname);
    bool
    lockVariable(const std::string& varname);
    bool
    unlockVariable(const std::string& varname);

    bool
    isVariable(const std::string& varname);
    bool
    isVariable(const std::wstring& varname);

    size_t
    getRecursionDepth();
    bool
    setRecursionDepth(size_t newDepth);
    size_t
    maximumRecursionDepth();
};
//=============================================================================
enum SCOPE_LEVEL
{
    GLOBAL_SCOPE,
    BASE_SCOPE,
    CALLER_SCOPE,
    LOCAL_SCOPE
};
//=============================================================================
} // namespace Nelson
//=============================================================================
