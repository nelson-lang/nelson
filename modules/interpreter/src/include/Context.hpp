//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
// Copyright (c) 2002, 2003 Samit Basu
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//=============================================================================
#pragma once
//=============================================================================
#include <cstdarg>
#include "ArrayOf.hpp"
#include "Scope.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
/**
 * This structure implements a linked list of Scope pointers.
 */
struct ScopeStack
{
public:
    Scope* data;
    ScopeStack* next;
};

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
    /**
     * The normal stack of scopes.
     */
    std::vector<Scope*> scopestack;
    /**
     * The stack of scopes that have been "bypassed"
     */
    std::vector<Scope*> bypassstack;

    size_t currentrecursiondepth;

public:
    /**
     * Create a context and initialize it with a global scope and a
     * base scope.
     */
    Context();
    /**
     * Delete the context
     */
    ~Context();
    /**
     * Push the given scope onto the bottom of the scope stack.
     */
    void pushScope(std::string);
    /**
     * Pop the bottom scope off of the scope stack.  The scope is
     * deleted.
     * Throws an Exception if the global scope is popped.
     */
    void
    popScope();
    /**
     * Insert the given variable into the right scope - the global
     * scope if the array is in the global list, and mangled in
     * global list if it is persistent.
     */
    bool
    insertVariable(const std::string& varName, const ArrayOf&);
    /**
     * Insert a variable into the local scope - do not check the
     * global list.
     */
    void
    insertVariableLocally(std::string varName, const ArrayOf&);
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
    bool
    lookupVariable(const std::string& varName, ArrayOf& var);
    /**
     * Look for a variable, but only locally.
     */
    bool
    lookupVariableLocally(std::string varName, ArrayOf& var);
    /**
     * Insert a function definition into the local scope (bottom of
     * the scope stack).
     */
    void insertMacroFunctionLocally(FuncPtr);
    /**
     * Add a built in function to the global scope with the given name.
     */
    bool
    lookupFunction(const std::string& funcName, FuncPtr& val, bool builtinOnly = false);
    bool
    lookupFunction(const std::wstring& wfuncName, FuncPtr& val, bool builtinOnly = false);

    bool
    lookupFunctionGlobally(const std::string& funcName, FuncPtr& val, bool builtinOnly = false);

    void
    deleteFunctionGlobally(const std::string& funcName);
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
    addPersistentVariable(std::string var);
    /**
     * Add a variable name into the global variable list of the current
     * scope.  If the variable does not exist in the global scope, an
     * empty variable is added.
     */
    void
    addGlobalVariable(std::string var);
    /**
     * Delete a variable if its defined.  Handles global and persistent
     * variables also.
     */
    void
    deleteVariable(std::string var);
    /**
     * Get the global scope off the top of the scope stack.
     */
    Scope*
    getGlobalScope();
    /**
     * Get the current (active) scope
     */
    Scope*
    getCurrentScope();
    /**
     * Print the context.
     */
    void
    printMe();
    /**
     * Increment the loop depth counter in the local scope.
     */
    void
    enterLoop();
    /**
     * Decrement the loop depth counter in the local scope.
     */
    void
    exitLoop();
    /**
     * Returns true if the current (local) scope indicates a
     * non-zero loop depth.
     */
    bool
    inLoop();
    /**
     * Returns true if the given variable is global.
     */
    bool
    isVariableGlobal(const std::string& varName);
    /**
     * Returns true if the given variable is persistent
     */
    bool
    isVariablePersistent(const std::string& varName);

    void
    bypassScope(int count);

    void
    restoreBypassedScopes();

    Scope*
    getCallerScope();
    Scope*
    getBaseScope();

    stringVector
    getLockedVariables();
    bool
    isLockedVariable(std::string varname);
    bool
    lockVariable(std::string varname);
    bool
    unlockVariable(std::string varname);

    bool
    isVariable(std::string varname);

    size_t
    getRecursionDepth();
    bool
    setRecursionDepth(size_t newDepth);
    size_t
    getMaximumRecursionDepth();
};

typedef enum
{
    GLOBAL_SCOPE,
    BASE_SCOPE,
    CALLER_SCOPE,
    LOCAL_SCOPE
} SCOPE_LEVEL;
} // namespace Nelson