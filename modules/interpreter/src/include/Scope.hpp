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
/**
 * A Scope is a combination of a variable hashtable and a function hashtable.
 */
//=============================================================================
#include <string>
#include <vector>
#include "ArrayOf.hpp"
#include "FunctionDef.hpp"
#include "VariablesTable.hpp"
#include "SymbolTable.hpp"
#include "LocalFunctionsTable.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * A Scope is a collection of functions and variables all visible
 * at some point in the execution.  The scope also keeps track of
 * the loop level, and a list of the global and persistent variables
 * relevant to the current scope.
 */
class NLSINTERPRETER_IMPEXP Scope
{
private:
    /**
     * This is the hash-table of ArrayOf pointers that forms the
     * symbol table.  Each variable has a name associated with
     * it that must be unique to the Scope.  The Scope owns the
     * variables in the symbol table, and is responsible for
     * destructing them when destroyed.
     */

    LocalFunctionsTable currentLocalFunctions;

    VariablesTable variablesTab;
    /**
     * This is a hash-table of function pointers.  The Scope does
     * _not_ own the function pointers, and it must not destroy
     * them when destructed.
     */
    // CodeTable macroTab;

    /**
     * The name of the scope.
     */
    std::string name;
    /**
     * The loop level.  This is used to track the depth of nested
     * loops.
     */
    int loopLevel;
    /**
     * These are the global variables as defined in the current
     * scope.  Global variables are not stored in this Scope, but
     * are deferred to the top scope in the Context.
     */
    stringVector globalVars;
    /**
     * Persistent variables are similar to global variables in that
     * they are deferred to the top scope in the Context.  However,
     * unlike global variables, persistent variables are mangled
     * with the name of the scope before being indexed into the global
     * scope.
     */
    stringVector persistentVars;
    /**
     * The location ID stack - stores information on where in the source
     * file the current token resides.
     */
    std::vector<int> IDstack;

    int nargin = 0;
    int nargout = 0;

public:
    /**
     * Construct a scope with the given name.
     */
    Scope(std::string scopeName);
    /**
     * Default destructor.
     */
    ~Scope();
    /**
     * Insert a variable with the given name.  If the variable
     * already exists in the Scope, then the previous definition
     * is replaced with the given one.
     */
    bool
    insertVariable(const std::string& varName, const ArrayOf& val);
    /**
     * Insert a function pointer into the current scope.  The name of
     * of the function is encoded in the FuncPtr.
     */
    void
    insertMacroFunctionLocally(FuncPtr a);

    /**
     * Delete a function from the current scope.
     */
    void
    deleteFunction(const std::string funcName);
    bool
    deleteBuiltin(void* fptr);

    /**
     * Lookup a function.  Return true if the function is defined, and
     * assigns the value of the function pointer to the second argument.
     */
    bool
    lookupFunction(std::string funcName, FuncPtr& val, bool builtinOnly = false);
    /**
     * Lookup a variable.  Return true if the variable is defined, and
     * assigns the value of the variable to the second argument.
     */
    bool
    lookupVariable(const std::string& funcName, ArrayOf& val);
    /**
     * Add a variable name to the global variables list.
     */
    void
    addGlobalVariablePointer(std::string varName);
    /**
     * Delete a variable name from the global variables list.
     */
    void
    deleteGlobalVariablePointer(std::string varName);
    /**
     * Check to see if a variable is globally defined.
     */
    bool
    isVariableGlobal(const std::string& varName);
    /**
     * Add a variable name to the persistent variables list.
     */
    void
    addPersistentVariablePointer(std::string varName);
    /**
     * Delete a variable name from the persistent variables list.
     */
    void
    deletePersistentVariablePointer(std::string varName);
    /**
     * Check to see if a variable is defined in the persistent
     * list.
     */
    bool
    isVariablePersistent(const std::string& varName);
    /**
     * Mangle the name of a variable by prepending
     * a "_scopename_" to the name of the variable.
     */
    std::string
    getMangledName(std::string varName);
    /**
     * Dump the scope.
     */
    void
    printMe();
    /**
     * Dump only the variables in the scope (not the functions).
     */
    void
    printData();
    /**
     * Get the name of the scope.
     */
    std::string
    getName();
    /**
     * Increment the loop counter.
     */
    void
    enterLoop();
    /**
     * Decrement the loop counter.
     */
    void
    exitLoop();
    /**
     * Test the loop counter.
     */
    bool
    inLoop();

    /* Get symbols list*/
    stringVector
    getVariablesList(bool withPersistent);
    stringVector
    getBuiltinsList();

    /**
     * Delete a variable in this scope.  It does not simply
     * replace the variable with an empty variable, but deletes
     * the variable from the symbol table completely.
     */
    bool
    deleteVariable(std::string var);

    bool
    isPointerOnFunction(FuncPtr val);

    void
    setNargIn(int _nargin);
    int
    getNargIn();

    void
    setNargOut(int _nargout);
    int
    getNargOut();

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
};
//=============================================================================
}
//=============================================================================
