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
/**
 * A Scope is a combination of a variable hashtable and a function hashtable.
 */
//=============================================================================
#include <string>
#include <vector>
#include <unordered_set>
#include "ArrayOf.hpp"
#include "FunctionDef.hpp"
#include "VariablesTable.hpp"
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
    stringVector inputNames;
    int nargin = 0;
    int nargout = 0;

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
    std::unordered_set<std::string> globalVars;
    /**
     * Persistent variables are similar to global variables in that
     * they are deferred to the top scope in the Context.  However,
     * unlike global variables, persistent variables are mangled
     * with the name of the scope before being indexed into the global
     * scope.
     */
    std::unordered_set<std::string>
        persistentVars; /**
                         * The location ID stack - stores information on where in the source
                         * file the current token resides.
                         */
    std::vector<int> IDstack;

    std::wstring filename;
    std::wstring directory;
    bool isInMacro;

public:
    /**
     * Construct a scope with the given name.
     */
    Scope(const Scope& scopeOrigin);

    Scope(const std::string& scopeName);

    Scope(const std::string& scopeName, const std::wstring& fullfilename);

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
    insertVariable(const std::string& varName, const ArrayOf& var);
    /**
     * Insert a function pointer into the current scope.  The name of
     * of the function is encoded in the FuncPtr.
     */
    void
    insertMacroFunctionLocally(FunctionDefPtr a);

    /**
     * Lookup a function.  Return true if the function is defined, and
     * assigns the value of the function pointer to the second argument.
     */
    bool
    lookupFunction(const std::string& funcName, FunctionDefPtr& val);
    /**
     * Lookup a variable.  Return true if the variable is defined, and
     * assigns the value of the variable to the second argument.
     */
    bool
    lookupVariable(const std::string& varName, ArrayOf& val);
    ArrayOf*
    lookupVariable(const std::string& varName);
    /**
     * Add a variable name to the global variables list.
     */
    void
    addGlobalVariablePointer(const std::string& varName);
    /**
     * Delete a variable name from the global variables list.
     */
    void
    deleteGlobalVariablePointer(const std::string& varName);
    /**
     * Check to see if a variable is globally defined.
     */
    bool
    isVariableGlobal(const std::string& varName);
    /**
     * Add a variable name to the persistent variables list.
     */
    void
    addPersistentVariablePointer(const std::string& varName);
    /**
     * Delete a variable name from the persistent variables list.
     */
    void
    deletePersistentVariablePointer(const std::string& varName);
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
    getMangledName(const std::string& varName);
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
    void
    getVariablesList(bool withPersistent, stringVector& list);

    void
    getVariablesList(bool withPersistent, wstringVector& list);

    /**
     * Delete a variable in this scope.  It does not simply
     * replace the variable with an empty variable, but deletes
     * the variable from the symbol table completely.
     */
    bool
    deleteVariable(const std::string& var);

    void
    setInputArgumentNames(const stringVector& names);
    stringVector
    getInputArgumentNames();

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
    isLockedVariable(const std::string& varname);
    bool
    lockVariable(const std::string& varname);
    bool
    unlockVariable(const std::string& varname);

    bool
    isVariable(const std::string& varname);

    std::wstring
    getFilename();
    std::wstring
    getDirectory();

    bool
    isInMacroFile();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
