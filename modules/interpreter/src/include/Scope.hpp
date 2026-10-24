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
 * Variables and functions visible at one point of the execution, with the
 * loop level and the global and persistent variable names of that scope.
 */
class NLSINTERPRETER_IMPEXP Scope
{
private:
    stringVector inputNames;
    int nargin = 0;
    int nargout = 0;

    LocalFunctionsTable currentLocalFunctions;

    // The Scope owns the variables of this table.
    VariablesTable variablesTab;
    // CodeTable macroTab;

    std::string name;
    // depth of nested loops
    int loopLevel;
    // Global variables are not stored in this Scope, they live in the top scope of the Context.
    std::unordered_set<std::string> globalVars;
    // Like globals, but the name is mangled with the scope name before it is indexed into the
    // global scope.
    std::unordered_set<std::string> persistentVars;
    // location of the current token in the source file
    std::vector<int> IDstack;

    std::wstring filename;
    std::wstring directory;
    bool isInMacro;
    stringVector importList;

public:
    Scope(const Scope& scopeOrigin);

    Scope(const std::string& scopeName);

    Scope(const std::string& scopeName, const std::wstring& fullfilename);

    ~Scope();
    // An existing variable with the same name is replaced.
    bool
    insertVariable(const std::string& varName, const ArrayOf& var);
    bool
    insertVariableWithoutNameValidation(const std::string& varName, const ArrayOf& var);
    void
    insertMacroFunctionLocally(FunctionDefPtr a);
    void
    removeMacroFunctionLocally(const std::string& funcName);

    // Returns true and sets val when the function is defined.
    bool
    lookupFunction(const std::string& funcName, FunctionDefPtr& val);
    std::vector<std::pair<std::string, FunctionDefPtr>>
    getLocalFunctionsList();
    // Returns true and sets val when the variable is defined.
    bool
    lookupVariable(const std::string& varName, ArrayOf& val);
    ArrayOf*
    lookupVariable(const std::string& varName);
    void
    addGlobalVariablePointer(const std::string& varName);
    void
    deleteGlobalVariablePointer(const std::string& varName);
    bool
    isVariableGlobal(const std::string& varName);
    void
    addPersistentVariablePointer(const std::string& varName);
    void
    deletePersistentVariablePointer(const std::string& varName);
    bool
    isVariablePersistent(const std::string& varName);
    // Prepends "_scopename_" to the variable name.
    std::string
    getMangledName(const std::string& varName);
    void
    printMe();
    void
    printData();
    inline std::string
    getName()
    {
        return name;
    }
    inline bool
    inLoop()
    {
        return (loopLevel > 0);
    }

    inline void
    enterLoop()
    {
        loopLevel++;
    }
    inline void
    exitLoop()
    {
        loopLevel--;
    }

    void
    getVariablesList(bool withPersistent, stringVector& list);

    void
    getVariablesList(bool withPersistent, wstringVector& list);

    // Removes the variable from the symbol table, it is not replaced by an empty one.
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

    bool
    addImport(const std::string& importSpec);
    void
    clearImports();
    // Used right before the scope dies.
    void
    clearVariables();
    stringVector
    getImports();
    // The task is owned by the function call this scope belongs to.
    void
    addCleanupTask(const ArrayOf& task);
    // The scope keeps no task afterwards.
    ArrayOfVector
    takeCleanupTasks();

private:
    // Declared last: other modules access the members above through inline
    // accessors, their offsets must not move.
    ArrayOfVector cleanupTasks;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
