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
#include <string>
#include "AbstractSyntaxTree.hpp"
#include "FunctionDef.hpp"
#include "nlsInterpreter_exports.h"
#include "LexerContext.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class MacroFunctionDef;
//=============================================================================
/**
 * An MacroFunctionDef is a FunctionDef for an interpreted function.  The
 * function pointer stores the name of the file where the function is
 * located. The number of input and output arguments are computed
 * based on the contents of the returnVals and arguments stringVectors.
 */
//=============================================================================
class NLSINTERPRETER_IMPEXP MacroFunctionDef : public FunctionDef
{
public:
    /**
     * The names of the return values - this is a vector of strings with
     * one entry for each return value in the declared function.  Thus,
     * if the function is declared as "function [a,b] = foo(x)", then
     * returnVals contains two entries: "a", and "b".  For variable
     * return functions, the last entry should be "varargout".
     */
    stringVector returnVals;
    /**
     * The AST for the code that defines the function (only the body of the
     * function is contained in this AST, not the function declaration itself).
     */
    AbstractSyntaxTreePtr code;
    AbstractSyntaxTreePtrVector ptrAstCodeAsVector;

    /**
     * For some function files, there are multiple functions defined in
     * a single file.  The subsequent functions are local to the function
     * scope of the main function, and override global functions inside
     * the body of the current function (they are essentially hidden inside
     * the scope of the current function).  These functions are parsed
     * and form a linked list of function definitions, with the main function
     * at the head of the list.
     */
    MacroFunctionDef* nextFunction;
    MacroFunctionDef* prevFunction;
    /**
     * Set to true for all of the localFunctions.  False for the head of the
     * linked list.
     */
    bool localFunction;
    /**
     * constructors.
     */
    MacroFunctionDef();
    MacroFunctionDef(const std::wstring& filename, bool withWatcher, bool isOverload);

    /**
     * The destructor
     */
    ~MacroFunctionDef() override;
    /** The type of the function
     */
    [[nodiscard]] FunctionType
    type() const override
    {
        return Nelson::FunctionType::NLS_MACRO_FUNCTION;
    }
    /**
     * The number of inputs required by this function, which is the number of
     * elements in arguments unless the last element is the keyword "varargin"
     * in which case the answer is -1.
     */
    int
    inputArgCount() override;
    /**
     * The number of outputs returned by this function, which is the number of
     * elements in returnVals unless the last element is the keyword "varargout"
     * in which case the answer is -1.
     */
    int
    outputArgCount() override;
    /**
     * Evaluate the function and return the outputs.
     * Throws an Exception if
     *   - the special variable 'varargout' is not defined in the body of the
     *     of the function as promised by the function declaration.
     *   - the variable 'varargout' contains too few elements to satisfy the
     *     number of return values in the call
     *   - the variable 'varargout' is the wrong type.
     */
    ArrayOfVector
    evaluateFunction(
        Evaluator* /*eval*/, const ArrayOfVector& /*inputs*/, int /*nargout*/) override;
    //=============================================================================
    int
    nargin();
    //=============================================================================
    int
    nargout();
    //=============================================================================
    bool
    updateCode() override;
    //=============================================================================
    void
    setIsScript(bool _isScript)
    {
        isScript = _isScript;
    }
    //=============================================================================
    bool
    getIsScript()
    {
        return isScript;
    }
    //=============================================================================
    bool
    getWithWatcher()
    {
        return this->withWatcher;
    }
    //=============================================================================
    void
    addCleanupFunction(ArrayOf& task)
    {
        cleanupTasks.push_back(task);
    }
    //=============================================================================
private:
    std::string
    getCompleteName();

    ArrayOfVector
    evaluateMFunction(Evaluator* eval, const ArrayOfVector& inputs, int nargout);

    ArrayOfVector
    evaluateMScript(Evaluator* eval, const ArrayOfVector& inputs, int nargout);

    ArrayOfVector cleanupTasks;
    void
    onCleanup(Evaluator* eval);

    bool isScript;
    bool withWatcher;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
