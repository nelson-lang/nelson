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
#pragma once
//=============================================================================
#include "AST.hpp"
#include "FunctionDef.hpp"
#include "Serialize.hpp"
#include "nlsInterpreter_exports.h"
#include <sys/stat.h>
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
    ASTPtr code;
    /**
     * Location of the function's defining file in the current filesystem.
     */
    std::wstring fileName;

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
     * The constructor.
     */
    MacroFunctionDef();
    /**
     * The destructor
     */
    ~MacroFunctionDef();
    /** The type of the function
     */
    virtual const FunctionType
    type()
    {
        return NLS_MACRO_FUNCTION;
    }
    /** Print a description of the function
     */
    virtual void
    printMe(Interface* io);
    /**
     * The number of inputs required by this function, which is the number of
     * elements in arguments unless the last element is the keyword "varargin"
     * in which case the answer is -1.
     */
    virtual int
    inputArgCount();
    /**
     * The number of outputs returned by this function, which is the number of
     * elements in returnVals unless the last element is the keyword "varargout"
     * in which case the answer is -1.
     */
    virtual int
    outputArgCount();
    /**
     * Evaluate the function and return the outputs.
     * Throws an Exception if
     *   - the special variable 'varargout' is not defined in the body of the
     *     of the function as promised by the function declaration.
     *   - the variable 'varargout' contains too few elements to satisfy the
     *     number of return values in the call
     *   - the variable 'varargout' is the wrong type.
     */
    virtual ArrayOfVector
    evaluateFunction(Evaluator*, ArrayOfVector&, int);

    std::vector<ASTPtr> ptAst;

    int
    nargin();
    int
    nargout();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
