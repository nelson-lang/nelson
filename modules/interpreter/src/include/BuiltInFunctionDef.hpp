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
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "FunctionDef.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
typedef ArrayOfVector (*BuiltInFuncPtr)(Evaluator*, int, const ArrayOfVector&);
//=============================================================================
class NLSINTERPRETER_IMPEXP BuiltInFunctionDef : public FunctionDef
{
public:
    /**
     * Location of the function's defining dynamic library.
     */
    std::wstring fileName;
    /**
     * The number of return args for this function (-1 for variable).
     */
    int retCount;
    /** The number of input args for this function (-1 for variable).
     */
    int argCount;
    /**
     * The pointer to (address of) the function.
     */
    BuiltInFuncPtr fptr;
    /**
     * Default constructor.
     */
    BuiltInFunctionDef();
    /**
     * Default destructor.
     */
    ~BuiltInFunctionDef();
    /**
     * The type of the function is NLS_BUILT_IN_FUNCTION
     */
    virtual const FunctionType
    type()
    {
        return NLS_BUILT_IN_FUNCTION;
    }
    /** Print a description of the function
     */
    virtual void
    printMe(Interface*);
    /**
     * The number of inputs required by this function.
     */
    virtual int
    inputArgCount()
    {
        return argCount;
    }
    /**
     * The number of outputs returned by this function.
     */
    virtual int
    outputArgCount()
    {
        return retCount;
    }
    /**
     * Evaluate the function and return the values.
     */

    virtual ArrayOfVector
    evaluateFunction(Evaluator*, ArrayOfVector&, int);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
