//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
class NLSINTERPRETER_IMPEXP BuiltInFunctionDef : public FunctionDef
{
public:
    //=============================================================================
    /**
     * Location of the function's defining dynamic library.
     */
    std::wstring fileName;
    //=============================================================================
    /**
     * The number of return args for this function (-1 for variable).
     */
    int retCount;
    //=============================================================================
    /** The number of input args for this function (-1 for variable).
     */
    int argCount;
    //=============================================================================
    /**
     * The pointer to (address of) the function.
     */
    void* fptr;
    //=============================================================================
    /**
     * Type of builtin;
     */
    size_t builtinPrototype;
    //=============================================================================
    /**
     * separated or interleaved complex
     * Nelson uses natively interleaved complex representation
     * MEX before 2018 version uses separated complex representation
     */
    bool interleavedComplex;
    //=============================================================================
    /**
     * Default constructor.
     */
    BuiltInFunctionDef();
    //=============================================================================
    /**
     * Default destructor.
     */
    ~BuiltInFunctionDef() override;
    //=============================================================================
    /**
     * The type of the function is NLS_BUILT_IN_FUNCTION
     */
    FunctionType
    type() const override
    {
        return NLS_BUILT_IN_FUNCTION;
    }
    //=============================================================================
    /**
     * The number of inputs required by this function.
     */
    int
    inputArgCount() override
    {
        return argCount;
    }
    //=============================================================================
    /**
     * The number of outputs returned by this function.
     */
    int
    outputArgCount() override
    {
        return retCount;
    }
    //=============================================================================
    /**
     * Evaluate the function and return the values.
     */
    ArrayOfVector
    evaluateFunction(
        Evaluator* /*unused*/, const ArrayOfVector& /*unused*/, int /*unused*/) override;
    //=============================================================================
    bool
    updateCode() override
    {
        return false;
    }
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
