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
    BuiltInFunctionDef(bool isOverload);
    //=============================================================================
    /**
     * Default destructor.
     */
    ~BuiltInFunctionDef() override;
    //=============================================================================
    /**
     * The type of the function is NLS_BUILT_IN_FUNCTION
     */
    [[nodiscard]] FunctionType
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
