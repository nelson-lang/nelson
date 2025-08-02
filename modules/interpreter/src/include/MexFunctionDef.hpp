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
#include <climits>
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "FunctionDef.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP MexFunctionDef : public FunctionDef
{
private:
    /**
     * The pointers to (address of) the function.
     */
    void* libraryPtr;
    void* mexFunctionPtr;
    void* mexClearAtExitFunctionPtr;
    void* mexIsLockedPtr;
    /**
     * separated or interleaved complex
     * Nelson uses natively interleaved complex representation
     * MEX before 2018 version uses separated complex representation
     */
    bool interleavedComplex;

    bool loaded;

public:
    //=============================================================================
    /**
     * Default constructor.
     */
    MexFunctionDef(const std::wstring& filename, const std::wstring& name, bool isOverload);
    //=============================================================================
    /**
     * Default destructor.
     */
    ~MexFunctionDef() override;
    //=============================================================================
    /**
     * The type of the function is NLS_BUILT_IN_FUNCTION
     */
    [[nodiscard]] FunctionType
    type() const override
    {
        return NLS_MEX_FUNCTION;
    }
    //=============================================================================
    /**
     * checks if mex is correctly loaded
     */
    bool
    isLoaded()
    {
        return loaded;
    }
    //=============================================================================
    /**
     * clear mex
     */
    bool
    clear();
    //=============================================================================
    /**
     * The number of inputs required by this function.
     */
    int
    inputArgCount() override
    {
        return INT_MIN;
    }
    //=============================================================================
    /**
     * The number of outputs returned by this function.
     */
    int
    outputArgCount() override
    {
        return INT_MIN;
    }
    //=============================================================================
    /**
     * Evaluate the function and return the values.
     */
    ArrayOfVector
    evaluateFunction(Evaluator* eval, const ArrayOfVector& inputs, int nargout) override;
    //=============================================================================
    /**
     * Check if mex is locked
     */
    bool
    isLocked();
    //=============================================================================
    bool
    updateCode() override;
    //=============================================================================
};
//=============================================================================
}
// namespace Nelson
//=============================================================================
