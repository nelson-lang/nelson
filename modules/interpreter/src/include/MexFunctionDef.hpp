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

    std::string debugDetailCache;

public:
    //=============================================================================
    // "built-in <name>" computed once: pushDebug runs on every mex call.
    const std::string&
    getDebugDetail()
    {
        if (debugDetailCache.empty()) {
            debugDetailCache = std::string("built-in ") + getName();
        }
        return debugDetailCache;
    }
    //=============================================================================
    MexFunctionDef(const std::wstring& filename, const std::wstring& name, bool isOverload);
    //=============================================================================
    ~MexFunctionDef() override;
    //=============================================================================
    [[nodiscard]] FunctionType
    type() const override
    {
        return NLS_MEX_FUNCTION;
    }
    //=============================================================================
    bool
    isLoaded()
    {
        return loaded;
    }
    //=============================================================================
    bool
    clear();
    //=============================================================================
    int
    inputArgCount() override
    {
        return INT_MIN;
    }
    //=============================================================================
    int
    outputArgCount() override
    {
        return INT_MIN;
    }
    //=============================================================================
    ArrayOfVector
    evaluateFunction(Evaluator* eval, const ArrayOfVector& inputs, int nargout) override;
    //=============================================================================
    bool
    isLocked();
    //=============================================================================
    bool
    updateCode() override;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
