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
    MexFunctionDef(const std::wstring& filename, const std::wstring& name);
    //=============================================================================
    /**
     * Default destructor.
     */
    ~MexFunctionDef() override;
    //=============================================================================
    /**
     * The type of the function is NLS_BUILT_IN_FUNCTION
     */
    FunctionType
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
    evaluateFunction(Evaluator* eval, const ArrayOfVector& argIn, int nLhs) override;
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
