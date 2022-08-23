//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
//=============================================================================
namespace Nelson {
//=============================================================================
class AnonymousMacroFunctionDef;
//=============================================================================
/**
 * Anonymous functions are a means of quickly defining small functions dynamically.
 */
//=============================================================================
class NLSINTERPRETER_IMPEXP AnonymousMacroFunctionDef : public FunctionDef
{
public:
    AbstractSyntaxTreePtr code;
    AbstractSyntaxTreePtrVector ptrAstCodeAsVector;

    AnonymousMacroFunctionDef(const std::string& anonymousContent);

    /**
     * The destructor
     */
    ~AnonymousMacroFunctionDef() override;
    /** The type of the function
     */
    [[nodiscard]] FunctionType
    type() const override
    {
        return Nelson::FunctionType::NLS_ANONYMOUS_MACRO_FUNCTION;
    }
    int
    inputArgCount() override;
    int
    outputArgCount() override;

    ArrayOfVector
    evaluateFunction(Evaluator* eval, const ArrayOfVector& arging, int nLhs) override;
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
    bool
    updateCode(int nLhs);
    //=============================================================================
    std::string
    getDefinition();
    //=============================================================================
    stringVector returnVals;
    //=============================================================================
private:
    std::string anonymousContent;
    std::string
    convertToStandardFunction(int nLhs = 1);
    int previousLhs = -1;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
