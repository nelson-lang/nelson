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
#include <vector>
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
    //=============================================================================
    AnonymousMacroFunctionDef(const std::string& functionHandle);
    //=============================================================================
    AnonymousMacroFunctionDef(const std::string& anonymousContent, const stringVector& arguments,
        const stringVector& variableNames, const std::vector<ArrayOf>& variables);
    //=============================================================================
    /**
     * The destructor
     */
    ~AnonymousMacroFunctionDef() override;
    //=============================================================================
    /** The type of the function
     */
    //=============================================================================
    [[nodiscard]] FunctionType
    type() const override
    {
        return Nelson::FunctionType::NLS_ANONYMOUS_MACRO_FUNCTION;
    }
    //=============================================================================
    int
    inputArgCount() override;
    //=============================================================================
    int
    outputArgCount() override;
    //=============================================================================
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
    stringVector
    getVariableNames();
    //=============================================================================
    std::vector<ArrayOf>
    getVariables();
    //=============================================================================
    stringVector returnVals;
    //=============================================================================
    stringVector
    getArguments()
    {
        return arguments;
    }
    //=============================================================================
    bool
    isFunctionHandle()
    {
        return isFunctionHandleOnly;
    }
    //=============================================================================
    std::string
    getContent()
    {
        if (isFunctionHandleOnly) {
            return functionHandleContent;
        }
        return anonymousContent;
    }
    //=============================================================================
    std::string
    toString()
    {
        if (isFunctionHandleOnly) {
            return functionHandleContent;
        }
        return getDefinition();
    }
    //=============================================================================

private:
    bool isFunctionHandleOnly = false;
    std::string anonymousContent;
    std::string functionHandleContent;
    stringVector variableNames;
    std::vector<ArrayOf> variables;
    std::string
    convertToStandardFunction(int nLhs = 1);
    int previousLhs = -1;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
