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
#include <memory>
#include <mutex>
#include <string>
#include <utility>
#include <vector>
#include "AbstractSyntaxTree.hpp"
#include "FunctionDef.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class BytecodeChunk;
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
    std::unique_ptr<BytecodeChunk> bytecodeChunk;
    //=============================================================================
    AnonymousMacroFunctionDef(const std::string& functionHandle);
    //=============================================================================
    AnonymousMacroFunctionDef(const std::string& functionHandle, FunctionDef* boundFunction,
        const stringVector& variableNames, const std::vector<ArrayOf>& variables);
    //=============================================================================
    AnonymousMacroFunctionDef(const std::string& anonymousContent, const stringVector& arguments,
        const stringVector& variableNames, const std::vector<ArrayOf>& variables);
    //=============================================================================
    AnonymousMacroFunctionDef(const std::string& anonymousContent, const stringVector& arguments,
        const stringVector& variableNames, const std::vector<ArrayOf>& variables,
        const std::wstring& sourceFilename,
        const std::vector<std::pair<std::string, FunctionDefPtr>>& localFunctions);
    //=============================================================================
    ~AnonymousMacroFunctionDef() override;
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
    void
    invalidateBytecode();
    //=============================================================================
    bool
    ensureBytecodeCompiled(int nLhs);
    //=============================================================================
    /**
     * Return the compiled scope-free chunk when this anonymous function can be
     * executed directly by the VM for `inputCount` inputs (no captured
     * variables, no varargin, chunk free of scope-dependent opcodes: such a
     * chunk cannot invalidate itself, so callers may run it in a loop).
     * Returns nullptr when the generic evaluateFunction path is required.
     */
    BytecodeChunk*
    scopeFreeBytecode(size_t inputCount, int nLhs);
    //=============================================================================
    std::string
    getDefinition();
    //=============================================================================
    /**
     * Body of an anonymous function as written (@(x)x+1), used by getDefinition
     * in place of the re-parsed content when known.
     */
    void
    setDefinitionText(const std::string& text);
    //=============================================================================
    std::string
    getDefinitionText();
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
    bool
    isBoundNestedFunctionHandle() const;
    //=============================================================================
    // Input/output argument count of the bound (named/nested) function a
    // function-handle-only wraps, or -1 when there is no bound function. Lets
    // nargin/nargout introspect a handle to a nested function that is not
    // resolvable by name from the caller's scope.
    int
    boundFunctionNargin();
    int
    boundFunctionNargout();
    //=============================================================================
    void
    syncCapturedVariables(const stringVector& names, const std::vector<ArrayOf>& values);
    //=============================================================================
    bool
    isStatelessSimpleIdentity() const;
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
    FunctionDef* boundFunction = nullptr;
    std::string anonymousContent;
    // named handle: the function name; anonymous handle: its body as written
    // (setDefinitionText), empty when unknown
    std::string functionHandleContent;
    std::wstring sourceFilename;
    stringVector variableNames;
    std::vector<ArrayOf> variables;
    std::vector<std::pair<std::string, FunctionDefPtr>> localFunctions;
    std::recursive_mutex functionMutex;
    std::string
    convertToStandardFunction(int nLhs = 1);
    int previousLhs = -1;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
