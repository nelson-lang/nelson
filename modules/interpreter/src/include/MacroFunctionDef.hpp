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
#include <string>
#include "AbstractSyntaxTree.hpp"
#include "FunctionDef.hpp"
#include "nlsInterpreter_exports.h"
#include "LexerContext.hpp"
#include "Context.hpp"
#include "ParserState.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class BytecodeChunk;
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
    AbstractSyntaxTreePtr code;
    AbstractSyntaxTreePtrVector ptrAstCodeAsVector;
    std::unique_ptr<BytecodeChunk> bytecodeChunk;
    std::string classdefOwnerClassName;
    // Constructor with a non-public Access attribute: calls are checked
    // against the caller classdef context before entering the function.
    bool classdefConstructorRestricted = false;
    // True for the generated class constructor. Used to mark the owning class
    // as "under construction" while it runs, so immutable (SetAccess) property
    // assignments are allowed during construction and rejected afterwards.
    bool classdefConstructor = false;
    // Per-type overload probe miss cache, valid while the function tables
    // generation matches (see FunctionsInMemory::generation()).
    uint64 overloadProbeGeneration = 0;
    uint32 overloadProbeMissMask = 0;

    /**
     * A function file can define several functions. The ones after the main
     * function are local to its scope and override global functions inside its
     * body. They form a linked list with the main function at the head.
     */
    MacroFunctionDef* nextFunction;
    MacroFunctionDef* prevFunction;
    /**
     * Set to true for all of the localFunctions.  False for the head of the
     * linked list.
     */
    bool localFunction;
    bool nestedFunction;
    MacroFunctionDef();
    MacroFunctionDef(
        const std::wstring& filename, bool withWatcher, bool isOverload, bool directScript = false);

    ~MacroFunctionDef() override;
    [[nodiscard]] FunctionType
    type() const override
    {
        return Nelson::FunctionType::NLS_MACRO_FUNCTION;
    }
    /**
     * The number of inputs required by this function, which is the number of
     * elements in arguments unless the last element is the keyword "varargin"
     * in which case the answer is -1.
     */
    int
    inputArgCount() override;
    /**
     * The number of outputs returned by this function, which is the number of
     * elements in returnVals unless the last element is the keyword "varargout"
     * in which case the answer is -1.
     */
    int
    outputArgCount() override;
    /**
     * Evaluate the function and return the outputs.
     * Throws an NelsonException if
     *   - the special variable 'varargout' is not defined in the body of the
     *     of the function as promised by the function declaration.
     *   - the variable 'varargout' contains too few elements to satisfy the
     *     number of return values in the call
     *   - the variable 'varargout' is the wrong type.
     */
    ArrayOfVector
    evaluateFunction(
        Evaluator* /*eval*/, const ArrayOfVector& /*inputs*/, int /*nargout*/) override;
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
    void
    invalidateBytecode();
    //=============================================================================
    bool
    ensureBytecodeCompiled();
    //=============================================================================
    void
    setIsScript(bool _isScript)
    {
        isScript = _isScript;
    }
    //=============================================================================
    bool
    getIsScript()
    {
        return isScript;
    }
    //=============================================================================
    bool
    getWithWatcher()
    {
        return this->withWatcher;
    }
    //=============================================================================
    // True when this function can run without a context scope of its own
    // (bytecode gate, cached per FunctionsInMemory generation).
    bool
    canEvaluateScopeFree(Evaluator* eval);
    //=============================================================================
    const std::string&
    getCompleteName();
    //=============================================================================
private:
    struct LocalFunctionBinding
    {
        std::string name;
        FunctionDefPtr previousFunction = nullptr;
        bool restorePreviousFunction = false;
    };

    std::string
    computeCompleteName();

    std::string completeNameCache;
    int needsContextScopeCache = -1;
    // Generation of FunctionsInMemory when needsContextScopeCache was computed:
    // the scope-free decision resolves callee names, so table mutations
    // invalidate it.
    uint64 needsContextScopeGeneration = 0;

    ArrayOfVector
    evaluateMFunction(Evaluator* eval, const ArrayOfVector& inputs, int nargout);

    ArrayOfVector
    evaluateMScript(Evaluator* eval, const ArrayOfVector& inputs, int nargout);

    std::vector<LocalFunctionBinding>
    insertLocalFunctions(Context* context);

    void
    restoreLocalFunctions(
        Context* context, const std::vector<LocalFunctionBinding>& localFunctionBindings);

    void
    setInputArgumentNames(Context* context, const ArrayOfVector& inputs);

    void
    bindInputs(Context* context, const ArrayOfVector& inputs);

    ArrayOfVector
    prepareOutputs(Context* context, int nargout);

    void
    resetCodeStorage();

    FILE*
    openSourceFile();

    ParserState
    parseSource(Evaluator* eval, FILE* fr, AbstractSyntaxTreePtrVector& ptAstCode);

    ParserState
    parseOverloadEndFunction(Evaluator* eval, FILE* fr, AbstractSyntaxTreePtrVector& ptAstCode);

    ParserState
    parseRegularFile(Evaluator* eval, FILE* fr, AbstractSyntaxTreePtrVector& ptAstCode);

    LexerContext&
    getOrCreateLexerContext(Evaluator* eval, LexerContext& localContext);

    void
    assignParsedResult(ParserState pstate);

    void
    validateFunctionNamesAndFilename();

    bool isScript;
    bool directScript;
    bool withWatcher;
};
//=============================================================================
// Transient-owner keep-alive registry.
//
// A MacroFunctionDef created only to run (typically a script executed through
// run()/EvaluateScriptFile) frees its local/nested function definitions, with
// its nextFunction chain, when it is destroyed. Function handles created while
// it runs may capture those definitions by raw pointer and outlive it, which
// would leave them dangling.
//
// A transient owner registers its shared_ptr here (keyed by the chain head) for
// the duration of its run. Code that captures one of its local function
// definitions looks the head up and takes a shared reference. Definitions owned
// by the function tables are kept alive by the retain registry below. The
// registry is per-thread: the owner runs, and every capture happens, on the
// same thread.
NLSINTERPRETER_IMPEXP void
registerTransientScriptOwner(const std::shared_ptr<MacroFunctionDef>& owner);
//=============================================================================
NLSINTERPRETER_IMPEXP void
unregisterTransientScriptOwner(MacroFunctionDef* owner);
//=============================================================================
// Registered shared owner whose chain head is `head`, or nullptr.
NLSINTERPRETER_IMPEXP std::shared_ptr<MacroFunctionDef>
lookupTransientScriptOwner(MacroFunctionDef* head);
//=============================================================================
// Retain registry for definitions owned by the function tables.
//
// FunctionsInMemory owns path function definitions by raw pointer and frees them,
// with their nextFunction chain, on `clear functions`, on `clear <name>` and when
// the file watcher reloads a modified file. A handle to a local or nested
// function holds its definition by raw pointer and can outlive the table entry
// (callback in a graphics object, onCleanup task, workspace variable).
//
// A capturing handle retains the exact definition it points at, not the chain
// head. The head is found by walking nextFunction when the tables free the file.
//
// Retaining a definition the tables do not own (one of a transient script) is
// harmless: nothing frees it through here, so the retain expires with the handle.
//
// Freeing a file whose chain holds a retained definition only drops it from the
// function tables; the last release of that chain frees it. Files no handle
// retained are freed straight away.
NLSINTERPRETER_IMPEXP void
retainMacroFunctionDef(MacroFunctionDef* definition);
//=============================================================================
NLSINTERPRETER_IMPEXP void
releaseMacroFunctionDef(MacroFunctionDef* definition);
//=============================================================================
// Free a file definition owned by the function tables, with its nextFunction
// chain, or defer freeing to the last release when a handle retains one of them.
NLSINTERPRETER_IMPEXP void
destroyMacroFunctionDef(MacroFunctionDef* head);
//=============================================================================
} // namespace Nelson
//=============================================================================
