//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Output-probe and silent varargout-retry helpers split out of BytecodeVM.cpp.
// A ScopedOutputProbe temporarily wraps the evaluator's Interface to detect
// whether a call produced any console output; the varargout AST helpers decide
// whether a macro that uses varargout{nargout} must be retried with nargout 0.
// These sit on cold call-resolution branches, not the scalar dispatch loop.
//=============================================================================
#include <string>
#include "BytecodeVMInternal.hpp"
#include "AbstractSyntaxTree.hpp"
#include "ClassdefParser.hpp"
#include "Context.hpp"
#include "Evaluator.hpp"
#include "FunctionDef.hpp"
#include "Interface.hpp"
#include "MacroFunctionDef.hpp"
#include "NelsonPrint.hpp"
#include "OverloadName.hpp"
#include "FunctionsInMemory.hpp"
//=============================================================================
namespace Nelson::bcvm_detail {
//=============================================================================
static bool
findObjectOverloadFunction(Evaluator* eval, const ArrayOf& receiver,
    const std::string& functionName, FunctionDef*& funcDef);
//=============================================================================
class OutputProbeInterface : public Interface
{
private:
    Interface* wrapped = nullptr;
    bool didOutput = false;

public:
    explicit OutputProbeInterface(Interface* wrappedInterface) : wrapped(wrappedInterface) { }

    bool
    hasOutput() const
    {
        return didOutput;
    }

    std::string
    getLine(const std::string& prompt) override
    {
        return wrapped->getLine(prompt);
    }

    std::wstring
    getLine(const std::wstring& prompt) override
    {
        return wrapped->getLine(prompt);
    }

    std::wstring
    getInput(const std::wstring& prompt) override
    {
        return wrapped->getInput(prompt);
    }

    size_t
    getTerminalWidth() override
    {
        return wrapped->getTerminalWidth();
    }

    size_t
    getTerminalHeight() override
    {
        return wrapped->getTerminalHeight();
    }

    void
    outputMessage(const std::string& msg) override
    {
        didOutput = true;
        wrapped->outputMessage(msg);
    }

    void
    outputMessage(const std::wstring& msg) override
    {
        didOutput = true;
        wrapped->outputMessage(msg);
    }

    void
    errorMessage(const std::string& msg) override
    {
        didOutput = true;
        wrapped->errorMessage(msg);
    }

    void
    errorMessage(const std::wstring& msg) override
    {
        didOutput = true;
        wrapped->errorMessage(msg);
    }

    void
    warningMessage(const std::string& msg) override
    {
        didOutput = true;
        wrapped->warningMessage(msg);
    }

    void
    warningMessage(const std::wstring& msg) override
    {
        didOutput = true;
        wrapped->warningMessage(msg);
    }

    bool
    outputExpandableText(const std::wstring& label, const std::wstring& expandedText,
        const std::wstring& plainFallback, const std::wstring& contextName = L"",
        long long contextValue = 0) override
    {
        didOutput = true;
        return wrapped->outputExpandableText(
            label, expandedText, plainFallback, contextName, contextValue);
    }

    bool
    outputStyledText(const std::wstring& msg, const TextStyle& style) override
    {
        didOutput = true;
        return wrapped->outputStyledText(msg, style);
    }

    void
    clearTerminal() override
    {
        wrapped->clearTerminal();
    }

    bool
    isAtPrompt() override
    {
        return wrapped->isAtPrompt();
    }

    void
    interruptGetLineByEvent() override
    {
        wrapped->interruptGetLineByEvent();
    }
};
//=============================================================================
class ScopedOutputProbe
{
private:
    Evaluator* eval = nullptr;
    Interface* savedInterface = nullptr;
    OutputProbeInterface probe;

public:
    explicit ScopedOutputProbe(Evaluator* evaluator)
        : eval(evaluator), savedInterface(evaluator->getInterface()), probe(savedInterface)
    {
        eval->setInterface(&probe);
        setPrintInterface(&probe);
    }

    ~ScopedOutputProbe()
    {
        eval->setInterface(savedInterface);
        setPrintInterface(savedInterface);
    }

    bool
    hasOutput() const
    {
        return probe.hasOutput();
    }
};
//=============================================================================
bool
isMacroFunctionWithVarargout(FunctionDef* fdef)
{
    return fdef != nullptr
        && (fdef->type() == NLS_MACRO_FUNCTION || fdef->type() == NLS_ANONYMOUS_MACRO_FUNCTION)
        && fdef->outputArgCount() == -1;
}
//=============================================================================
bool
astContainsIdentifier(AbstractSyntaxTreePtr node, const std::string& name)
{
    while (node != nullptr) {
        if (node->type == id_node && node->text == name) {
            return true;
        }
        if (astContainsIdentifier(node->down, name)) {
            return true;
        }
        node = node->right;
    }
    return false;
}
//=============================================================================
bool
astContainsVarargoutIndexedByNargout(AbstractSyntaxTreePtr node)
{
    while (node != nullptr) {
        if (node->type == id_node && node->text == "varargout" && node->down != nullptr
            && node->down->opNum == OP_BRACES
            && astContainsIdentifier(node->down->down, "nargout")) {
            return true;
        }
        if (astContainsVarargoutIndexedByNargout(node->down)) {
            return true;
        }
        node = node->right;
    }
    return false;
}
//=============================================================================
bool
shouldRetrySilentVarargoutCall(FunctionDef* fdef)
{
    if (!isMacroFunctionWithVarargout(fdef) || fdef->type() != NLS_MACRO_FUNCTION) {
        return false;
    }
    auto* macroDef = reinterpret_cast<MacroFunctionDef*>(fdef);
    return astContainsVarargoutIndexedByNargout(macroDef->code);
}
//=============================================================================
ArrayOfVector
evaluateFunctionAndDetectOutput(
    Evaluator* eval, FunctionDef* fdef, const ArrayOfVector& args, int nout, bool& hasOutput)
{
    ScopedOutputProbe probe(eval);
    ArrayOfVector result = fdef->evaluateFunction(eval, args, nout);
    hasOutput = probe.hasOutput();
    return result;
}
//=============================================================================
ArrayOfVector
callFunctionHandleAndDetectOutput(
    Evaluator* eval, const ArrayOf& handle, const ArrayOfVector& args, int nout, bool& hasOutput)
{
    ScopedOutputProbe probe(eval);
    ArrayOfVector result = callFunctionHandle(eval, handle, args, nout);
    hasOutput = probe.hasOutput();
    return result;
}
//=============================================================================
bool
displayImplicitObjectOverloadCall(Evaluator* eval, const std::string& functionName,
    const ArrayOfVector& args, bool refreshCallerContext,
    const std::function<void()>& refreshLocalsFromContext)
{
    if (args.empty()) {
        return false;
    }
    FunctionDef* overloadDef = nullptr;
    if (!findObjectOverloadFunction(eval, args[0], functionName, overloadDef)
        || overloadDef->outputArgCount() <= 0) {
        return false;
    }
    size_t clearEpoch = eval->getContext()->getVariablesClearedEpoch();
    ArrayOfVector result = overloadDef->evaluateFunction(eval, args, 1);
    if (eval->getContext()->getVariablesClearedEpoch() != clearEpoch) {
        refreshLocalsFromContext();
    } else if (refreshCallerContext) {
        refreshLocalsFromContext();
    }
    if (!result.empty()) {
        eval->getContext()->insertVariable("ans", result[0]);
        for (size_t k = 0; k < result.size(); ++k) {
            eval->display(result[k], "ans", false, true);
        }
    }
    return true;
}
//=============================================================================
static bool
findObjectOverloadFunction(Evaluator* eval, const ArrayOf& receiver,
    const std::string& functionName, FunctionDef*& funcDef)
{
    funcDef = nullptr;
    if (!(receiver.isHandle() || receiver.isClassType())) {
        return false;
    }
    std::string currentType = receiver.isHandle() ? receiver.handleCategory()
        : receiver.isClassType()                  ? receiver.classType()
                                                  : "";
    if (currentType.empty()) {
        return false;
    }
    std::string overloadName = getOverloadFunctionName(currentType, functionName);
    bool canUseDirectMethod = true;
    if (classdefLoadClass(currentType)) {
        canUseDirectMethod = classdefResolveMethodFunction(
            currentType, functionName, overloadName, eval->getClassdefAccessContext());
    }
    return canUseDirectMethod
        && lookupOverloadFunctionCached(eval->getContext(), overloadName, funcDef)
        && funcDef != nullptr;
}
//=============================================================================
} // namespace Nelson::bcvm_detail
//=============================================================================
