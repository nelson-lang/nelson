//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "AnonymousMacroFunctionDef.hpp"
#include "BytecodeCompiler.hpp"
#include "BytecodeVM.hpp"
#include "BuiltinEmptyTypeName.hpp"
#include "StringHelpers.hpp"
#include "Context.hpp"
#include "ClassdefParser.hpp"
#include "FunctionsInMemory.hpp"
#include "ParserInterface.hpp"
#include "Warning.hpp"
#include "Evaluator.hpp"
#include "characters_encoding.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonConfiguration.hpp"
#include "Bytecode.hpp"
#include "MacroFunctionDef.hpp"
#include <atomic>
#include <mutex>
#include <unordered_map>
//=============================================================================
namespace Nelson {
//=============================================================================
static void
pinCapturedLocalFunctionOwners(const AnonymousMacroFunctionDef* handle, FunctionDef* boundFunction,
    const std::vector<std::pair<std::string, FunctionDefPtr>>& localFunctions);
static void
releaseCapturedLocalFunctionOwners(const AnonymousMacroFunctionDef* handle);
static void
retainCapturedDefinitions(FunctionDef* boundFunction,
    const std::vector<std::pair<std::string, FunctionDefPtr>>& localFunctions);
static void
releaseCapturedDefinitions(FunctionDef* boundFunction,
    const std::vector<std::pair<std::string, FunctionDefPtr>>& localFunctions);
//=============================================================================
namespace {
    //=============================================================================
    bool
    anonymousBytecodeCanRunWithoutScope(const BytecodeChunk& chunk)
    {
        for (const Instruction& ins : chunk.code) {
            switch (ins.op) {
            case OpCode::LOAD_GLOBAL:
            case OpCode::LOAD_GLOBAL_OR_JUMP:
            case OpCode::STORE_GLOBAL:
            case OpCode::LOAD_ANS:
            case OpCode::STORE_ANS:
            case OpCode::DELETE_LOCAL:
            case OpCode::CALL_NAMED:
            case OpCode::CALL_NAMED_DYNAMIC:
            case OpCode::CALL_BUILTIN:
            case OpCode::CALL_MACRO:
            case OpCode::CALL_HANDLE:
            case OpCode::CALL_HANDLE_DYNAMIC:
            case OpCode::MAKE_FH_NAMED:
            case OpCode::MAKE_FH_ANONYMOUS:
            case OpCode::TRY_BEGIN:
            case OpCode::CATCH_BEGIN:
                return false;
            default:
                break;
            }
        }
        return true;
    }
    //=============================================================================
    bool
    validateAnonymousInputCount(const stringVector& arguments, const ArrayOfVector& inputs)
    {
        if (arguments.empty()) {
            return inputs.empty();
        }
        if (arguments.back() != "varargin") {
            return inputs.size() <= arguments.size();
        }
        return inputs.size() >= arguments.size() - 1;
    }
    //=============================================================================
    bool
    hasAnonymousVarargin(const stringVector& arguments)
    {
        return !arguments.empty() && arguments.back() == "varargin";
    }
    //=============================================================================
} // namespace
//=============================================================================
AnonymousMacroFunctionDef::AnonymousMacroFunctionDef(const std::string& functionHandle)
    : FunctionDef(false)
{
    this->code = nullptr;
    this->isFunctionHandleOnly = true;
    this->boundFunction = nullptr;
    this->sourceFilename.clear();
    this->functionHandleContent = functionHandle;
    if (!functionHandle.empty()) {
        StringHelpers::trim_left(functionHandleContent);
        StringHelpers::trim_right(functionHandleContent);
        if (!functionHandleContent.empty() && functionHandleContent[0] == '@') {
            functionHandleContent.erase(functionHandleContent.begin());
        }
        setName(functionHandleContent);
    }
    arguments.clear();
}
//=============================================================================
AnonymousMacroFunctionDef::AnonymousMacroFunctionDef(const std::string& functionHandle,
    FunctionDef* boundFunction, const stringVector& variableNames,
    const std::vector<ArrayOf>& variables)
    : AnonymousMacroFunctionDef(functionHandle)
{
    this->boundFunction = boundFunction;
    this->variableNames = variableNames;
    this->variables = variables;
    pinCapturedLocalFunctionOwners(this, this->boundFunction, this->localFunctions);
    retainCapturedDefinitions(this->boundFunction, this->localFunctions);
}
//=============================================================================
AnonymousMacroFunctionDef::AnonymousMacroFunctionDef(const std::string& anonymousContent,
    const stringVector& arguments, const stringVector& variableNames,
    const std::vector<ArrayOf>& variables)
    : AnonymousMacroFunctionDef(anonymousContent, arguments, variableNames, variables, L"", {})
{
}
//=============================================================================
AnonymousMacroFunctionDef::AnonymousMacroFunctionDef(const std::string& anonymousContent,
    const stringVector& arguments, const stringVector& variableNames,
    const std::vector<ArrayOf>& variables, const std::wstring& sourceFilename,
    const std::vector<std::pair<std::string, FunctionDefPtr>>& localFunctions)
    : FunctionDef(false)
{
    this->code = nullptr;
    this->isFunctionHandleOnly = false;
    this->boundFunction = nullptr;
    this->anonymousContent = anonymousContent;
    this->arguments = arguments;
    this->sourceFilename = sourceFilename;
    this->localFunctions = localFunctions;

    std::unordered_set<std::string> filteredNames(arguments.begin(), arguments.end());
    filteredNames.insert("varargout");
    stringVector filteredVariableNames;
    std::vector<ArrayOf> filteredVariables;
    filteredVariableNames.reserve(variableNames.size());
    filteredVariables.reserve(variables.size());
    size_t captureCount = std::min(variableNames.size(), variables.size());
    for (size_t k = 0; k < captureCount; ++k) {
        const auto& varName = variableNames[k];
        if (filteredNames.find(varName) == filteredNames.end()) {
            filteredVariableNames.push_back(varName);
            filteredVariables.push_back(variables[k]);
        }
    }

    this->variableNames = std::move(filteredVariableNames);
    this->variables = std::move(filteredVariables);
    pinCapturedLocalFunctionOwners(this, this->boundFunction, this->localFunctions);
    retainCapturedDefinitions(this->boundFunction, this->localFunctions);
}
//=============================================================================
AnonymousMacroFunctionDef::~AnonymousMacroFunctionDef()
{
    // An owner pin and a retain are taken only for boundFunction and the
    // localFunctions entries. Testing them here keeps both registries out of
    // the destructor of an ordinary anonymous handle.
    const bool mayHavePinnedOwners
        = this->boundFunction != nullptr || !this->localFunctions.empty();
    FunctionDef* capturedBoundFunction = this->boundFunction;
    bytecodeChunk.reset();
    AbstractSyntaxTree::deleteReferences(this->ptrAstCodeAsVector);
    this->ptrAstCodeAsVector.clear();
    this->anonymousContent.clear();
    this->functionHandleContent.clear();
    this->previousLhs = -1;
    this->code = nullptr;
    this->isFunctionHandleOnly = false;
    this->boundFunction = nullptr;
    this->arguments.clear();
    this->variableNames.clear();
    this->variables.clear();
    if (mayHavePinnedOwners) {
        releaseCapturedDefinitions(capturedBoundFunction, this->localFunctions);
        this->localFunctions.clear();
        releaseCapturedLocalFunctionOwners(this);
    }
    this->sourceFilename.clear();
}
//=============================================================================
std::string
AnonymousMacroFunctionDef::getDefinition()
{
    if (isFunctionHandleOnly) {
        return "@" + functionHandleContent;
    }
    std::string content = "@(";
    size_t nbArguments = arguments.size();
    for (size_t k = 0; k < arguments.size(); ++k) {
        content = content + arguments[k];
        if (k < nbArguments - 1) {
            content = content + ",";
        }
    }
    content = content + ")";
    content = content + (functionHandleContent.empty() ? anonymousContent : functionHandleContent);
    return content;
}
//=============================================================================
void
AnonymousMacroFunctionDef::setDefinitionText(const std::string& text)
{
    if (!isFunctionHandleOnly) {
        functionHandleContent = text;
    }
}
//=============================================================================
std::string
AnonymousMacroFunctionDef::getDefinitionText()
{
    return isFunctionHandleOnly ? std::string() : functionHandleContent;
}
//=============================================================================
stringVector
AnonymousMacroFunctionDef::getVariableNames()
{
    return variableNames;
}
//=============================================================================
std::vector<ArrayOf>
AnonymousMacroFunctionDef::getVariables()
{
    return variables;
}
//=============================================================================
bool
AnonymousMacroFunctionDef::isBoundNestedFunctionHandle() const
{
    if (!isFunctionHandleOnly || boundFunction == nullptr
        || boundFunction->type() != NLS_MACRO_FUNCTION) {
        return false;
    }
    auto* macroDef = dynamic_cast<MacroFunctionDef*>(boundFunction);
    return macroDef != nullptr && macroDef->nestedFunction;
}
//=============================================================================
int
AnonymousMacroFunctionDef::boundFunctionNargin()
{
    return boundFunction != nullptr ? boundFunction->inputArgCount() : -1;
}
//=============================================================================
int
AnonymousMacroFunctionDef::boundFunctionNargout()
{
    return boundFunction != nullptr ? boundFunction->outputArgCount() : -1;
}
//=============================================================================
void
AnonymousMacroFunctionDef::syncCapturedVariables(
    const stringVector& names, const std::vector<ArrayOf>& values)
{
    if (!isBoundNestedFunctionHandle()) {
        return;
    }
    for (size_t k = 0; k < variableNames.size(); ++k) {
        auto it = std::find(names.begin(), names.end(), variableNames[k]);
        if (it == names.end()) {
            continue;
        }
        size_t sourceIndex = static_cast<size_t>(std::distance(names.begin(), it));
        if (sourceIndex < values.size()) {
            variables[k] = values[sourceIndex];
        }
    }
}
//=============================================================================
int
AnonymousMacroFunctionDef::nargin()
{
    return inputArgCount();
}
//=============================================================================
int
AnonymousMacroFunctionDef::inputArgCount()
{
    if (isFunctionHandleOnly) {
        return -1;
    }
    if (arguments.empty()) {
        return 0;
    }
    if (arguments[arguments.size() - 1] == "varargin") {
        return -1;
    }
    return static_cast<int>(arguments.size());
}
//=============================================================================
int
AnonymousMacroFunctionDef::nargout()
{
    return outputArgCount();
}
//=============================================================================
int
AnonymousMacroFunctionDef::outputArgCount()
{
    return -1;
}
//=============================================================================
bool
AnonymousMacroFunctionDef::isStatelessSimpleIdentity() const
{
    return !isFunctionHandleOnly && arguments.size() == 1 && arguments[0] != "varargin"
        && anonymousContent == arguments[0];
}
//=============================================================================
ArrayOfVector
AnonymousMacroFunctionDef::evaluateFunction(
    Evaluator* eval, const ArrayOfVector& inputs, int nargout)
{
    if (isFunctionHandleOnly) {
        FunctionDef* funcDef = boundFunction;
        if (funcDef == nullptr) {
            eval->lookupFunction(functionHandleContent, funcDef);
        }
        if (funcDef == nullptr) {
            // @double.empty: builtin types have no generated static method, the
            // call goes to the `__empty__` helper as the dotted call does.
            const size_t lastDot = functionHandleContent.find_last_of('.');
            const std::string typeName = functionHandleContent.substr(0, lastDot);
            if (lastDot != std::string::npos
                && functionHandleContent.compare(lastDot, std::string::npos, ".empty") == 0
                && isBuiltinEmptyTypeName(typeName) && eval->lookupFunction("__empty__", funcDef)
                && funcDef != nullptr) {
                ArrayOfVector emptyInputs;
                emptyInputs.reserve(inputs.size() + 1);
                emptyInputs << ArrayOf::characterArray(typeName);
                for (const ArrayOf& input : inputs) {
                    emptyInputs << input;
                }
                return funcDef->evaluateFunction(eval, emptyInputs, nargout);
            }
        }
        if (funcDef == nullptr) {
            // A handle to a classdef static method (@Class.staticMethod, or a
            // package-qualified @pkg.Class.staticMethod) carries a dotted name
            // that the plain function lookup does not resolve. Resolve it as a
            // static method and fetch the generated function.
            const size_t lastDot = functionHandleContent.find_last_of('.');
            if (lastDot != std::string::npos && lastDot > 0
                && lastDot + 1 < functionHandleContent.size()) {
                const std::string className = functionHandleContent.substr(0, lastDot);
                const std::string methodName = functionHandleContent.substr(lastDot + 1);
                std::string resolvedName;
                if (classdefResolveStaticMethodFunction(
                        className, methodName, resolvedName, eval->getClassdefAccessContext())) {
                    FunctionDefPtr resolved = nullptr;
                    if (FunctionsInMemory::getInstance()->find(resolvedName, resolved)
                        && resolved != nullptr) {
                        funcDef = resolved;
                    }
                }
            }
        }
        if (funcDef == nullptr && !inputs.empty()) {
            // A bare @name handle may refer to a method of the class of its
            // arguments (for instance a sibling method captured inside a class
            // method: cellfun(@method, objs) invokes the class implementation).
            // When the plain and static lookups fail, dispatch on the class of
            // the first object argument, matching overloaded call semantics.
            for (const ArrayOf& arg : inputs) {
                std::string argClass;
                if (arg.isHandle()) {
                    argClass = arg.handleCategory();
                } else if (arg.isClassType()) {
                    argClass = arg.classType();
                } else {
                    continue;
                }
                if (argClass.empty()) {
                    continue;
                }
                std::string resolvedName;
                if (classdefResolveMethodFunction(argClass, functionHandleContent, resolvedName,
                        eval->getClassdefAccessContext())) {
                    FunctionDefPtr resolved = nullptr;
                    // Resolve the mangled overload name through the context so a
                    // method file that is on the path but not yet parsed is
                    // loaded, rather than only matching already-in-memory ones.
                    if (lookupOverloadFunctionCached(eval->getContext(), resolvedName, resolved)
                        && resolved != nullptr) {
                        funcDef = resolved;
                        break;
                    }
                }
            }
        }
        if (funcDef) {
            if (boundFunction != nullptr && funcDef->type() == NLS_MACRO_FUNCTION) {
                Context* context = eval->getContext();
                context->pushScope(functionHandleContent);
                try {
                    for (size_t k = 0; k < variableNames.size() && k < variables.size(); ++k) {
                        context->insertVariableLocally(variableNames[k], variables[k]);
                    }
                    ArrayOfVector outputs = funcDef->evaluateFunction(eval, inputs, nargout);
                    Scope* closureScope = context->getCurrentScope();
                    for (size_t k = 0; k < variableNames.size() && k < variables.size(); ++k) {
                        ArrayOf value;
                        if (closureScope->lookupVariable(variableNames[k], value)) {
                            variables[k] = value;
                        }
                    }
                    context->popScope();
                    return outputs;
                } catch (const NelsonException&) {
                    context->popScope();
                    throw;
                }
            }
            return funcDef->evaluateFunction(eval, inputs, nargout);
        }
        std::string msg = functionHandleContent.empty()
            ? Nelson::resolveMessageTemplate(
                  "Nelson:interpreter:undefinedFunction", "Undefined function.")
            : formatI18nMessage(
                  Nelson::resolveMessageTemplate("Nelson:interpreter:undefinedVariableFunction",
                      "Undefined variable or function: %s"),
                  functionHandleContent);
        std::string id = "Nelson:UndefinedFunction";
        RaiseError(msg, id);
    }
    std::lock_guard<std::recursive_mutex> lock(functionMutex);
    updateCode(nargout);
    if (variables.empty() && !hasAnonymousVarargin(arguments)
        && validateAnonymousInputCount(arguments, inputs)) {
        ensureBytecodeCompiled(nargout);
        if (bytecodeChunk != nullptr) {
            if (!bytecodeChunk->anonymousScopeFreeComputed) {
                bytecodeChunk->anonymousScopeFree
                    = anonymousBytecodeCanRunWithoutScope(*bytecodeChunk);
                bytecodeChunk->anonymousScopeFreeComputed = true;
            }
            if (bytecodeChunk->anonymousScopeFree) {
                // no scope of its own: the caller's scope must stay untouched
                return BytecodeVM::executeFunction(eval, *bytecodeChunk, inputs, nargout, false);
            }
        }
    }
    ArrayOfVector outputs;
    size_t minCount = 0;
    Context* context = eval->getContext();
    if (sourceFilename.empty()) {
        context->pushScope(this->getName());
    } else {
        context->pushScope(this->getName(), sourceFilename);
    }

    eval->callstack.pushDebug("", this->getName());
    for (const auto& localFunction : localFunctions) {
        if (localFunction.second != nullptr) {
            context->insertMacroFunctionLocally(localFunction.second);
        }
    }
    stringVector inputNames;
    inputNames.reserve(inputs.size());
    for (const auto& input : inputs) {
        inputNames.push_back(input.name());
    }
    context->getCurrentScope()->setInputArgumentNames(inputNames);
    minCount = 0;
    if (inputArgCount() != -1) {
        if (inputs.size() > arguments.size()) {
            context->popScope();
            eval->callstack.popDebug();
            Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
        }
        minCount = (inputs.size() < arguments.size()) ? inputs.size() : arguments.size();
        for (size_t i = 0; i < minCount; i++) {
            std::string arg(arguments[i]);
            if (arg != "~") {
                context->insertVariableLocally(arg, inputs[i]);
            }
        }
        context->getCurrentScope()->setNargIn(static_cast<int>(minCount));
    } else {
        size_t inputCount = inputs.size();
        size_t nbArgumentsWithoutVarArgIn = arguments.size();
        if (arguments[arguments.size() - 1] == "varargin") {
            nbArgumentsWithoutVarArgIn = nbArgumentsWithoutVarArgIn - 1;
        }
        if (inputCount < nbArgumentsWithoutVarArgIn) {
            context->popScope();
            eval->callstack.popDebug();
            Error(L"Nelson:notEnoughInputs", L"Not enough input arguments.");
        }
        context->getCurrentScope()->setNargIn(static_cast<int>(inputCount));
        int explicitCount = static_cast<int>(arguments.size()) - 1;
        minCount = (explicitCount < static_cast<int>(inputCount))
            ? static_cast<size_t>(explicitCount)
            : inputCount;
        size_t i;
        for (i = 0; i < minCount; i++) {
            std::string arg(arguments[i]);
            if (arg != "~") {
                context->insertVariableLocally(arg, inputs[i]);
            }
        }
        inputCount -= minCount;
        ArrayOf varg(NLS_CELL_ARRAY);
        varg.vectorResize(inputCount);
        auto* dp = static_cast<ArrayOf*>(varg.readWriteDataPointer());
        for (i = 0; i < inputCount; i++) {
            dp[i] = inputs[i + minCount];
        }
        varg.name("varargin");
        context->insertVariableLocally("varargin", varg);
    }
    for (size_t k = 0; k < variableNames.size(); k++) {
        context->insertVariableLocally(variableNames[k], variables[k]);
    }

    context->getCurrentScope()->setNargOut(nargout);
    uint64 tic = 0;
    bool backupEcho = eval->getEchoMode();
    eval->setEchoMode(false);
    try {
        tic = Profiler::getInstance()->tic();
        ensureBytecodeCompiled(nargout);
        outputs = BytecodeVM::executeFunction(eval, *bytecodeChunk, inputs, nargout);
        eval->setEchoMode(backupEcho);
        if (tic != 0) {
            internalProfileFunction stack = computeProfileStack(eval, getName(), L"", false);
            Profiler::getInstance()->toc(tic, stack);
        }
        State state(eval->getState());
        if (state < NLS_STATE_QUIT) {
            eval->resetState();
        }
        if (state == NLS_STATE_ABORT) {
            return scalarArrayOfToArrayOfVector(ArrayOf::empty());
        }
        for (size_t k = 0; k < variableNames.size() && k < variables.size(); ++k) {
            ArrayOf value;
            if (context->lookupVariableLocally(variableNames[k], value)) {
                variables[k] = value;
            }
        }
        context->popScope();
        eval->callstack.popDebug();
    } catch (const NelsonException&) {
        internalProfileFunction stack = computeProfileStack(eval, getName(), L"", false);
        Profiler::getInstance()->toc(tic, stack);
        eval->setEchoMode(backupEcho);
        context->popScope();
        eval->callstack.popDebug();
        throw;
    }
    return outputs;
}
//=============================================================================
void
AnonymousMacroFunctionDef::invalidateBytecode()
{
    std::lock_guard<std::recursive_mutex> lock(functionMutex);
    bytecodeChunk.reset();
}
//=============================================================================
bool
AnonymousMacroFunctionDef::ensureBytecodeCompiled(int nLhs)
{
    std::lock_guard<std::recursive_mutex> lock(functionMutex);
    if (isFunctionHandleOnly) {
        return false;
    }
    updateCode(nLhs);
    if (bytecodeChunk != nullptr) {
        return true;
    }
    bytecodeChunk = BytecodeCompiler::compileAnonymous(code, variableNames, arguments, returnVals);
    return bytecodeChunk != nullptr;
}
//=============================================================================
std::string
AnonymousMacroFunctionDef::convertToStandardFunction(int nLhs)
{
    if (!isFunctionHandleOnly) {
        std::string functionReworked;
        std::string outputVariablesList;
        if (nLhs == 1) {
            outputVariablesList = "_VAR_ANONYMOUS_";
            outputVariablesList += " = ";

        } else {
            for (int k = 0; k < nLhs; ++k) {
                if (k == 0) {
                    outputVariablesList = "[_VAR_ANONYMOUS_" + std::to_string(k);
                } else {
                    outputVariablesList += ", _VAR_ANONYMOUS_" + std::to_string(k);
                }
            }
            if (nLhs > 0) {
                outputVariablesList += "] = ";
            }
        }
        functionReworked = "function " + outputVariablesList + "anonymousFunction(";
        size_t nbArguments = arguments.size();
        for (size_t k = 0; k < arguments.size(); ++k) {
            functionReworked = functionReworked + arguments[k];
            if (k < nbArguments - 1) {
                functionReworked = functionReworked + ",";
            }
        }
        functionReworked = functionReworked + ")" + "\n";
        functionReworked = functionReworked + outputVariablesList + anonymousContent + ";" + "\n";
        return functionReworked;
    }
    return "";
}
//=============================================================================
bool
AnonymousMacroFunctionDef::updateCode(int nLhs)
{
    std::lock_guard<std::recursive_mutex> lock(functionMutex);
    if (isFunctionHandleOnly) {
        return true;
    }
    bool needToUpdate = (code == nullptr) || (this->previousLhs != nLhs);
    if (!needToUpdate) {
        return false;
    }
    this->previousLhs = nLhs;
    if (code != nullptr) {
        invalidateBytecode();
        for (auto ptr : this->ptrAstCodeAsVector) {
            delete ptr;
        }
        ptrAstCodeAsVector.clear();
        code = nullptr;
    }
    ParserState pstate = ParserState::ParseError;
    AbstractSyntaxTree::clearReferences();
    AbstractSyntaxTreePtrVector ptAstCode;
    Evaluator* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
    try {
        if (eval) {
            pstate = parseString(eval->lexerContext, convertToStandardFunction(nLhs));
        } else {
            LexerContext lexerContext;
            pstate = parseString(lexerContext, convertToStandardFunction(nLhs));
        }
        ptAstCode = AbstractSyntaxTree::getReferences();
        // The nodes now belong to this definition (released on the next
        // re-parse and in the destructor): drop them from the shared list.
        AbstractSyntaxTree::clearReferences();
    } catch (const NelsonException&) {
        AbstractSyntaxTree::deleteReferences();
        throw;
    }
    if (pstate == ParserState::ParseError) {
        AbstractSyntaxTree::deleteReferences(ptAstCode);
        AbstractSyntaxTree::clearReferences();
        Error(L"Nelson:interpreter:invalidAnonymousFunctionDefinition",
            L"a valid function definition expected.");
    }

    MacroFunctionDef* macroFunctionDef = getParsedFunctionDef();
    if (macroFunctionDef) {
        if (macroFunctionDef->code && macroFunctionDef->code->down
            && macroFunctionDef->code->down->right != nullptr) {
            macroFunctionDef->code = nullptr;
            delete macroFunctionDef;
            macroFunctionDef = nullptr;
            AbstractSyntaxTree::deleteReferences(ptAstCode);
            Error("Nelson:interpreter:invalidExpression", "simple expression expected.");
            return false;
        }
        this->code = macroFunctionDef->code;
        this->ptrAstCodeAsVector = ptAstCode;
        this->arguments = macroFunctionDef->arguments;
        this->setName("Anonymous");
        this->returnVals = macroFunctionDef->returnVals;
        invalidateBytecode();
        macroFunctionDef->code = nullptr;
        delete macroFunctionDef;
        return true;
    }
    AbstractSyntaxTree::deleteReferences(ptAstCode);
    return false;
}
//=============================================================================
bool
AnonymousMacroFunctionDef::updateCode()
{
    if (isFunctionHandleOnly) {
        return true;
    }
    return updateCode(1);
}
//=============================================================================
BytecodeChunk*
AnonymousMacroFunctionDef::scopeFreeBytecode(size_t inputCount, int nLhs)
{
    if (isFunctionHandleOnly) {
        return nullptr;
    }
    std::lock_guard<std::recursive_mutex> lock(functionMutex);
    updateCode(nLhs);
    if (!variables.empty() || hasAnonymousVarargin(arguments) || inputCount > arguments.size()) {
        return nullptr;
    }
    ensureBytecodeCompiled(nLhs);
    if (bytecodeChunk == nullptr) {
        return nullptr;
    }
    if (!bytecodeChunk->anonymousScopeFreeComputed) {
        bytecodeChunk->anonymousScopeFree = anonymousBytecodeCanRunWithoutScope(*bytecodeChunk);
        bytecodeChunk->anonymousScopeFreeComputed = true;
    }
    return bytecodeChunk->anonymousScopeFree ? bytecodeChunk.get() : nullptr;
}
//=============================================================================
// Out-of-line table of the transient owners pinned by each anonymous function
// handle. It is kept out of the class to leave the class layout unchanged.
// A handle maps one-to-one to an AnonymousMacroFunctionDef whose destructor
// releases the pin.
static std::mutex&
capturedOwnersMutex()
{
    static auto* mutex = new std::mutex;
    return *mutex;
}
//=============================================================================
static std::unordered_map<const AnonymousMacroFunctionDef*,
    std::vector<std::shared_ptr<MacroFunctionDef>>>&
capturedOwnersTable()
{
    using CapturedOwners = std::unordered_map<const AnonymousMacroFunctionDef*,
        std::vector<std::shared_ptr<MacroFunctionDef>>>;
    static auto* table = new CapturedOwners;
    return *table;
}
//=============================================================================
// Number of handles that currently pin at least one transient owner. The common
// case (no transient captures live anywhere) is a single relaxed atomic load, so
// the destructor of an ordinary function handle never touches the lock.
static std::atomic<size_t>&
capturedOwnersActiveCount()
{
    static auto* count = new std::atomic<size_t> { 0 };
    return *count;
}
//=============================================================================
static void
pinCapturedLocalFunctionOwners(const AnonymousMacroFunctionDef* handle, FunctionDef* boundFunction,
    const std::vector<std::pair<std::string, FunctionDefPtr>>& localFunctions)
{
    std::vector<std::shared_ptr<MacroFunctionDef>> owners;
    auto pinOwnerOf = [&owners](FunctionDef* captured) {
        if (captured == nullptr || captured->type() != NLS_MACRO_FUNCTION) {
            return;
        }
        auto* head = static_cast<MacroFunctionDef*>(captured);
        while (head->prevFunction != nullptr) {
            head = head->prevFunction;
        }
        std::shared_ptr<MacroFunctionDef> owner = lookupTransientScriptOwner(head);
        if (owner == nullptr) {
            return;
        }
        for (const auto& existing : owners) {
            if (existing == owner) {
                return;
            }
        }
        owners.push_back(std::move(owner));
    };
    pinOwnerOf(boundFunction);
    for (const auto& localFunction : localFunctions) {
        pinOwnerOf(localFunction.second);
    }
    if (owners.empty()) {
        return;
    }
    std::lock_guard<std::mutex> lock(capturedOwnersMutex());
    capturedOwnersTable()[handle] = std::move(owners);
    capturedOwnersActiveCount().fetch_add(1, std::memory_order_relaxed);
}
//=============================================================================
static void
releaseCapturedLocalFunctionOwners(const AnonymousMacroFunctionDef* handle)
{
    if (capturedOwnersActiveCount().load(std::memory_order_relaxed) == 0) {
        return;
    }
    // Dropping an owner may destroy it, and with it function handle values it
    // holds, which re-enters this function: destroy the owners unlocked.
    std::vector<std::shared_ptr<MacroFunctionDef>> owners;
    {
        std::lock_guard<std::mutex> lock(capturedOwnersMutex());
        auto it = capturedOwnersTable().find(handle);
        if (it == capturedOwnersTable().end()) {
            return;
        }
        owners = std::move(it->second);
        capturedOwnersTable().erase(it);
        capturedOwnersActiveCount().fetch_sub(1, std::memory_order_relaxed);
    }
}
//=============================================================================
// A function handle keeps the definitions it captured alive: the function tables
// free them on `clear functions` and when the file watcher reloads a file, while
// the handle stays callable for as long as anything references it - a callback
// stored in a graphics object, an onCleanup task, a variable in the workspace.
//
// Retaining is unconditional, so releasing mirrors it with no bookkeeping. A
// definition owned by a transient script is freed through that owner's shared
// reference, and its retain expires unused. boundFunction and localFunctions
// are set by different constructors, so a definition is retained once.
static void
retainCapturedDefinitions(FunctionDef* boundFunction,
    const std::vector<std::pair<std::string, FunctionDefPtr>>& localFunctions)
{
    if (boundFunction != nullptr && boundFunction->type() == NLS_MACRO_FUNCTION) {
        retainMacroFunctionDef(static_cast<MacroFunctionDef*>(boundFunction));
    }
    for (const auto& localFunction : localFunctions) {
        FunctionDef* captured = localFunction.second;
        if (captured != nullptr && captured->type() == NLS_MACRO_FUNCTION) {
            retainMacroFunctionDef(static_cast<MacroFunctionDef*>(captured));
        }
    }
}
//=============================================================================
static void
releaseCapturedDefinitions(FunctionDef* boundFunction,
    const std::vector<std::pair<std::string, FunctionDefPtr>>& localFunctions)
{
    if (boundFunction != nullptr && boundFunction->type() == NLS_MACRO_FUNCTION) {
        releaseMacroFunctionDef(static_cast<MacroFunctionDef*>(boundFunction));
    }
    for (const auto& localFunction : localFunctions) {
        FunctionDef* captured = localFunction.second;
        if (captured != nullptr && captured->type() == NLS_MACRO_FUNCTION) {
            releaseMacroFunctionDef(static_cast<MacroFunctionDef*>(captured));
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
