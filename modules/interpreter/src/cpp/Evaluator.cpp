//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#if defined(__clang__) && (__clang_major__ >= 17)
#pragma clang diagnostic ignored "-Wmissing-template-arg-list-after-template-kw"
#endif
#include "BytecodeCompiler.hpp"
#include "BytecodeVM.hpp"
#include "ClassdefParser.hpp"
#include "Evaluator.hpp"
#include "NelsonConfiguration.hpp"
#include "LexerInterface.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "ParserInterface.hpp"
#include "Interface.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Process-wide command-vs-expression variable check for the lexer: resolve the
// head identifier against the current main evaluator's live workspace. Used for
// parse contexts (script/function bodies) that carry no per-context hook.
static bool
lexerResolveVariableFromMainEvaluator(const std::string& name)
{
    NelsonConfiguration* config = NelsonConfiguration::getInstance();
    // During shutdown getMainEvaluator() may still return a dangling pointer to an
    // evaluator whose Context is already deleted, and finish.m is lexed in that
    // window. Returning false falls back to the conservative shape test.
    if (config->isShuttingDown()) {
        return false;
    }
    auto* eval = static_cast<Evaluator*>(config->getMainEvaluator());
    return (eval != nullptr) && (eval->getContext() != nullptr)
        && eval->getContext()->isVariable(name);
}
//=============================================================================
Evaluator::Evaluator(Context* aContext, Interface* aInterface, bool haveEventsLoop, size_t ID)
{
    this->ID = ID;
    NelsonException e;
    this->setLastErrorException(e);
    this->setLastWarningException(e);
    this->resetLastUncaughtException();
    context = aContext;
    // Command-vs-expression syntax is resolved against the live workspace ("x /y"
    // divides, "disp /y" is a command). The per-context hook serves interactive
    // parses; the process-wide resolver covers script and function bodies.
    lexerContext.isVariableHook = [this](const std::string& name) -> bool {
        return (this->context != nullptr) && this->context->isVariable(name);
    };
    setLexerVariableResolver(&lexerResolveVariableFromMainEvaluator);
    resetState();
    depth = 0;
    _haveEventsLoop = haveEventsLoop;
    io = aInterface;
    defaultInterface = aInterface;
    InCLI = false;
    bpActive = false;
    clearStacks();
    commandLineArguments.clear();
}
//=============================================================================
Evaluator::~Evaluator()
{
    // The process-wide resolver reaches the workspace through getMainEvaluator(),
    // which must never resolve to a destroyed evaluator.
    setLexerVariableResolver(nullptr);
    clearStacks();
    commandLineArguments.clear();
    resetLastErrorException();
    resetLastWarningException();
    resetLastUncaughtException();
}
//=============================================================================
void
sigInterrupt(int arg)
{
    NelsonConfiguration::getInstance()->setInterruptPending(true, 0);
}
//=============================================================================
void
Evaluator::clearStacks()
{
    callstack.clear();
    classdefAccessContextStack.clear();
    classdefPropertySetContextStack.clear();
}
//=============================================================================
void
Evaluator::resetState()
{
    state = NLS_STATE_OK;
}
//=============================================================================
State
Evaluator::setState(State newState)
{
    State previousState = state;
    state = newState;
    return previousState;
}
//=============================================================================
State
Evaluator::getState()
{
    return state;
}
//=============================================================================
bool
Evaluator::isQuitOrForceQuitState()
{
    return ((state == NLS_STATE_QUIT) || (state == NLS_STATE_FORCE_QUIT));
}
//=============================================================================
int
Evaluator::getExitCode()
{
    return exitCode;
}
//=============================================================================
void
Evaluator::setExitCode(int _exitCode)
{
    exitCode = _exitCode;
}
//=============================================================================
bool
Evaluator::needToOverloadOperator(const ArrayOf& a)
{
    return ((a.dataClass() == NLS_STRUCT_ARRAY) || (a.dataClass() == NLS_CELL_ARRAY)
        || (a.dataClass() == NLS_STRING_ARRAY) || a.isSparse() || a.isHandle()
        || (a.dataClass() == NLS_CLASS_ARRAY) || (a.dataClass() == NLS_CLASSDEF_ARRAY)
        || (a.dataClass() == NLS_FUNCTION_HANDLE));
}
//=============================================================================
bool
Evaluator::lookupFunction(const std::string& funcName, FunctionDefPtr& val)
{
    return context->lookupFunction(funcName, val);
}
//=============================================================================
void
Evaluator::pushClassdefAccessContext(const std::string& className)
{
    classdefAccessContextStack.push_back(className);
}
//=============================================================================
void
Evaluator::popClassdefAccessContext()
{
    if (!classdefAccessContextStack.empty()) {
        classdefAccessContextStack.pop_back();
    }
}
//=============================================================================
std::string
Evaluator::getClassdefAccessContext() const
{
    return classdefAccessContextStack.empty() ? std::string() : classdefAccessContextStack.back();
}
//=============================================================================
bool
Evaluator::isClassdefMethodContextOf(const std::string& className) const
{
    if (classdefAccessContextStack.empty() || classdefAccessContextStack.back().empty()) {
        return false;
    }
    const std::string& context = classdefAccessContextStack.back();
    return context == className || classdefIsA(className, context);
}
//=============================================================================
bool
Evaluator::isOverloadedDotAccess(const std::string& className, bool indexingOverloaded) const
{
    // builtin('subsref'/'subsasgn', ...) clears withOverload: default indexing
    return indexingOverloaded && withOverload && !isClassdefMethodContextOf(className);
}
//=============================================================================
bool
Evaluator::isDirectPropertyAccess(
    const std::string& className, const std::string& propertyName, bool indexingOverloaded) const
{
    return classdefHasExactProperty(className, propertyName)
        && !isOverloadedDotAccess(className, indexingOverloaded);
}
//=============================================================================
void
Evaluator::pushClassdefPropertySetContext(
    const std::string& className, const std::string& propertyName)
{
    classdefPropertySetContextStack.emplace_back(className, propertyName);
}
//=============================================================================
void
Evaluator::popClassdefPropertySetContext()
{
    if (!classdefPropertySetContextStack.empty()) {
        classdefPropertySetContextStack.pop_back();
    }
}
//=============================================================================
bool
Evaluator::isCurrentClassdefPropertySetContext(
    const std::string& className, const std::string& propertyName) const
{
    if (classdefPropertySetContextStack.empty()) {
        return false;
    }
    const auto& current = classdefPropertySetContextStack.back();
    return current.first == className && current.second == propertyName;
}
//=============================================================================
void
Evaluator::pushClassdefConstruction(const std::string& className)
{
    classdefConstructionStack.push_back(className);
}
//=============================================================================
void
Evaluator::popClassdefConstruction()
{
    if (!classdefConstructionStack.empty()) {
        classdefConstructionStack.pop_back();
    }
}
//=============================================================================
bool
Evaluator::isConstructingClass(const std::string& className) const
{
    for (const auto& name : classdefConstructionStack) {
        if (name == className) {
            return true;
        }
    }
    return false;
}
//=============================================================================
Context*
Evaluator::getContext()
{
    return context;
}
//=============================================================================
stringVector
Evaluator::getCallers(bool includeCurrent)
{
    stringVector callersName;
    size_t i = 0;
    while (i < callstack.size()) {
        if (callstack.getID(i) == 0) {
            size_t j = i + 1;
            while ((j < callstack.size()) && (callstack.getID(j) != 0)) {
                j++;
            }
            std::string functionname = callstack.getDetail(i);
            if (StringHelpers::starts_with(functionname, "built-in ")) {
                StringHelpers::replace_all(functionname, "built-in ", "");
            } else if (StringHelpers::starts_with(functionname, "filename ")) {
                StringHelpers::replace_all(functionname, "filename ", "");
                if (StringHelpers::ends_with(functionname, ".m")) {
                    FileSystemWrapper::Path p(functionname);
                    functionname = p.stem().generic_string();
                    callersName.push_back(functionname);
                }
            } else {
                bool bOK = !StringHelpers::contains(functionname, "(")
                    && !StringHelpers::contains(functionname, ")")
                    && !StringHelpers::contains(functionname, "'")
                    && !StringHelpers::contains(functionname, "/")
                    && !StringHelpers::contains(functionname, "\\")
                    && !StringHelpers::contains(functionname, " ")
                    && !StringHelpers::contains(functionname, ",");
                if (bOK) {
                    callersName.push_back(functionname);
                }
            }
            i = j;
        } else {
            i++;
        }
    }
    if (!includeCurrent) {
        callersName.pop_back();
    }
    return callersName;
}
//=============================================================================
void
Evaluator::setInterface(Interface* _io)
{
    io = _io;
}
//=============================================================================
Interface*
Evaluator::getInterface()
{
    return io;
}
//=============================================================================
Interface*
Evaluator::getDefaultInterface()
{
    return defaultInterface ? defaultInterface : io;
}
//=============================================================================
size_t
Evaluator::getID()
{
    return ID;
}
//=============================================================================
bool
Evaluator::evaluateString(const std::wstring& line, bool propagateException)
{
    return evaluateString(wstring_to_utf8(line), propagateException);
}
//=============================================================================
bool
Evaluator::evaluateString(const std::string& line, bool propagateException)
{
    AbstractSyntaxTreePtr tree = nullptr;
    ParserState parserState = ParseError;
    NelsonConfiguration::getInstance()->setInterruptPending(false, ID);
    if (line.size() == 0) {
        return false;
    }
    if (line == "\n") {
        return false;
    }
    std::string command = line;
    char ch = *command.rbegin();
    // the parser needs a trailing <RETURN>
    if (ch != '\n') {
        command = command + "\n";
    }
    AbstractSyntaxTree::clearReferences();
    try {
        parserState = parseString(lexerContext, command);
    } catch (NelsonException& e) {
        AbstractSyntaxTree::deleteReferences();
        resetParser();
        setLastErrorException(e);
        if (propagateException) {
            throw;
        }
        setLastUncaughtException(e);
        e.printMe(io);
        return false;
    }
    if (parserState != ScriptBlock) {
        AbstractSyntaxTree::deleteReferences();
        resetParser();
        NelsonException e(
            resolveMessageTemplate(L"Nelson:interpreter:validScript", L"a valid script expected."),
            L"Nelson:interpreter:validScript");
        setLastErrorException(e);
        if (propagateException) {
            throw;
        }
        setLastUncaughtException(e);
        return false;
    }
    tree = getParsedScriptBlock();
    callstack.pushDebug("evaluator", command);
    if (tree == nullptr) {
        AbstractSyntaxTree::deleteReferences();
        callstack.popDebug();
        return false;
    }
    AbstractSyntaxTreePtrVector astAsVector = AbstractSyntaxTree::getReferences();
    AbstractSyntaxTree::clearReferences();
    try {
        auto chunk = BytecodeCompiler::compileScript(tree, L"");
        BytecodeVM::executeScript(this, *chunk);
    } catch (NelsonException& e) {
        AbstractSyntaxTree::deleteReferences(astAsVector);
        tree = nullptr;
        setLastErrorException(e);
        if (state == NLS_STATE_ABORT || isQuitOrForceQuitState()) {
            callstack.popDebug();
            throw;
        }
        if (propagateException) {
            throw e;
        }
        setLastUncaughtException(e);
        e.printMe(io);
        callstack.popDebug();
        return false;
    } catch (const std::exception& stdEx) {
        // A raw C++ exception raised inside the compiler or the bytecode VM (not a
        // Nelson error) must never abort the process: convert it to a Nelson error
        // so the command fails cleanly and the prompt survives.
        AbstractSyntaxTree::deleteReferences(astAsVector);
        tree = nullptr;
        NelsonException e(utf8_to_wstring(stdEx.what()), L"Nelson:interpreter:cppRuntimeError");
        setLastErrorException(e);
        if (state == NLS_STATE_ABORT || isQuitOrForceQuitState()) {
            callstack.popDebug();
            throw e;
        }
        if (propagateException) {
            throw e;
        }
        setLastUncaughtException(e);
        e.printMe(io);
        callstack.popDebug();
        return false;
    } catch (...) {
        AbstractSyntaxTree::deleteReferences(astAsVector);
        tree = nullptr;
        NelsonException e(resolveMessageTemplate(L"Nelson:interpreter:unexpectedInternalError",
                              L"An unexpected internal error occurred."),
            L"Nelson:interpreter:unexpectedInternalError");
        setLastErrorException(e);
        if (state == NLS_STATE_ABORT || isQuitOrForceQuitState()) {
            callstack.popDebug();
            throw e;
        }
        if (propagateException) {
            throw e;
        }
        setLastUncaughtException(e);
        e.printMe(io);
        callstack.popDebug();
        return false;
    }
    AbstractSyntaxTree::deleteReferences(astAsVector);
    tree = nullptr;
    if (state == NLS_STATE_RETURN) {
        if (depth > 0) {
            depth--;
        }
    }
    callstack.popDebug();
    return true;
}
//=============================================================================
std::wstring
Evaluator::getCurrentEvaluateFilename()
{
    if (evaluatedFilenames.size() > 0) {
        return evaluatedFilenames[evaluatedFilenames.size() - 1];
    }
    return L"";
}
//=============================================================================
std::string
Evaluator::getCallerFunctionName()
{
    int ipos = (int)callstack.size() - 2;
    if (ipos >= 0) {
        return callstack.getContext(ipos);
    }
    return {};
}
//=============================================================================
std::wstring
Evaluator::getCallerFunctionNameW()
{
    return utf8_to_wstring(getCallerFunctionName());
}
//=============================================================================
std::wstring
Evaluator::getCurrentFunctionNameW()
{
    return utf8_to_wstring(getCurrentFunctionName());
}
//=============================================================================
std::string
Evaluator::getCurrentFunctionName()
{
    int ipos = (int)callstack.size() - 1;
    if (ipos >= 0) {
        std::string fullname = callstack.getLastContext();
        if (StringHelpers::ends_with(fullname, ".m")) {
            FileSystemWrapper::Path pathForStem(fullname);
            return pathForStem.stem().string();
        }
        return fullname;
    }
    return {};
}
//=============================================================================
void
Evaluator::pushEvaluateFilenameList(const std::wstring& filename)
{
    evaluatedFilenames.push_back(filename);
}
//=============================================================================
void
Evaluator::popEvaluateFilenameList()
{
    evaluatedFilenames.pop_back();
}
//=============================================================================
std::wstring
Evaluator::buildPrompt()
{
    std::wstring prompt;
    if (isBreakpointActive()) {
        prompt += L"K";
    }
    prompt += L">> ";
    return prompt;
}
//=============================================================================
bool
Evaluator::haveEventsLoop()
{
    return this->_haveEventsLoop;
}
//=============================================================================
void
Evaluator::setCommandLineArguments(wstringVector args)
{
    this->commandLineArguments = std::move(args);
}
//=============================================================================
wstringVector
Evaluator::getCommandLineArguments()
{
    return this->commandLineArguments;
}
//=============================================================================
void
Evaluator::setUserCommandLineArguments(wstringVector args)
{
    this->userCommandLineArguments = std::move(args);
}
//=============================================================================
wstringVector
Evaluator::getUserCommandLineArguments()
{
    return this->userCommandLineArguments;
}
//=============================================================================
bool
Evaluator::getEchoMode()
{
    return bEchoMode;
}
//=============================================================================
void
Evaluator::setEchoMode(bool _mode)
{
    bEchoMode = _mode;
}
//=============================================================================
bool
Evaluator::isQuietMode()
{
    return bQuietMode;
}
//=============================================================================
void
Evaluator::setQuietMode(bool _quiet)
{
    bQuietMode = _quiet;
}
//=============================================================================
void
Evaluator::addCommandToQueue(const std::wstring& command, bool bIsPriority)
{
    wchar_t ch = *command.rbegin();
    if (ch != L'\n') {
        this->commandQueue.add(command + L"\n", bIsPriority);
    } else {
        this->commandQueue.add(command, bIsPriority);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
