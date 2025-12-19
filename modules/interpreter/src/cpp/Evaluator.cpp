//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
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
#include <regex>
#ifdef _MSC_VER
#include <Windows.h>
#else
#include <unistd.h>
#endif
#include "Evaluator.hpp"
#include "NelsonConfiguration.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadName.hpp"
#include "ClassName.hpp"
#include "ClassToString.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "ParserInterface.hpp"
#include "LexerInterface.hpp"
#include "Interface.hpp"
#include "NelsonReadyNamedMutex.hpp"
#include "CallbackQueue.hpp"
#include "EventQueue.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Constructor: Initializes the Evaluator with context, interface, and state.
Evaluator::Evaluator(Context* aContext, Interface* aInterface, bool haveEventsLoop, size_t ID)
{
    this->ID = ID;
    Exception e;
    this->setLastErrorException(e);
    this->setLastWarningException(e);
    context = aContext;
    resetState();
    depth = 0;
    _haveEventsLoop = haveEventsLoop;
    io = aInterface;
    autostop = true;
    InCLI = false;
    debugActive = false;
    inStepMode = false;
    bpActive = false;
    clearStacks();
    commandLineArguments.clear();
    lineNumber = 0;
}
//=============================================================================
// Destructor: Cleans up the Evaluator by clearing stacks and resetting exceptions.
Evaluator::~Evaluator()
{
    clearStacks();
    commandLineArguments.clear();
    resetLastErrorException();
    resetLastWarningException();
}
//=============================================================================
// Signal handler for interrupt: Sets the interrupt pending flag in the configuration.
void
sigInterrupt(int arg)
{
    NelsonConfiguration::getInstance()->setInterruptPending(true, 0);
}
//=============================================================================
// Clears the call stack and breakpoint stack.
void
Evaluator::clearStacks()
{
    callstack.clear();
}
//=============================================================================
// Resets the state of the evaluator to NLS_STATE_OK.
void
Evaluator::resetState()
{
    state = NLS_STATE_OK;
}
//=============================================================================
// Sets the state of the evaluator to a new state and returns the previous state.
State
Evaluator::setState(State newState)
{
    State previousState = state;
    state = newState;
    return previousState;
}
//=============================================================================
// Returns the current state of the evaluator.
State
Evaluator::getState()
{
    return state;
}
//=============================================================================
// Checks if the evaluator is in a quit or force quit state.
bool
Evaluator::isQuitOrForceQuitState()
{
    return ((state == NLS_STATE_QUIT) || (state == NLS_STATE_FORCE_QUIT));
}
//=============================================================================
// Returns the exit code of the evaluator.
int
Evaluator::getExitCode()
{
    return exitCode;
}
//=============================================================================
// Sets the exit code of the evaluator.
void
Evaluator::setExitCode(int _exitCode)
{
    exitCode = _exitCode;
}
//=============================================================================
// Checks if an operator needs to be overloaded based on the data class of the array.
bool
Evaluator::needToOverloadOperator(const ArrayOf& a)
{
    return ((a.getDataClass() == NLS_STRUCT_ARRAY) || (a.getDataClass() == NLS_CELL_ARRAY)
        || (a.getDataClass() == NLS_STRING_ARRAY) || a.isSparse() || a.isHandle()
        || (a.getDataClass() == NLS_CLASS_ARRAY) || (a.getDataClass() == NLS_FUNCTION_HANDLE));
}
//=============================================================================
// Returns the context in which the evaluator operates.
Context*
Evaluator::getContext()
{
    return context;
}
//=============================================================================
// Returns the command queue associated with the evaluator.
stringVector
Evaluator::getCallers(bool includeCurrent)
{
    stringVector callersName;
    size_t i = 0;
    while (i < callstack.size()) {
        if (callstack.getID(i) == 0) {
            size_t j = i + 1;
            CallStack callstackRef = callstack;
            while ((j < callstack.size()) && (callstack.getContext(j) == callstack.getContext(i))
                && (callstack.getDetail(j) == callstack.getDetail(i))
                && (callstack.getID(j) != 0)) {
                j++;
            }
            std::string functionname = callstack.getDetail(j - 1);
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
                // remove all that is not functions
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
//======================================================================
// Sets the interface for the evaluator.
void
Evaluator::setInterface(Interface* _io)
{
    io = _io;
}
//======================================================================
// Returns the interface associated with the evaluator.
Interface*
Evaluator::getInterface()
{
    return io;
}
//=============================================================================
// Returns the ID of the evaluator.
size_t
Evaluator::getID()
{
    return ID;
}
//=============================================================================
// Evaluates a string command in the evaluator context.
bool
Evaluator::evaluateString(const std::wstring& line, bool propogateException)
{
    return evaluateString(wstring_to_utf8(line), propogateException);
}
//=============================================================================
// Evaluates a string command in the evaluator context.
bool
Evaluator::evaluateString(const std::string& line, bool propogateException)
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

    // we add <RETURN> at the end
    // the command need a <RETURN> to be correctly parser
    if (ch != '\n') {
        command = command + "\n";
    }
    AbstractSyntaxTree::clearReferences();
    try {
        parserState = parseString(lexerContext, command);
    } catch (Exception& e) {
        AbstractSyntaxTree::deleteReferences();
        resetParser();
        setLastErrorException(e);
        if (propogateException) {
            throw;
        }
        e.printMe(io);
        return false;
    }
    if (parserState != ScriptBlock) {
        AbstractSyntaxTree::deleteReferences();
        resetParser();
        Exception e(_W("a valid script expected."));
        setLastErrorException(e);
        if (propogateException) {
            throw;
        }
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
        block(tree);
    } catch (Exception& e) {
        AbstractSyntaxTree::deleteReferences(astAsVector);
        tree = nullptr;
        setLastErrorException(e);
        if (propogateException) {
            throw e;
        }
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
// Returns the current evaluated filename from the list of evaluated filenames.
std::wstring
Evaluator::getCurrentEvaluateFilename()
{
    if (evaluatedFilenames.size() > 0) {
        return evaluatedFilenames[evaluatedFilenames.size() - 1];
    }
    return L"";
}
//=============================================================================
// Returns the name of the caller function from the call stack.
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
// Returns the name of the caller function in wide string format from the call stack.
std::wstring
Evaluator::getCallerFunctionNameW()
{
    return utf8_to_wstring(getCallerFunctionName());
}
//=============================================================================
// Returns the name of the current function from the call stack.
std::wstring
Evaluator::getCurrentFunctionNameW()
{
    return utf8_to_wstring(getCurrentFunctionName());
}
//=============================================================================
// Returns the name of the current function from the call stack.
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
// Pushes a filename onto the list of evaluated filenames.
void
Evaluator::pushEvaluateFilenameList(const std::wstring& filename)
{
    evaluatedFilenames.push_back(filename);
}
//=============================================================================
// Pops the last filename from the list of evaluated filenames.
void
Evaluator::popEvaluateFilenameList()
{
    evaluatedFilenames.pop_back();
}
//=============================================================================
// Returns the current debug depth of the evaluator.
std::wstring
Evaluator::buildPrompt()
{
    std::wstring prompt;
    if (depth > 0) {
        if (bpActive) {
            prompt = std::to_wstring(depth) + L"D>> ";
        } else {
            prompt = std::to_wstring(depth) + L">> ";
        }
    } else {
        if (bpActive) {
            prompt = L"D>> ";
        } else {
            prompt = L">> ";
        }
    }
    return prompt;
}
//=============================================================================
void
setNamedMutexNelsonReady()
{
    // Use portable process id retrieval
#ifdef _MSC_VER
    openIsReadyNelsonMutex(static_cast<int>(GetCurrentProcessId()));
#else
    openIsReadyNelsonMutex(static_cast<int>(getpid()));
#endif
}
//=============================================================================
static bool doOnce = true;
//=============================================================================
// Evaluates commands in the CLI mode, processing input from the command queue or user input.
void
Evaluator::evalCLI()
{
    while (true) {
        if (!bpActive) {
            clearStacks();
        }

        if (EventQueue::getInstance()->processCallback(this)) {
            return;
        }
        if (CallbackQueue::getInstance()->processCallback(this)) {
            return;
        }

        std::wstring commandLine;
        commandQueue.get(commandLine);
        if (commandLine.empty()) {
            if (doOnce) {
                setNamedMutexNelsonReady();
                doOnce = false;
            }
            commandLine = io->getLine(buildPrompt());
            if (commandLine.empty()) {
                InCLI = false;
                this->setState(NLS_STATE_QUIT);
                return;
            }
            wchar_t ch = *commandLine.rbegin();
            if (ch != L'\n') {
                commandLine.push_back(L'\n');
            }
        }
        // Scan the line and tokenize it
        AbstractSyntaxTree::clearReferences();
        setLexBuffer(lexerContext, commandLine);
        try {
            bool bContinueLine = lexCheckForMoreInput(lexerContext, 0);
            AbstractSyntaxTree::deleteReferences();
            if (bContinueLine) {
                int lastCount = getContinuationCount(lexerContext);
                std::wstring lines = commandLine;
                bool enoughInput = false;
                int emptyLineCount = 0; // Track consecutive empty lines

                if (lexerContext.lineNumber > 2) {
                    enoughInput = true;
                }

                while (!enoughInput) {
                    std::wstring prompt = L""; // Default continuation prompt

                    // Show helpful continuation prompt based on context
                    if (lexerContext.bracketStackSize > 0) {
                        switch (lexerContext.bracketStack[lexerContext.bracketStackSize - 1]) {
                        case '[':
                            prompt = L"[> ";
                            break;
                        case '{':
                            prompt = L"{> ";
                            break;
                        case '(':
                            prompt = L"(> ";
                            break;
                        }
                    } else if (lexerContext.inFunction) {
                        prompt = L"f> "; // Function continuation
                    } else if (lexerContext.inStatement > 0) {
                        prompt = L"s> "; // Statement continuation
                    }

                    commandLine = io->getLine(prompt);

                    // Handle empty lines - allow double empty line to force evaluation
                    if (commandLine == L"\n" || commandLine.empty()) {
                        emptyLineCount++;

                        if (NelsonConfiguration::getInstance()->getInterruptPending(ID)) {
                            commandLine.clear();
                            return;
                        }

                        // Double empty line forces evaluation (user override)
                        if (emptyLineCount >= 2) {
                            enoughInput = true;
                            break;
                        }

                        // Single empty line - check if we can safely terminate
                        if (isSafeToAutoTerminateCLI()) {
                            enoughInput = true;
                            break;
                        }
                        continue;
                    } else {
                        emptyLineCount = 0; // Reset counter on non-empty input
                    }
                    wchar_t ch = *commandLine.rbegin();
                    if (ch != L'\n') {
                        commandLine.push_back(L'\n');
                    }
                    lines.append(commandLine);
                    AbstractSyntaxTree::clearReferences();
                    setLexBuffer(lexerContext, lines);
                    enoughInput = !lexCheckForMoreInput(lexerContext, lastCount);
                    AbstractSyntaxTree::deleteReferences();
                    lastCount = getContinuationCount(lexerContext);
                    if (enoughInput) {
                        lines.append(L"\n");
                    }
                }
                commandLine = std::move(lines);
            }
        } catch (Exception& e) {
            e.printMe(io);
            commandLine.clear();
        }
        InCLI = true;
        if (!commandLine.empty()) {
            size_t stackdepth = callstack.size();
            if (lexerContext.lineNumber > 1) {
                auto normalize_newlines = [](const std::wstring& input) -> std::wstring {
                    static const std::wregex newline_re(L"\n+");
                    return std::regex_replace(input, newline_re, L"\n");
                };
                commandLine = normalize_newlines(commandLine);
            }
            bool evalResult = evaluateString(commandLine, false);
            while (callstack.size() > stackdepth) {
                callstack.pop_back();
            }
            if (!evalResult || isQuitOrForceQuitState() || this->getState() == NLS_STATE_ABORT) {
                InCLI = false;
                return;
            }
        }
    }
}
//=============================================================================
// Checks if it is safe to auto-terminate the CLI based on the current lexer context.
bool
Evaluator::isSafeToAutoTerminateCLI()
{
    // Only allow termination if we're just missing simple end statements
    // but not if we have unbalanced brackets
    if (lexerContext.bracketStackSize > 0) {
        return false; // Never terminate with unbalanced brackets
    }

    // Allow termination if we're only missing 'end' keywords
    // This handles cases like incomplete if/for/while blocks
    return (lexerContext.inStatement >= 0 && !lexerContext.inFunction);
}
//=============================================================================
// Checks if the evaluator has an event loop.
bool
Evaluator::haveEventsLoop()
{
    return this->_haveEventsLoop;
}
//=============================================================================
// Sets the command line arguments for the evaluator.
void
Evaluator::setCommandLineArguments(wstringVector args)
{
    this->commandLineArguments = std::move(args);
}
//=============================================================================
// Returns the command line arguments for the evaluator.
wstringVector
Evaluator::getCommandLineArguments()
{
    return this->commandLineArguments;
}
//=============================================================================
// Returns the echo mode of the evaluator.
bool
Evaluator::getEchoMode()
{
    return bEchoMode;
}
//=============================================================================
// Sets the echo mode of the evaluator.
void
Evaluator::setEchoMode(bool _mode)
{
    bEchoMode = _mode;
}
//=============================================================================
// Checks if the evaluator is in quiet mode, where it suppresses output messages.
bool
Evaluator::isQuietMode()
{
    return bQuietMode;
}
//=============================================================================
// Sets the quiet mode of the evaluator, where it suppresses output messages.
void
Evaluator::setQuietMode(bool _quiet)
{
    bQuietMode = _quiet;
}
//=============================================================================
// Returns the overload function name based on the class and function type.
void
Evaluator::setHandle(ArrayOf r, const std::string& fieldname, const ArrayOfVector& fieldvalue)
{
    if (fieldvalue.size() != 1) {
        Error(_W("Right hand values must satisfy left hand side expression."));
    }
    std::string currentType;
    if (r.isGraphicsObject()) {
        currentType = ClassToString(r.getDataClass());
    } else {
        currentType = r.getHandleCategory();
    }
    std::string functionNameSetHandle = getOverloadFunctionName(currentType, "set");
    Context* _context = this->getContext();
    FunctionDef* funcDef = nullptr;
    if (!_context->lookupFunction(functionNameSetHandle, funcDef)) {
        Error(_W("Function not found."));
    }
    if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION))) {
        Error(_W("Type function not valid."));
    }
    int nLhs = 0;
    ArrayOfVector argIn;
    argIn.push_back(r);
    argIn.push_back(ArrayOf::characterArrayConstructor(fieldname));
    argIn.push_back(fieldvalue[0]);
    funcDef->evaluateFunction(this, argIn, nLhs);
}
//=============================================================================
// Retrieves the handle of an object based on its type and field name, using the provided
// parameters.
ArrayOfVector
Evaluator::getHandle(ArrayOf r, const std::string& fieldname, const ArrayOfVector& params)
{
    ArrayOfVector argIn;
    std::string currentType;
    std::string functionNameCurrentType;
    if (r.isGraphicsObject()) {
        currentType = ClassToString(r.getDataClass());
        functionNameCurrentType = getOverloadFunctionName(ClassName(r), fieldname);
    } else {
        currentType = r.getHandleCategory();
        functionNameCurrentType = getOverloadFunctionName(currentType, fieldname);
    }
    Context* _context = this->getContext();
    FunctionDef* funcDef = nullptr;
    if (_context->lookupFunction(functionNameCurrentType, funcDef)) {
        if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                || (funcDef->type() == NLS_MACRO_FUNCTION))) {
            Error(_W("Type function not valid."));
        }
        int nLhs = 1;
        argIn.reserve(params.size() + 1);
        argIn.push_back(r);
        for (const ArrayOf& a : params) {
            argIn.push_back(a);
        }
        return funcDef->evaluateFunction(this, argIn, nLhs);
    }
    std::string functionNameGetHandle = getOverloadFunctionName(currentType, "get");
    if (!context->lookupFunction(functionNameGetHandle, funcDef)) {
        Error(_("Function not found: ") + functionNameGetHandle);
    }
    if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION))) {
        Error(_W("Type function not valid."));
    }
    int nLhs = 1;
    argIn.push_back(r);
    argIn.push_back(ArrayOf::characterArrayConstructor(fieldname));
    return funcDef->evaluateFunction(this, argIn, nLhs);
}
//=============================================================================
// Checks if the given object has a method with the specified name.
bool
Evaluator::isObjectMethod(const ArrayOf& r, const std::string& methodName)
{
    Context* _context = this->getContext();
    FunctionDef* funcDef = nullptr;
    if (_context->lookupFunction("ismethod", funcDef)) {
        int nLhs = 1;
        ArrayOfVector argIn;
        argIn.push_back(r);
        argIn.push_back(ArrayOf::characterArrayConstructor(methodName));
        ArrayOfVector res = funcDef->evaluateFunction(this, argIn, nLhs);
        if (res.size() != 1) {
            return false;
        }
        if (res[0].isLogical() && res[0].isScalar()) {
            return res[0].getContentAsLogicalScalar();
        }
    }
    if (r.isHandle()) {
        return r.isHandleMethod(utf8_to_wstring(methodName));
    }
    return false;
}
//=============================================================================
// Invokes a method on the given object with the specified parameters.
ArrayOfVector
Evaluator::invokeMethod(
    const ArrayOf& r, const std::string& methodName, const ArrayOfVector& params)
{
    Context* _context = this->getContext();
    FunctionDef* funcDef = nullptr;

    std::string currentType = r.isHandle() ? r.getHandleCategory()
        : r.isClassType()                  ? r.getClassType()
                                           : "";

    if (!currentType.empty()) {
        std::string functionNameCurrentType = getOverloadFunctionName(currentType, "invoke");
        if (_context->lookupFunction(functionNameCurrentType, funcDef)) {
            ArrayOfVector argIn;
            int nLhs = 1;
            argIn.push_back(r);
            argIn.push_back(ArrayOf::characterArrayConstructor(methodName));
            for (const ArrayOf& a : params) {
                argIn.push_back(a);
            }
            return funcDef->evaluateFunction(this, argIn, nLhs);
        }
        return getHandle(r, methodName, params);
    }
    Error(_("Function not found: ") + "invoke");
    return {};
}
//=============================================================================
// Adds a command to the command queue, ensuring it ends with a newline character.
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
