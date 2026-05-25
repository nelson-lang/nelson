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
#pragma once
//=============================================================================
#include <set>
#include <map>
#include <optional>

#include "nlsInterpreter_exports.h"
#include "AbstractSyntaxTree.hpp"
#include "ArrayOf.hpp"
#include "CommandQueue.hpp"
#include "Context.hpp"
#include "FunctionDef.hpp"
#include "Interface.hpp"
#include "StackEntry.hpp"
#include "Exception.hpp"
#include "CallStack.hpp"
#include "LexerContext.hpp"
//=============================================================================
namespace Nelson {

enum State
{
    NLS_STATE_OK = 0,
    NLS_STATE_BREAK = 1,
    NLS_STATE_CONTINUE = 2,
    NLS_STATE_RETURN = 3,
    NLS_STATE_QUIT = 4,
    NLS_STATE_FORCE_QUIT = 5,
    NLS_STATE_CANCEL_QUIT = 6,
    NLS_STATE_ABORT = 7,
    NLS_STATE_DEBUG_CONTINUE = 8,
    NLS_STATE_DEBUG_STEP = 9,
    NLS_STATE_DEBUG_QUIT = 10,
    NLS_STATE_DEBUG_QUIT_ALL = 11,
};

class Context;

class Breakpoint
{
public:
    std::wstring filename;
    std::string functionName;
    size_t line = 0;
    bool enabled = true;
    bool stepMode = false;
    bool stepNext = false; // When true, break on first executed line after 'fromLine'
    bool stepInto = false;
    bool stepOut = false;
    size_t maxLines = 0;
    int targetDepth = -1; // Used for step out: callstack depth to stop at
    size_t fromLine = 0; // Used for stepNext: starting line to search after
};

/**
 * Runtime state and services shared by the bytecode VM, debugger, profiler,
 * parser, and
 * command-line evaluator.
 */
class NLSINTERPRETER_IMPEXP Evaluator
{
    std::vector<Breakpoint> breakpoints;

    wstringVector commandLineArguments;

    /**
     * The context that the intepreter operates in.
     */
    Context* context;
    stringVector classdefAccessContextStack;
    /**
     * The current state of the interpreter.
     */
    State state;
    /**
     * The debug depth.  Each time the command line interface is invoked
     * via a pause command, the debug depth is increased.
     */
    int depth = 0;
    /**
     * The interface for I/O
     */
    Interface* io;
    /**
     * The default (interactive) interface, saved at construction.
     * Used to restore interactive I/O during debug sessions
     * when the current interface has been temporarily replaced
     * (e.g., by evalc).
     */
    Interface* defaultInterface = nullptr;

    int exitCode = 0;

    bool InCLI;

    bool bEchoMode = true;

    bool bQuietMode = false;

public:
    bool bpActive = false;

    bool stepMode = false;

    std::optional<Breakpoint> stepBreakpoint;

    bool
    isBreakpointActive()
    {
        return bpActive;
    }

    void
    addBreakpoint(const Breakpoint& bp);
    void
    clearBreakpoints();
    void
    clearStepBreakpoints();
    bool
    removeBreakpoint(const std::wstring& filename, size_t line);
    std::vector<Breakpoint>
    getBreakpoints() const;
    std::vector<size_t>
    getBreakpointLines(const std::wstring& filename) const;
    bool
    hasBreakpoint(const std::wstring& filename, size_t line) const;
    bool
    stepBreakpointExists(const Breakpoint& bp);

    bool
    adjustBreakpointLine(const std::wstring& filename, size_t requestedLine, size_t& adjustedLine,
        std::wstring& errorMessage);

    bool
    onBytecodeBreakpoint(const std::wstring& filename, const std::string& functionName,
        size_t currentLine, size_t maxLine);

    bool
    dbDown(int n);
    bool
    dbUp(int n);

    LexerContext lexerContext;

    size_t
    getID();

    std::vector<std::wstring> evaluatedFilenames;

    bool isReadyToUse = false;

    CallStack callstack;

    bool withOverload = true;

    std::wstring
    buildPrompt();

    /**
     * Get the context we are running with.
     */
    Context*
    getContext();

    stringVector
    getCallers(bool includeCurrent);

    /* Command Queue */
    CommandQueue commandQueue;

    /**
     * Construct a Evaluator object with the given context to operate
     * in.
     * ID must be unique
     */
    Evaluator(Context* aContext, Interface* aInterface, bool haveEventLoop, size_t ID);
    /**
     * Destruct the Evaluator object.
     */
    ~Evaluator();

    void
    clearStacks();
    /**
     * Reset the state of the tree walker.
     */
    void
    resetState();
    /**
     * Set the state of the tree walker.
     */
    State
    setState(State newState);
    /**
     * Get the state of the tree walker.
     */
    State
    getState();
    /**
     * check if state is QUIT or FORCE_QUIT
     */
    bool
    isQuitOrForceQuitState();
    /**
     * Get exit code.
     */
    int
    getExitCode();
    /**
     * Set exit code.
     */
    void
    setExitCode(int _exitCode);
    /**
     * Look up an identifier as a potential function name
     */
    bool
    lookupFunction(const std::string& funcName, FunctionDefPtr& val);

    void
    pushClassdefAccessContext(const std::string& className);
    void
    popClassdefAccessContext();
    std::string
    getClassdefAccessContext() const;

    void
    setInterface(Interface* _io);
    Interface*
    getInterface();
    Interface*
    getDefaultInterface();
    ArrayOfVector
    subsindexOperator(const ArrayOfVector& m);

    /**
     * Start a command line interface.  Statements are retrieved
     * from the console,
     * and executed sequentially until a "return" statement is executed or the user presses
     * 'CTRL-D'.
     */
    void
    evalCLI();

    void
    debugCLI();

    /**
     * The workhorse routine - "evaluate" the contents of a string
     * and execute it.
     */
    bool
    evaluateString(const std::string& line, bool propogateException = true);
    bool
    evaluateString(const std::wstring& line, bool propogateException = true);

    std::wstring
    getCurrentEvaluateFilename();
    void
    pushEvaluateFilenameList(const std::wstring& filename);
    void
    popEvaluateFilenameList();

    /*
     * time value used by tic toc
     */
    uint64 TimerValue = 0;
    std::string
    getCallerFunctionName();
    std::wstring
    getCallerFunctionNameW();

    std::string
    getCurrentFunctionName();
    std::wstring
    getCurrentFunctionNameW();

    void
    resetDebugDepth();
    int
    getDebugDepth();
    void
    increaseDebugDepth();
    void
    decreaseDebugDepth();

    bool
    haveEventsLoop();

    void
    setCommandLineArguments(wstringVector args);
    wstringVector
    getCommandLineArguments();

    bool
    getEchoMode();
    void
    setEchoMode(bool _mode);

    bool
    isQuietMode();
    void
    setQuietMode(bool _quiet);

    void
    addCommandToQueue(const std::wstring& command, bool bIsPriority = false);

    /**
     * Get the last error that occurred.
     */
    Exception
    getLastErrorException();
    bool
    setLastErrorException(const Exception& e);

    /**
     * reset last error
     */
    void
    resetLastErrorException();

    /**
     * Get the last warning that occurred.
     */
    Exception
    getLastWarningException();
    /**
     * Set the text for the last warning.
     */
    bool
    setLastWarningException(const Exception& e);
    /**
     * reset last warning
     */
    void
    resetLastWarningException();

    ArrayOf
    colonUnitOperator(const ArrayOf& A, const ArrayOf& B);
    ArrayOf
    colonOperator(const ArrayOf& A, const ArrayOf& B, const ArrayOf& C);

    ArrayOf
    uplusOperator(const ArrayOf& A);
    ArrayOf
    uminusOperator(const ArrayOf& A);

    ArrayOf
    transposeOperator(const ArrayOf& A);
    ArrayOf
    complexTransposeOperator(const ArrayOf& A);

    ArrayOf
    mpowerOperator(const ArrayOfVector& args);
    ArrayOf
    powerOperator(const ArrayOfVector& args);

    ArrayOf
    plusOperator(const ArrayOfVector& args);
    ArrayOf
    minusOperator(const ArrayOfVector& args);
    ArrayOf
    timesOperator(const ArrayOfVector& args);
    ArrayOf
    mtimesOperator(const ArrayOfVector& args);

    ArrayOf
    rightDivideOperator(const ArrayOfVector& args);

    ArrayOf
    leftDivideOperator(const ArrayOfVector& args);

    ArrayOf
    dotRightDivideOperator(const ArrayOfVector& args);

    ArrayOf
    dotLeftDivideOperator(const ArrayOfVector& args);

    ArrayOf
    eqOperator(const ArrayOfVector& args);
    ArrayOf
    gtOperator(const ArrayOfVector& args);
    ArrayOf
    geOperator(const ArrayOfVector& args);
    ArrayOf
    leOperator(const ArrayOfVector& args);
    ArrayOf
    ltOperator(const ArrayOfVector& args);
    ArrayOf
    neOperator(const ArrayOfVector& args);

    ArrayOf
    orOperator(const ArrayOfVector& args);
    ArrayOf
    andOperator(const ArrayOfVector& args);
    ArrayOf
    notOperator(const ArrayOf& A);

    ArrayOf
    vertcatOperator(const ArrayOfVector& v);
    ArrayOf
    horzcatOperator(const ArrayOfVector& v);

    void
    display(
        const ArrayOf& A, const std::string& name, bool asDispBuiltin, bool withProfiling = false);

    ArrayOf
    bytecodeEndReference(const ArrayOf& v, indexType index, size_t count);

    ArrayOfVector
    bytecodeGetHandle(ArrayOf r, const std::string& fieldname, const ArrayOfVector& params);

    ArrayOfVector
    bytecodeGetOrInvokeHandle(
        const ArrayOf& r, const std::string& fieldname, const ArrayOfVector& params);

    bool
    bytecodeInvokeObjectMethodIfExists(const ArrayOf& r, const std::string& methodName,
        const ArrayOfVector& params, int nLhs, ArrayOfVector& result);

    void
    bytecodeSetHandle(ArrayOf r, const std::string& fieldname, const ArrayOfVector& fieldvalue);

    ArrayOfVector
    bytecodeExtractClass(const ArrayOf& r, const stringVector& subtypes,
        const ArrayOfVector& subsindices, bool& haveFunction);

    ArrayOf
    bytecodeAssignClass(const ArrayOf& r, const stringVector& subtypes,
        const ArrayOfVector& subsindices, const ArrayOf& value, bool& haveFunction);

private:
    ArrayOfVector
    extractClass(const ArrayOf& r, const stringVector& subtypes, const ArrayOfVector& subsindices,
        bool& haveFunction);

    void
    setHandle(ArrayOf r, const std::string& fieldname, const ArrayOfVector& fieldvalue);
    ArrayOfVector
    getHandle(ArrayOf r, const std::string& fieldname, const ArrayOfVector& params);
    ArrayOf
    EndReference(const ArrayOf& v, indexType index, size_t count);
    ArrayOf
    EndOverloadReference(const ArrayOf& v, indexType index, size_t count, bool& needToBeOverloaded);

    bool
    isObjectMethod(const ArrayOf& r, const std::string& methodName);
    ArrayOfVector
    invokeMethod(const ArrayOf& r, const std::string& methodName, const ArrayOfVector& params);

    bool
    needToOverloadOperator(const ArrayOf& a);

    bool _haveEventsLoop;

    bool
    isSafeToAutoTerminateCLI();

    size_t ID;

    size_t
    getLinePosition(AbstractSyntaxTreePtr t);

    size_t
    getMaxLinePosition(AbstractSyntaxTreePtr t);
};
NLSINTERPRETER_IMPEXP void
sigInterrupt(int arg);
} // namespace Nelson
