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
#include <atomic>
#include <cstdint>
#include "nlsInterpreter_exports.h"
#include "AbstractSyntaxTree.hpp"
#include "ArrayOf.hpp"
#include "CommandQueue.hpp"
#include "Context.hpp"
#include "FunctionDef.hpp"
#include "Interface.hpp"
#include "StackEntry.hpp"
#include "NelsonException.hpp"
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
//=============================================================================
class Context;
class Breakpoint
{
public:
    // stable identifier given by Evaluator::addBreakpoint (0: not stored yet)
    uint64_t id = 0;
    std::wstring filename;
    std::string functionName;
    size_t line = 0;
    bool enabled = true;
    // Nelson expression evaluated in the stopped scope, empty = unconditional
    std::wstring condition;
    bool stepMode = false;
    // one-shot "run to here" breakpoint, consumed on hit, never listed
    bool runToLine = false;
    bool stepNext = false; // When true, break on first executed line after 'fromLine'
    bool stepInto = false;
    bool stepOut = false;
    size_t maxLines = 0;
    int targetDepth = -1; // Used for step out: callstack depth to stop at
    size_t fromLine = 0; // Used for stepNext: starting line to search after

    bool
    isUserBreakpoint() const
    {
        return !stepMode && !runToLine;
    }
};
//=============================================================================
enum class DebugStopReason : uint8_t
{
    None = 0,
    Breakpoint,
    Step,
    Keyboard,
    Pause,
    Error,
    RunToLine
};
//=============================================================================
// Where and why the debugger is stopped, read by the editors and the web RPC.
struct DebugStopInfo
{
    DebugStopReason reason = DebugStopReason::None;
    std::wstring filename;
    std::string functionName;
    size_t line = 0;
    size_t maxLine = 0;
    size_t column = 0;
    uint64_t breakpointId = 0;
    std::wstring errorIdentifier;
    std::wstring errorMessage;
    bool canStep = true;
};
//=============================================================================
// Outcome of a conditional breakpoint expression.
enum class BreakpointConditionOutcome : uint8_t
{
    True = 0,
    False,
    NonScalar,
    Error
};
//=============================================================================
/**
 * Runtime state and services shared by the bytecode VM, debugger, profiler,
 * parser, and
 * command-line evaluator.
 */
class NLSINTERPRETER_IMPEXP Evaluator
{
    std::vector<Breakpoint> breakpoints;
    uint64_t nextBreakpointId = 1;
    bool stopOnError = false;
    bool stopOnCaughtError = false;
    // per-statement gate read by the VM: bit 0 enabled breakpoint, bit 1 pause requested
    static constexpr uint32_t STATEMENT_HOOK_BREAKPOINTS = 1u;
    static constexpr uint32_t STATEMENT_HOOK_PAUSE = 2u;
    std::atomic<uint32_t> statementHookFlags { 0 };
    std::atomic<uint64_t> breakpointsRevision { 0 };
    std::atomic<uint64_t> debugStateRevision { 0 };
    DebugStopInfo debugStopInfo;
    // candidate selected by onBytecodeBreakpoint, confirmed by confirmPendingDebugStop
    struct PendingDebugStop
    {
        DebugStopReason reason = DebugStopReason::None;
        uint64_t breakpointId = 0;
        std::optional<Breakpoint> previousStepBreakpoint;
    } pendingStop;
    // > 0: breakpoint, pause and error hooks are inert (condition or data tip evaluation)
    int debugHooksSuppressDepth = 0;
    // > 0: a command typed at the K>> prompt is running
    int debugPromptNesting = 0;
    // active try regions over all VM frames, and active C++ catchers
    int vmTryDepth = 0;
    int cppCatchDepth = 0;
    // one error stop per unwinding
    bool errorStopTaken = false;
    bool errorAlreadyDisplayed = false;

    wstringVector commandLineArguments;
    wstringVector userCommandLineArguments;

    Context* context;
    stringVector classdefAccessContextStack;
    std::vector<std::pair<std::string, std::string>> classdefPropertySetContextStack;
    // Names of the classes whose constructor is currently running (a stack, so
    // the @Super chain and nested construction are covered). Used to allow
    // immutable (SetAccess) property assignments only during construction.
    stringVector classdefConstructionStack;
    State state;
    /**
     * The debug depth.  Each time the command line interface is invoked
     * via a pause command, the debug depth is increased.
     */
    int depth = 0;
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

    NelsonException lastUncaughtException;

public:
    bool bpActive = false;

    bool stepMode = false;

    std::optional<Breakpoint> stepBreakpoint;

    bool
    isBreakpointActive()
    {
        return bpActive;
    }

    // breakpoint table (interpreter thread only); a second dbstop on a line replaces its condition
    uint64_t
    addBreakpoint(const Breakpoint& bp);
    void
    clearBreakpoints();
    void
    clearStepBreakpoints();
    void
    clearRunToLineBreakpoints();
    bool
    removeBreakpoint(const std::wstring& filename, size_t line);
    bool
    removeBreakpointById(uint64_t id);
    size_t
    removeAllBreakpoints(const std::wstring& filename = L"");
    std::vector<Breakpoint>
    getBreakpoints() const;
    // user breakpoints (disabled ones included, temporaries excluded), sorted by line
    std::vector<Breakpoint>
    getUserBreakpoints(const std::wstring& filename = L"") const;
    std::vector<size_t>
    getBreakpointLines(const std::wstring& filename) const;
    const Breakpoint*
    findBreakpoint(const std::wstring& filename, size_t line) const;
    const Breakpoint*
    findBreakpointById(uint64_t id) const;
    bool
    hasEnabledBreakpoints() const;
    bool
    hasBreakpoint(const std::wstring& filename, size_t line) const;
    bool
    stepBreakpointExists(const Breakpoint& bp);
    bool
    setBreakpointEnabled(const std::wstring& filename, size_t line, bool enabled);
    size_t
    setAllBreakpointsEnabled(bool enabled, const std::wstring& filename = L"");
    // parse-only check of a condition, errorMessage is the text on failure
    bool
    validateBreakpointCondition(const std::wstring& condition, std::wstring& errorMessage);
    bool
    setBreakpointCondition(const std::wstring& filename, size_t line, const std::wstring& condition,
        std::wstring& errorMessage);
    bool
    moveBreakpoint(const std::wstring& filename, size_t oldLine, size_t newLine);
    // one-shot breakpoint for "Run to Here", the line is adjusted like dbstop does
    uint64_t
    addRunToLineBreakpoint(const std::wstring& filename, size_t line, std::wstring& errorMessage);

    // dbstop if error / dbstop if caught error
    void
    setStopOnError(bool on);
    bool
    getStopOnError() const;
    void
    setStopOnCaughtError(bool on);
    bool
    getStopOnCaughtError() const;
    bool
    hasErrorBreakpoints() const;

    // Pause button: stop at the next statement of user code (thread-safe).
    bool
    requestPause();
    void
    cancelPauseRequest();
    bool
    isPauseRequested() const;
    bool
    isAtPrompt() const;

    const DebugStopInfo&
    getDebugStopInfo() const;
    // bumped on every breakpoint mutation / every debug stop change
    uint64_t
    getBreakpointsRevision() const;
    uint64_t
    getDebugStateRevision() const;
    void
    noteDebugStateChanged();

    // Hot gate used by the VM on every statement: a single relaxed load.
    bool
    debuggerStatementHookArmed() const
    {
        return statementHookFlags.load(std::memory_order_relaxed) != 0;
    }
    // Gate of the call fast paths: also armed by error breakpoints and an active session.
    bool
    debuggerHooksArmed() const
    {
        return debuggerStatementHookArmed() || stopOnError || stopOnCaughtError || bpActive
            || stepMode;
    }

    bool
    adjustBreakpointLine(const std::wstring& filename, size_t requestedLine, size_t& adjustedLine,
        std::wstring& errorMessage);

    // VM hooks: onBytecodeBreakpoint picks a candidate stop, confirmPendingDebugStop checks it
    bool
    onBytecodeBreakpoint(const std::wstring& filename, const std::string& functionName,
        size_t currentLine, size_t maxLine);
    bool
    confirmPendingDebugStop(size_t column);

    // `keyboard` statement reached: records its location as the current
    // stop so that dbstep can resume from it.
    void
    onBytecodeKeyboard(const std::wstring& filename, const std::string& functionName,
        size_t currentLine, size_t maxLine);

    // dbstop if error support: willBeCaught tells whether a catcher up the stack swallows the error
    bool
    shouldStopOnError(bool caughtInCurrentFrame, bool& willBeCaught);
    void
    beginErrorStop(const NelsonException& e, const std::wstring& fallbackFilename,
        const std::string& fallbackFunctionName, size_t fallbackLine);
    void
    clearErrorStopTaken();
    // the error of an uncaught error stop was already printed before the K>> prompt
    void
    markErrorDisplayed();
    bool
    consumeErrorDisplayed();
    // leaving the K>> prompt: drop the dbup/dbdown scope bypasses taken inside it
    void
    restoreDebugScopes(size_t bypassedScopeCountAtEntry, int depthAtEntry);
    int&
    vmTryDepthRef();
    void
    pushCppErrorCatcher();
    void
    popCppErrorCatcher();
    void
    pushDebugHooksSuppression();
    void
    popDebugHooksSuppression();
    bool
    debugHooksSuppressed() const;
    void
    pushDebugPromptCommand();
    void
    popDebugPromptCommand();

    // execution came back to the top-level prompt: drop temporaries and pending requests
    void
    onExecutionReturnedToTopLevel();
    // the K>> prompt was left to resume execution: no stop is current any more
    void
    markDebugResumed();
    // prints "<line>   <source>" when execution stops
    void
    displayDebugStopLine();

    // Hooks are inert while an instance is alive (condition / data tip evaluation).
    class DebugHooksSuppressor
    {
    public:
        explicit DebugHooksSuppressor(Evaluator* eval) : m_eval(eval)
        {
            m_eval->pushDebugHooksSuppression();
        }
        ~DebugHooksSuppressor() { m_eval->popDebugHooksSuppression(); }
        DebugHooksSuppressor(const DebugHooksSuppressor&) = delete;
        DebugHooksSuppressor&
        operator=(const DebugHooksSuppressor&)
            = delete;

    private:
        Evaluator* m_eval;
    };
    // Marks a C++ catcher that swallows errors (eval with catch, run with errcatch).
    class ScopedErrorCatcher
    {
    public:
        explicit ScopedErrorCatcher(Evaluator* eval) : m_eval(eval)
        {
            m_eval->pushCppErrorCatcher();
        }
        ~ScopedErrorCatcher() { m_eval->popCppErrorCatcher(); }
        ScopedErrorCatcher(const ScopedErrorCatcher&) = delete;
        ScopedErrorCatcher&
        operator=(const ScopedErrorCatcher&)
            = delete;

    private:
        Evaluator* m_eval;
    };

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

    Context*
    getContext();

    stringVector
    getCallers(bool includeCurrent);

    CommandQueue commandQueue;

    /**
     * ID must be unique.
     */
    Evaluator(Context* aContext, Interface* aInterface, bool haveEventLoop, size_t ID);
    ~Evaluator();

    void
    clearStacks();
    void
    resetState();
    State
    setState(State newState);
    State
    getState();
    bool
    isQuitOrForceQuitState();
    int
    getExitCode();
    void
    setExitCode(int _exitCode);
    bool
    lookupFunction(const std::string& funcName, FunctionDefPtr& val);

    void
    pushClassdefAccessContext(const std::string& className);
    void
    popClassdefAccessContext();
    std::string
    getClassdefAccessContext() const;
    // true inside a method of className or of one of its superclasses
    bool
    isClassdefMethodContextOf(const std::string& className) const;
    // a single-level obj.name access belongs to the overloaded subsref/subsasgn
    // (indexingOverloaded) when it comes from outside the methods of the class
    bool
    isOverloadedDotAccess(const std::string& className, bool indexingOverloaded) const;
    // a single-level obj.name access reaches the declared property directly:
    // exact name, and no overloaded subsref/subsasgn (indexingOverloaded) or
    // an access from a method of the class
    bool
    isDirectPropertyAccess(const std::string& className, const std::string& propertyName,
        bool indexingOverloaded) const;

    void
    pushClassdefPropertySetContext(const std::string& className, const std::string& propertyName);
    void
    popClassdefPropertySetContext();
    bool
    isCurrentClassdefPropertySetContext(
        const std::string& className, const std::string& propertyName) const;

    void
    pushClassdefConstruction(const std::string& className);
    void
    popClassdefConstruction();
    bool
    isConstructingClass(const std::string& className) const;

    void
    setInterface(Interface* _io);
    Interface*
    getInterface();
    Interface*
    getDefaultInterface();
    ArrayOfVector
    subsindexOperator(const ArrayOfVector& m);

    /**
     * Start a command line interface. Statements are read from the console and
     * executed until a "return" statement runs or the user presses CTRL-D.
     */
    void
    evalCLI();

    void
    debugCLI();

    bool
    evaluateString(const std::string& line, bool propagateException = true);
    bool
    evaluateString(const std::wstring& line, bool propagateException = true);

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
    void
    setUserCommandLineArguments(wstringVector args);
    wstringVector
    getUserCommandLineArguments();

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

    NelsonException
    getLastErrorException();
    bool
    setLastErrorException(const NelsonException& e);

    void
    resetLastErrorException();

    NelsonException
    getLastUncaughtException();
    bool
    setLastUncaughtException(const NelsonException& e);
    void
    resetLastUncaughtException();

    NelsonException
    getLastWarningException();
    bool
    setLastWarningException(const NelsonException& e);
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

    // `end` of a value without an end overload, used as subscript index of count
    static double
    endSubscriptValue(const ArrayOf& v, indexType index, size_t count);

    ArrayOfVector
    bytecodeGetHandle(ArrayOf r, const std::string& fieldname, const ArrayOfVector& params);

    ArrayOfVector
    bytecodeGetOrInvokeHandle(
        const ArrayOf& r, const std::string& fieldname, const ArrayOfVector& params);

    bool
    bytecodeInvokeObjectMethodIfExists(const ArrayOf& r, const std::string& methodName,
        const ArrayOfVector& params, int nLhs, ArrayOfVector& result);

    bool
    bytecodeInvokeObjectMethodIfExistsWithInputs(const ArrayOf& r, const std::string& methodName,
        const ArrayOfVector& argIn, int nLhs, ArrayOfVector& result);

    void
    bytecodeSetHandle(ArrayOf r, const std::string& fieldname, const ArrayOfVector& fieldvalue);

    ArrayOfVector
    bytecodeExtractClass(const ArrayOf& r, const stringVector& subtypes,
        const ArrayOfVector& subsindices, bool& haveFunction, int nLhs = 1);

    ArrayOf
    bytecodeAssignClass(const ArrayOf& r, const stringVector& subtypes,
        const ArrayOfVector& subsindices, const ArrayOf& value, bool& haveFunction);

private:
    ArrayOfVector
    extractClass(const ArrayOf& r, const stringVector& subtypes, const ArrayOfVector& subsindices,
        bool& haveFunction, int nLhs = 1);

    void
    setHandle(ArrayOf r, const std::string& fieldname, const ArrayOfVector& fieldvalue);
    ArrayOfVector
    getHandle(ArrayOf r, const std::string& fieldname, const ArrayOfVector& params);
    // h.name read/write on a classdef handle: overloaded subsref/subsasgn or
    // declared property; false leaves the access to the generic handle path
    bool
    tryReadHandleDot(const ArrayOf& r, const std::string& fieldname, ArrayOfVector& result);
    bool
    tryReadHandleDotIndex(const ArrayOf& r, const std::string& fieldname,
        const ArrayOfVector& params, ArrayOfVector& result);
    bool
    tryAssignHandleDot(const ArrayOf& r, const std::string& fieldname, const ArrayOf& value);
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
    processCliCallbacks();
    bool
    readCliCommandLine(std::wstring& commandLine);
    void
    resetDebugPromptIfExecutionFinished();
    bool
    completeCliCommandLine(std::wstring& commandLine);
    std::wstring
    getCliContinuationPrompt() const;
    void
    appendCliNewline(std::wstring& commandLine);
    bool
    evaluateCliCommand(std::wstring commandLine);
    bool
    handleCliEvaluationResult(bool evalResult);
    bool
    isSafeToAutoTerminateCLI();

    void
    resolveBytecodeBreakpointContext(const std::wstring& bytecodeFilename,
        const std::string& bytecodeFunctionName, size_t currentLine, std::wstring& filename,
        std::string& currentFunctionName, size_t& maxLine);
    bool
    handleActiveStepNextBreakpoint(const std::wstring& filename,
        const std::string& currentFunctionName, size_t currentLine, size_t maxLine);
    bool
    handleQueuedStepNextBreakpoint(const std::wstring& filename,
        const std::string& currentFunctionName, size_t currentLine, size_t maxLine);
    bool
    handleStepOutBreakpoint(const std::wstring& filename, const std::string& currentFunctionName,
        size_t currentLine, size_t maxLine);
    bool
    handleLineBreakpoint(const std::wstring& filename, const std::string& currentFunctionName,
        size_t currentLine, size_t maxLine, bool requireFunctionName);
    bool
    handleStepIntoBreakpoint(const std::wstring& filename, const std::string& currentFunctionName,
        size_t currentLine, size_t maxLine);
    bool
    handlePauseRequest(const std::wstring& filename, const std::string& currentFunctionName,
        size_t currentLine, size_t maxLine);
    Breakpoint*
    findMutableBreakpointById(uint64_t id);
    Breakpoint*
    findMutableUserBreakpoint(const std::wstring& filename, size_t line);
    void
    noteBreakpointsChanged();
    void
    refreshEnabledBreakpointsFlag();
    BreakpointConditionOutcome
    evaluateBreakpointCondition(const std::wstring& condition);
    void
    fillDebugStopInfoFromStepBreakpoint(
        DebugStopReason reason, uint64_t breakpointId, size_t column);

    size_t ID;

    size_t
    getLinePosition(AbstractSyntaxTreePtr t);

    size_t
    maxLinePosition(AbstractSyntaxTreePtr t);
};
//=============================================================================
NLSINTERPRETER_IMPEXP void
sigInterrupt(int arg);
//=============================================================================
// a stop reached from an editor command keeps the console quiet: the flag serves one stop
NLSINTERPRETER_IMPEXP void
setNextDebugStopQuiet(bool quiet);
NLSINTERPRETER_IMPEXP bool
takeNextDebugStopQuiet();
//=============================================================================
} // namespace Nelson
//=============================================================================
