//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
// Copyright (c) 2002, 2003 Samit Basu
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

#pragma once
#include "AST.hpp"
#include "ArrayOf.hpp"
#include "CommandQueue.hpp"
#include "Context.hpp"
#include "FunctionDef.hpp"
#include "Error.hpp"
#include "Interface.hpp"
#include "StackEntry.hpp"
#include "Warning.hpp"
#include "PositionScript.hpp"
#include "nlsInterpreter_exports.h"
#include "Exception.hpp"
#include <stack>
#include <vector>

namespace Nelson {

typedef enum
{
    NLS_STATE_OK = 0,
    NLS_STATE_BREAK = 1,
    NLS_STATE_CONTINUE = 2,
    NLS_STATE_RETURN = 3,
    NLS_STATE_QUIT = 4,
    NLS_STATE_ABORT = 5
} State;

class Context;

/**
 * This is the class that implements the interpreter - it generally
 * operates on abstract syntax trees (ASTs).
 */
class NLSINTERPRETER_IMPEXP Evaluator
{
    wstringVector commandLineArguments;

    bool bAllowOverload;

    int engineMode;

    /**
     * The context that the intepreter operates in.
     */
    Context* context;
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
     * The last error that occured.
     */
    Exception lastErrorException;

    /**
     * The last warning that occured.
     */
    Exception lastWarningException;

    /**
     * autostop storage flag
     */
    bool autostop;
    /**
     * When this flag is active, autostop does nothing.
     */

    int exitCode = 0;

    // The debug stack - this stack tracks our current location
    // in each file, as well as the files themselves.

    std::vector<StackEntry> bpStack;
    bool inStepMode;
    int lineNumber;
    StackEntry stepTrap;
    bool bpActive;

    bool InCLI;

    std::wstring
    buildPrompt();

    bool bEchoMode = true;

    bool bQuietMode = false;

    // by default, overload on basic types is called after hardcoded operators.
    // faster but double, single, char, logical, integers does not allow overload.
    // "overloadbasictypes(true)" modify this behavior.
    // overload on basic types can be usefull (example: code generator).
    bool overloadOnBasicTypes = false;

public:
    std::vector<std::wstring> evaluatedFilenames;

    void* mainGuiObject = nullptr;

    void* FileManager = nullptr;

    void* RandomEngine = nullptr;

    void* HistoryManager = nullptr;

    std::vector<StackEntry> cstack;
    void
    setCLI(bool bCLI);
    bool
    getCLI();

    /**
     * Get the context we are running with.
     */
    Context*
    getContext();

    bool debugActive;

    void
    dbstep(int linecount);
    void
    handleDebug(int fullcontext);
    void
    debugCLI();
    void
    pushDebug(std::string fname, std::string detail);
    void
    popDebug();

    void
    addBreakpoint(StackEntry& bp);
    bool
    adjustBreakpoint(StackEntry& bp, bool dbstep);
    void
    adjustBreakpoints();
    void
    listBreakpoints();
    void
    deleteBreakpoint(int number);

    stringVector
    getCallers(bool includeCurrent);

    /* Command Queue */
    CommandQueue commandQueue;

    /**
     * Construct a Evaluator object with the given context to operate
     * in.
     */
    Evaluator(Context* aContext, Interface* aInterface, int engineMode);
    /**
     * Destruct the Evaluator object.
     */
    ~Evaluator();
    /**
     * Push the given location ID onto the stack
     */
    void
    pushID(int);
    /**
     * Pop a location ID off the stack
     */
    void
    popID();
    /**
     * Set the autostop flag - this flag determines what happens when
     * an exception occurs
     */
    bool
    AutoStop();
    void
    AutoStop(bool a);

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
     * Get current overload state (enabled by default)
     */
    bool
    isOverloadAllowed();
    /**
     * disable overload
     */
    void
    disableOverload();
    /**
     * enable overload
     */
    void
    enableOverload();
    /**
     *  return current overload basic types
     */
    bool
    mustOverloadBasicTypes();
    /**
     * enable overload basic type
     */
    void
    enableOverloadBasicTypes();
    /**
     * disable overload basic type
     */
    void
    disableOverloadBasicTypes();
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
     * Convert an expression list into a vector of ArrayOf variables.
     */
    ArrayOfVector
    rowDefinition(ASTPtr t);
    /**
     * Convert a matrix definition of the form: [expr1,expr2;expr3;expr4] into
     * a vector of row definitions.  The first row is the vector [expr1,expr2],
     * and the second is the vector [expr3,expr4].  The AST input should look
     * like:
     *  []
     *   |
     *   ;-> ; -> ... -> NULL
     *   |   |
     *   |   rowDef
     *   rowDef
     */
    ArrayOf
    matrixDefinition(ASTPtr t);
    /**
     * Convert a cell defintion of the form: {expr1,expr2;expr3;expr4} into
     * a vector of row definitions.  The first row is the vector {expr1,expr2},
     * and the second is the vector {expr3,expr4}.  The AST input should look
     * like:
     *  {}
     *   |
     *   ;-> ; -> ... -> NULL
     *   |   |
     *   |   rowDef
     *   rowDef
     */
    ArrayOf
    cellDefinition(ASTPtr t);
    /**
     * Evaluate the expression pointed to by the AST t into a variable.
     */
    ArrayOf
    expression(ASTPtr t);
    /**
     * Evaluate a unit colon expression.  The AST input should look like:
     *   :
     *   |
     *   expr1->expr2
     *
     * The output is the expression expr1:expr2, which is the vector
     * [expr1,expr1+1,expr1+2,...,expr1+n], where n is the largest
     * integer such that expr1+n <= expr2.
     */
    ArrayOf
    unitColon(ASTPtr t);
    /**
     * Evaluate a double colon expression.  The AST input should look like:
     *   :
     *   |
     *   :->expr3
     *   |
     *   expr1->expr2
     * The output is the expression expr1:expr2:expr3, which is the
     * vector [expr1,expr1+expr2,expr1+2*expr2,...,expr1+n*expr2], where
     * n is the largest integer such that expr1+n*expr2 <= expr3.
     */
    ArrayOf
    doubleColon(ASTPtr t);
    /**
     * Process a sequence of expressions into a vector of ArrayOfs.
     * The input AST must be:
     *   expr1->expr2->expr3...->NULL
     * If the dim argument is non-NULL, then before the nth expression
     * is evaluated, the end value is set to the nth dimension length.
     * Also, if one of the expressions is a multi-valued RHS expression,
     * then it is expanded into the result.  Also, if one of
     * the expressions is the ':' token, then the corresponding
     * expression is replaced with either 1:length (if the ':' is
     * a singleton) or 1:dim->getDimensionLength(k).  This is only
     * valid if we are a subindexing expression list (i.e.,
     * VAR(exprssionlist)), in which case dim != nullptr.
     */
    ArrayOfVector
    expressionList(ASTPtr t);
    ArrayOfVector
    expressionList(ASTPtr t, ArrayOf subRoot);
    /**
     * The RHS expression is used to represent an rvalue in an
     * assignment statement (or an implicit assignment such as
     * -->2+3).  The form of the AST depends on the head.  If the
     * head identifier is a function name, then the form of the
     * AST is:
     *    ident
     *     |
     *     ()
     *     |
     *     expr1->expr2->...
     * On the other hand, if the identifier represents a variable
     * (variables are checked for first, incidentally), then the AST
     * looks like:
     *    ident
     *     |
     *     ()->{}->.
     *     |   |   |
     *     |   |   field
     *     |   expr3->...
     *     expr1->expr2->...
     * Throws an Exception
     *    - if the identifier cannot be matched to
     *      either a variable or function.
     *    -
     */
    ArrayOfVector
    rhsExpression(ASTPtr t);
    /**
     * Look up an identifier as a potential function name
     */
    bool
    lookupFunction(std::string funcName, FuncPtr& val);
    /**
     * Special case the single assignment statement 'A = B' for speed.
     */
    inline ArrayOf
    rhsExpressionSimple(ASTPtr t);

    void
    setInterface(Interface* _io);
    Interface*
    getInterface();
    /**
     * Process an AST to form an lvalue in an assignment statement.
     * The AST looks like:
     *    ident
     *     |
     *     ()->{}->.
     *     |   |   |
     *     |   |   field
     *     |   expr3->...
     *     expr1->expr2->...
     * The lhsExpression method creates a LeftHandSide object that
     * captures the original object and the subindexing expressions
     * applied to it.  Throws an Exception if the indexing expressions
     * are empty.
     */
    ArrayOf
    simpleSubindexExpression(ArrayOf& r, ASTPtr t);
    ArrayOfVector
    subsindex(ArrayOfVector m);

    indexType
    countLeftHandSides(ASTPtr t);

    void
    simpleAssign(ArrayOf& r, ASTPtr t, ArrayOf& value);

    void
    simpleAssign(ArrayOf& r, ASTPtr t, ArrayOfVector& value);

    ArrayOf
    assignExpression(ASTPtr t, ArrayOf& value);

    ArrayOf
    assignExpression(ASTPtr t, ArrayOfVector& value);

    /**
     * Evaluate a function and return the results of the function as
     * an ArrayOfVector.  For scripts, the body of the function is
     * evaluated directly.  Otherwise, the function evaluates itself
     * (using the FunctionDef::evaluateFunction member function).  The
     * arguments to the function are unpacked from the AST ast follows
     *   ident
     *    |
     *    ()
     *    |
     *    expr1->expr2->...
     * The outputOptional flag allows the function to not assign an
     * output.
     * Throws an exception if
     *    - something other than a parenthesis pair "()" follows the
     *      identifier.
     *    - if too many arguments are passed to the function.
     *    - too many outputs are requested from the function.
     */
    ArrayOfVector
    functionExpression(FunctionDef* funcDef, ASTPtr t, int narg_out, bool outputOptional);
    /**
     * A multifunction call is an expression of the sort
     * [expr1,expr2,...,exprn] = func(args).  The AST is
     *    multiCall
     *       |
     *       []->functionExpression_AST
     *       |
     *       ;->NULL
     *       |
     *       rowDefs
     * When calculating the number of left hand sides for the
     * multifunction, single variables without subindexing expressions
     * are counted as a single left hand side.  Thus, an expression
     * such as [A{1:3},B] = func(args) counts for four left hand
     * sides, regardless of the contents of B.
     * If the printIt flag is set, each of the outputs is to be
     * written to the console.
     * Throws an exception if the AST is malformed (i.e., the '[]' is
     * missing, or there are multiple rows in the left hand side.).
     */
    void
    multiFunctionCall(ASTPtr t, bool printIt);
    /**
     * A special function call is an expression of the form
     * >> func arg1 arg2 arg3
     * which is represented in an AST is
     *     scall
     *       |
     *       fname->arg
     */
    ArrayOfVector
    specialFunctionCall(ASTPtr t, bool printIt);
    /**
     * Test a conditional expression, and if its true, evaluate
     * the associated block of code.  Used by a number of control
     * constructs.  The AST looks like:
     *     cstat
     *      |
     *     expr->codeBlock
     * The expression output is treated as false if the real part
     * is all zeros.  Throws an Exception if the head of the
     * AST is not a cstat.
     */
    bool
    conditionedStatement(ASTPtr t);
    /**
     * Handles an if statement, corresponding to an if, a number
     * of elseif blocks and an optional else statement.  The AST looks
     * like
     *     cstat->elseIfBlock->elseCode
     *      |        |
     *      |       cstat->cstat->cstat
     *     expr->codeBlock
     * where each of the elseIf blocks are tested sequentially until
     * one of them succeeds.
     */
    void
    ifStatement(ASTPtr t);
    /**
     * Handle a switch statement.  The switch statement tests
     * an expression against a number of case blocks.  If a
     * case block matches, the code in the case block is
     * executed, and the switch statement quits.  If none of the
     * case blocks match, the otherwise code is executed. The
     * AST looks like
     *     expr->caseBlock->otherwiseCode
     *              |
     *             testCase->testCase->...->NULL
     * Where testCase is the AST passed to testCaseStatement. For
     * a switch statement, the switch value must be either a
     * scalar value or a string.  The test values in each case
     * statement can either be the same type, or a cell array.
     * If it is a cell array, then the switch value is compared
     * with each entry in the case expression.
     * Throws an Exception if the switch expression is not
     * either a scalar or a string.
     */
    void
    switchStatement(ASTPtr t);
    /**
     * Implements the for control statement.  The AST looks like
     *     ident->codeBlock
     *       |
     *      expr
     * The identifier used as the control variable in the for
     * statement.  The variable defined by the identifier takes
     * on each of the values in the expression.  For each such
     * assignment, the code in the codeBlock is executed.
     */
    void
    forStatement(ASTPtr t);
    /**
     * Implements the while control statement.  The AST looks like
     *     expr->codeBlock
     * The test expression is evaluated until it fails, and for each
     * successful expression, the codeBlock is executed.
     */
    void
    whileStatement(ASTPtr t);
    /**
     * Implements the try statement.  The AST looks like
     *     block->catchBlock
     * The code in block is executed.  If an exception occurs, then
     * the code in catchBlock is executed.
     */
    void
    tryStatement(ASTPtr t);
    /**
     * This somewhat strange test is used by the switch statement.
     * If x is a scalar, and we are a scalar, this is an equality
     * test.  If x is a string and we are a string, this is a
     * strcmp test.  If x is a scalar and we are a cell-array, this
     * test is applied on an element-by-element basis, looking for
     * any matches.  If x is a string and we are a cell-array, then
     * this is applied on an element-by-element basis also.
     * The AST for this looks like:
     *     NLS_CASE
     *       |
     *      expr->codeBlock
     * The expression is compared to x using ArrayOf::testForCaseMatch.
     * If a match is found, then the codeBlock is executed.  The result of
     * the test is returned.  Throws an exception if the AST is
     * malformed.
     */
    bool
    testCaseStatement(ASTPtr t, ArrayOf x);
    /**
     * Execute the statement described by the AST - the printIt flag
     * determines if the result of the statement should be printed to
     * the console.  The form of the AST required depends on the
     * type of statement being executed.  The various statement types
     * handled are as follows:
     *   =          Assignment of expression to LHS
     *   |
     *  LHS->expr
     *
     *  multicall   Multifunction call
     *   |
     *  multicallBody
     *
     *  NLS_FOR      For statement
     *   |
     *  forBody
     *
     *  NLS_WHILE    While statement
     *   |
     *  whileBody
     *
     *  NLS_IF       If statement
     *   |
     *  ifBody
     *
     *  NLS_BREAK    Break statement - change the state of the interpreter to
     *              NLS_STATE_BREAK
     *
     *  NLS_CONTINUE Continue statement - change the state of the interpreter
     *              to NLS_STATE_CONTINUE
     *
     *  NLS_RETURN   Return statement - change the state to NLS_STATE_RETURN
     *
     *  NLS_SWITCH   Switch statement
     *   |
     *  switchBody
     *
     *  NLS_TRY      Try statement
     *   |
     *  tryBody
     *
     *  NLS_PAUSE Enter another CLI session
     *
     *  rhs         A function call being evaluated as a statement
     *   |
     *  funcName
     *
     *  expr        An expression being evaluated as a statement
     *
     * The function call is trapped before the expression to handle
     * the special variable "ans".
     * Throws an Exception if the statement type is not recognized.
     */
    void
    statementType(ASTPtr t, bool printIt);
    /**
     * The statement method simply screens out the need for the
     * printIt flag.  It also retrieves the statement context
     * for use in error reporting.  The AST looks like:
     *   qstatement          A quiet statement (printIt -> false)
     *      |
     *    context (optional)
     *      |
     *  statementBody
     *
     *   rstatement          A normal statement (printIt -> true)
     *      |
     *    context (optional)
     *      |
     *  statementBody
     * The context data is supplied by the parse (indicates the
     * line number and filename if necessary).
     */
    void
    statement(ASTPtr t);
    /**
     * Executes a sequence of statements, trapping exceptions
     * as necessary.  The AST looks like
     *   <ignored>
     *      |
     *    statement->statement->statement
     * If an exception occurs, it is caught and rethrown.  The
     * lasterr string is also set to the contents of the exception.
     *
     */
    void
    block(ASTPtr t);
    /**
     * Start a command line interface.  Statements are retrieved
     * from the console, and executed sequentially until a "return"
     * statement is executed or the user presses 'CTRL-D'.
     */
    void
    evalCLI();
    /**
     * The workhorse routine - "evaluate" the contents of a string
     * and execute it.
     */
    bool
    evaluateString(const std::string& cmdToEvaluate, bool propogateException = true);
    bool
    evaluateString(const std::wstring& cmdToEvaluate, bool propogateException = true);

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

    int
    getDebugDepth();
    void
    increaseDebugDepth();
    void
    decreaseDebugDepth();

    int
    getNelsonEngineMode();
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

    typedef ArrayOf (*UnaryFunction)(const ArrayOf& A);
    typedef ArrayOf (*BinaryFunction)(ArrayOf& A, ArrayOf& B, bool mustRaiseError, bool& bSuccess);
    typedef ArrayOf (*TernaryFunction)(
        ArrayOf& A, ArrayOf& B, ArrayOf& C, bool mustRaiseError, bool& bSuccess);

    ArrayOf
    doBinaryOperatorOverload(
        ArrayOf& A, ArrayOf& B, BinaryFunction functionOperator, std::string functionName);

    ArrayOf
    additionOperator(ArrayOf A, ArrayOf B);
    ArrayOf
    subtractionOperator(ArrayOf A, ArrayOf B);
    ArrayOf
    timesOperator(ArrayOf A, ArrayOf B);
    ArrayOf
    mtimesOperator(ArrayOf A, ArrayOf B);

    ArrayOf
    eqOperator(ArrayOf A, ArrayOf B);
    ArrayOf
    gtOperator(ArrayOf A, ArrayOf B);
    ArrayOf
    geOperator(ArrayOf A, ArrayOf B);
    ArrayOf
    leOperator(ArrayOf A, ArrayOf B);
    ArrayOf
    ltOperator(ArrayOf A, ArrayOf B);
    ArrayOf
    neOperator(ArrayOf A, ArrayOf B);

    ArrayOf
    shortCutOrOperator(ArrayOf A, ArrayOf B);
    ArrayOf
    shortCutAndOperator(ArrayOf A, ArrayOf B);
    ArrayOf
    orOperator(ArrayOf A, ArrayOf B);
    ArrayOf
    andOperator(ArrayOf A, ArrayOf B);
    ArrayOf
    notOperator(ArrayOf A);

private:
    void
    setHandle(ArrayOf r, std::string fieldname, ArrayOfVector fieldvalue);
    ArrayOfVector
    getHandle(ArrayOf r, std::string fieldname, ArrayOfVector params);
    ArrayOf
    EndReference(ArrayOf v, indexType index, size_t count);
    size_t
    countSubExpressions(ASTPtr t);

    ArrayOf
    doUnaryOperatorOverload(ASTPtr t, UnaryFunction functionOperator, std::string functionName);
    ArrayOf
    doBinaryOperatorOverload(ASTPtr t, BinaryFunction functionOperator, std::string functionName);
    ArrayOf
    doTernaryOperatorOverload(ASTPtr t, TernaryFunction functionOperator, std::string functionName);

    /**
     * Handles the logistics of shortcut evaluation
     */
    ArrayOf
    shortCutOrOperator(ASTPtr t);
    ArrayOf
    shortCutAndOperator(ASTPtr t);
    ArrayOf
    orOperator(ASTPtr t);
    ArrayOf
    andOperator(ASTPtr t);

    ArrayOf
    additionOperator(ASTPtr t);
    ArrayOf
    subtractionOperator(ASTPtr t);
    ArrayOf
    timesOperator(ASTPtr t);
    ArrayOf
    mtimesOperator(ASTPtr t);

    ArrayOf
    eqOperator(ASTPtr t);
    ArrayOf
    gtOperator(ASTPtr t);
    ArrayOf
    geOperator(ASTPtr t);
    ArrayOf
    leOperator(ASTPtr t);
    ArrayOf
    ltOperator(ASTPtr t);
    ArrayOf
    neOperator(ASTPtr t);
    ArrayOf
    notOperator(ASTPtr t);

    bool
    needToOverloadOperator(const ArrayOf& a);
};
NLSINTERPRETER_IMPEXP void
sigInterrupt(int arg);
NLSINTERPRETER_IMPEXP void
ExitInterrupt(int arg);
} // namespace Nelson
