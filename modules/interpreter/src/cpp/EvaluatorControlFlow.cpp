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
#define FMT_HEADER_ONLY
#include <fmt/core.h>
#include "Evaluator.hpp"
#include "i18n.hpp"
#include "Error.hpp"
#include "IsValidVariableName.hpp"
#include "CheckIfWhileCondition.hpp"
#include "PredefinedErrorMessages.hpp"
#include "MException.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "NelsonConfiguration.hpp"
#include "CallbackQueue.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
//!
//@Module IF-ELSEIF-ELSE Conditional Statements
//@@Section FLOW
//@@Usage
// The @|if| and @|else| statements form a control structure for
// conditional execution.  The general syntax involves an @|if|
// test, followed by zero or more @|elseif| clauses, and finally
// an optional @|else| clause:
//@[
//  if conditional_expression_1
//    statements_1
//  elseif conditional_expression_2
//    statements_2
//  elseif conditional_expresiion_3
//    statements_3
//  ...
//  else
//    statements_N
//  end
//@]
// Note that a conditional expression is considered true if
// the real part of the result of the expression contains
// any non-zero elements (this strange convention is adopted
// for compatibility with MATLAB).
//@@Examples
// Here is an example of a function that uses an @|if| statement
//@{ if_test.m
// function c = if_test(a)
//  if (a == 1)
//     c = 'one';
//  elseif (a==2)
//     c = 'two';
//  elseif (a==3)
//     c = 'three';
//  else
//     c = 'something else';
//  end
//@}
// Some examples of @|if_test| in action:
//@<
// if_test(1)
// if_test(2)
// if_test(3)
// if_test(pi)
//@>
//!
void
Evaluator::ifStatement(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    bool condStat = conditionedStatement(t);
    if (!condStat) {
        t = t->right;
        // Check for additional conditions
        if (t != nullptr) {
            bool elseifMatched = false;
            if (t->opNum == (OP_ELSEIFBLOCK)) {
                AbstractSyntaxTreePtr s = t->down;
                while (!elseifMatched && s != nullptr) {
                    elseifMatched = conditionedStatement(s);
                    s = s->right;
                }
                t = t->right;
            }
            if (!(elseifMatched || t == nullptr)) {
                block(t);
            }
        }
    }
    callstack.popID();
}
//=============================================================================
//!
//@Module WHILE While Loop
//@@Section FLOW
//@@Usage
// The @|while| loop executes a set of statements as long as
// a the test condition remains @|true|.  The syntax of a
//@|while| loop is
//@[
//  while test_expression
//     statements
//  end
//@]
// Note that a conditional expression is considered true if
// the real part of the result of the expression contains
// any non-zero elements (this strange convention is adopted
// for compatibility with MATLAB).
//@@Examples
// Here is a @|while| loop that adds the integers from @|1|
// to @|100|:
//@<
// accum = 0;
// k=1;
// while (k<100), accum = accum + k; k = k + 1; end
// accum
//@>
//!
void
Evaluator::whileStatement(AbstractSyntaxTreePtr t)
{
    AbstractSyntaxTreePtr testCondition;
    ArrayOf condVar;
    AbstractSyntaxTreePtr codeBlock;
    bool conditionTrue;
    bool breakEncountered;
    callstack.pushID((size_t)t->getContext());
    testCondition = t;
    codeBlock = t->right;
    breakEncountered = false;
    condVar = expression(testCondition);
    conditionTrue = checkIfWhileCondition(condVar);
    context->enterLoop();
    while (conditionTrue && !breakEncountered) {
        block(codeBlock);
        if (state == NLS_STATE_RETURN || state == NLS_STATE_ABORT || isQuitOrForceQuitState()) {
            break;
        }
        if (state == NLS_STATE_CONTINUE) {
            resetState();
        }
        breakEncountered = (state == NLS_STATE_BREAK);
        if (!breakEncountered) {
            condVar = expression(testCondition);
            conditionTrue = checkIfWhileCondition(condVar);
        } else {
            resetState();
        }
    }
    context->exitLoop();
    callstack.popID();
}
//=============================================================================
//!
//@Module FOR For Loop
//@@Section FLOW
//@@Usage
// The @|for| loop executes a set of statements with an
// index variable looping through each element in a vector.
// The syntax of a @|for| loop is one of the following:
//@[
//  for (variable=expression)
//     statements
//  end
//@]
// Alternately, the parenthesis can be eliminated
//@[
//  for variable=expr
//     statements
//  end
//@]
// or alternately, the index variable can be pre-initialized
// with the vector of values it is going to take:
//@[
//  for variable
//     statements
//  end
//@]
// The third form is essentially equivalent to @|for variable=variable|,
// where @|variable| is both the index variable and the set of values
// over which the for loop executes.  See the examples section for
// an example of this form of the @|for| loop.
//@@Examples
// Here we write @|for| loops to add all the integers from
//@|1| to @|100|.  We will use all three forms of the @|for|
// statement.
//@<
// accum = 0;
// for (i=1:100); accum = accum + i; end
// accum
//@>
// The second form is functionally the same, without the
// extra parenthesis
//@<
// accum = 0;
// for i=1:100; accum = accum + i; end
// accum
//@>
// In the third example, we pre-initialize the loop variable
// with the values it is to take
//!
//=============================================================================
template <class T>
void
ForStatementRowVectorComplexHelper(AbstractSyntaxTreePtr codeBlock, NelsonType indexClass,
    ArrayOf& indexSet, indexType elementCount, const std::string& indexVarName, Evaluator* eval)
{
    T* ptrValue = nullptr;
    const T* data = (const T*)indexSet.getDataPointer();
    Scope* scope = eval->getContext()->getCurrentScope();
    if (scope->isLockedVariable(indexVarName)) {
        Error(_W("Redefining permanent variable."));
    }
    for (indexType elementNumber = 0; elementNumber < elementCount; elementNumber++) {
        ArrayOf* ptrVariable = scope->lookupVariable(indexVarName);
        if ((ptrVariable == nullptr) || (ptrVariable->getDataClass() != indexClass)
            || (!ptrVariable->isScalar())) {
            scope->insertVariable(indexVarName,
                ArrayOf(indexClass, Dimensions(1, 1), ArrayOf::allocateArrayOf(indexClass, 1)));
            ptrVariable = scope->lookupVariable(indexVarName);
        }
        ((T*)ptrVariable->getReadWriteDataPointer())[0] = data[2 * elementNumber];
        ((T*)ptrVariable->getReadWriteDataPointer())[1] = data[2 * elementNumber + 1];

        eval->block(codeBlock);

        if (eval->getState() == NLS_STATE_BREAK) {
            eval->resetState();
            break;
        }
        if (eval->getState() == NLS_STATE_RETURN || eval->getState() == NLS_STATE_ABORT
            || eval->isQuitOrForceQuitState()) {
            break;
        }
        if (eval->getState() == NLS_STATE_CONTINUE) {
            eval->resetState();
        }
    }
}
//=============================================================================
// This function handles the row vector case for complex types.
template <class T>
void
ForStatementRowVectorHelper(AbstractSyntaxTreePtr codeBlock, NelsonType indexClass,
    ArrayOf& indexSet, indexType elementCount, const std::string& indexVarName, Evaluator* eval)
{
    const T* data = (const T*)indexSet.getDataPointer();
    Scope* scope = eval->getContext()->getCurrentScope();
    if (scope->isLockedVariable(indexVarName)) {
        Error(_W("Redefining permanent variable."));
    }
    for (indexType elementNumber = 0; elementNumber < elementCount; elementNumber++) {
        ArrayOf* ptrVariable = scope->lookupVariable(indexVarName);
        if ((ptrVariable == nullptr) || (ptrVariable->getDataClass() != indexClass)
            || (!ptrVariable->isScalar())) {
            scope->insertVariable(indexVarName,
                ArrayOf(indexClass, Dimensions(1, 1), ArrayOf::allocateArrayOf(indexClass, 1)));
            ptrVariable = scope->lookupVariable(indexVarName);
        }
        ((T*)ptrVariable->getReadWriteDataPointer())[0] = data[elementNumber];
        eval->block(codeBlock);
        if (eval->getState() == NLS_STATE_BREAK) {
            eval->resetState();
            break;
        }
        if (eval->getState() == NLS_STATE_RETURN || eval->getState() == NLS_STATE_ABORT
            || eval->isQuitOrForceQuitState()) {
            break;
        }
        if (eval->getState() == NLS_STATE_CONTINUE) {
            eval->resetState();
        }
    }
}
//=============================================================================
// This function handles the row vector case for non-complex types.
static void
ForStatemenRowVectorGenericHelper(AbstractSyntaxTreePtr codeBlock, ArrayOf& indexSet,
    indexType elementCount, const std::string& indexVarName, Evaluator* eval)
{
    ArrayOf indexVar;
    for (indexType elementNumber = 0; elementNumber < elementCount; elementNumber++) {
        indexVar = indexSet.getValueAtIndex(elementNumber);
        if (!eval->getContext()->insertVariable(indexVarName, indexVar)) {
            Error(_W("Valid variable name expected."));
        }
        eval->block(codeBlock);
        if (eval->getState() == NLS_STATE_RETURN || eval->getState() == NLS_STATE_ABORT
            || eval->isQuitOrForceQuitState()) {
            break;
        }
        if (eval->getState() == NLS_STATE_CONTINUE) {
            eval->resetState();
        }
        if (eval->getState() == NLS_STATE_BREAK) {
            eval->resetState();
            break;
        }
    }
}
//=============================================================================
// This function handles the case for matrix indexing in a generic way.
static void
ForStatemenMatrixGenericHelper(AbstractSyntaxTreePtr codeBlock, ArrayOf& indexSet,
    indexType elementCount, const std::string& indexVarName, Evaluator* eval)
{
    ArrayOf indexVar;
    for (indexType elementNumber = 0; elementNumber < elementCount; elementNumber++) {
        indexType tmp = indexSet.getRows();
        ArrayOfVector m;
        m.reserve(2);
        m.push_back(ArrayOf::integerRangeConstructor(1, 1, tmp, false));
        m.push_back(ArrayOf::doubleConstructor((double)(elementNumber + 1)));
        indexVar = indexSet.getNDimSubset(m);
        if (!eval->getContext()->insertVariable(indexVarName, indexVar)) {
            Error(_W("Valid variable name expected."));
        }
        eval->block(codeBlock);
        if (eval->getState() == NLS_STATE_RETURN || eval->getState() == NLS_STATE_ABORT
            || eval->isQuitOrForceQuitState()) {
            break;
        }
        if (eval->getState() == NLS_STATE_CONTINUE) {
            eval->resetState();
        }
        if (eval->getState() == NLS_STATE_BREAK) {
            eval->resetState();
            break;
        }
    }
}
//=============================================================================
// This function handles the for statement for row vectors.
void
Evaluator::forStatement(AbstractSyntaxTreePtr t)
{
    indexType elementCount = 0;
    if (t == nullptr) {
        resetState();
        context->exitLoop();
        return;
    }
    callstack.pushID((size_t)t->getContext());

    /* Get the name of the indexing variable */
    std::string indexVarName = t->text;
    /* Evaluate the index set */
    ArrayOf indexSet = expression(t->down);
    if (indexSet.isEmpty()) {
        return;
    }
    if (!IsValidVariableName(indexVarName, true)) {
        Error(_W("Valid variable name expected."));
    }
    /* Get the code block */
    AbstractSyntaxTreePtr codeBlock = t->right;
    bool isRowVector = indexSet.isRowVector();
    if (isRowVector) {
        elementCount = indexSet.getElementCount();
    } else if (indexSet.isColumnVector()) {
        elementCount = 1;
    } else {
        elementCount = indexSet.getColumns();
    }
    context->enterLoop();
    if (isRowVector) {
        switch (indexSet.getDataClass()) {
        case NLS_LOGICAL: {
            ForStatementRowVectorHelper<logical>(
                codeBlock, NLS_LOGICAL, indexSet, elementCount, indexVarName, this);
        } break;
        case NLS_UINT8: {
            ForStatementRowVectorHelper<uint8>(
                codeBlock, NLS_UINT8, indexSet, elementCount, indexVarName, this);
        } break;
        case NLS_INT8: {
            ForStatementRowVectorHelper<int8>(
                codeBlock, NLS_INT8, indexSet, elementCount, indexVarName, this);
        } break;
        case NLS_UINT16: {
            ForStatementRowVectorHelper<uint16>(
                codeBlock, NLS_UINT16, indexSet, elementCount, indexVarName, this);
        } break;
        case NLS_INT16: {
            ForStatementRowVectorHelper<int16>(
                codeBlock, NLS_INT16, indexSet, elementCount, indexVarName, this);
        } break;
        case NLS_UINT32: {
            ForStatementRowVectorHelper<uint32>(
                codeBlock, NLS_UINT32, indexSet, elementCount, indexVarName, this);
        } break;
        case NLS_INT32: {
            ForStatementRowVectorHelper<int32>(
                codeBlock, NLS_INT32, indexSet, elementCount, indexVarName, this);
        } break;
        case NLS_UINT64: {
            ForStatementRowVectorHelper<uint64>(
                codeBlock, NLS_UINT64, indexSet, elementCount, indexVarName, this);
        } break;
        case NLS_INT64: {
            ForStatementRowVectorHelper<int64>(
                codeBlock, NLS_INT64, indexSet, elementCount, indexVarName, this);
        } break;
        case NLS_SINGLE: {
            ForStatementRowVectorHelper<single>(
                codeBlock, NLS_SINGLE, indexSet, elementCount, indexVarName, this);
        } break;
        case NLS_DOUBLE: {
            ForStatementRowVectorHelper<double>(
                codeBlock, NLS_DOUBLE, indexSet, elementCount, indexVarName, this);
        } break;
        case NLS_SCOMPLEX: {
            ForStatementRowVectorComplexHelper<single>(
                codeBlock, NLS_SCOMPLEX, indexSet, elementCount, indexVarName, this);
        } break;
        case NLS_DCOMPLEX: {
            ForStatementRowVectorComplexHelper<double>(
                codeBlock, NLS_DCOMPLEX, indexSet, elementCount, indexVarName, this);
        } break;
        case NLS_CHAR: {
            ForStatementRowVectorHelper<charType>(
                codeBlock, NLS_CHAR, indexSet, elementCount, indexVarName, this);
        } break;
        default: {
            ForStatemenRowVectorGenericHelper(
                codeBlock, indexSet, elementCount, indexVarName, this);
        } break;
        }
    } else {
        ForStatemenMatrixGenericHelper(codeBlock, indexSet, elementCount, indexVarName, this);
    }
    context->exitLoop();
    callstack.popID();
}
//=============================================================================
bool
Evaluator::conditionedStatement(AbstractSyntaxTreePtr t)
{
    bool conditionState;
    if (t->opNum != OP_CSTAT) {
        Error(ERROR_AST_SYNTAX_ERROR);
    }
    AbstractSyntaxTreePtr s = t->down;
    callstack.pushID((size_t)s->getContext());
    ArrayOf condVar;
    condVar = expression(s);
    conditionState = checkIfWhileCondition(condVar);
    AbstractSyntaxTreePtr codeBlock = s->right;
    if (conditionState) {
        block(codeBlock);
    }
    callstack.popID();
    return conditionState;
}
//=============================================================================
/**
 * This somewhat strange test is used by the switch statement.
 * If x is a scalar, and we are a scalar, this is an equality
 * test.  If x is a string and we are a string, this is a
 * strcmp test.  If x is a scalar and we are a cell-array, this
 * test is applied on an element-by-element basis, looking for
 * any matches.  If x is a string and we are a cell-array, then
 * this is applied on an element-by-element basis also.
 */
bool
Evaluator::testCaseStatement(AbstractSyntaxTreePtr t, const ArrayOf& s)
{
    bool caseMatched;
    ArrayOf r;
    callstack.pushID((size_t)t->getContext());
    if (t->type != reserved_node || t->tokenNumber != NLS_KEYWORD_CASE) {
        Error(ERROR_AST_SYNTAX_ERROR);
    }
    t = t->down;
    r = expression(t);
    caseMatched = s.testForCaseMatch(r);
    if (caseMatched) {
        block(t->right);
    }
    callstack.popID();
    return caseMatched;
}
//=============================================================================
//!
//@Module TRY-CATCH Try and Catch Statement
//@@Section FLOW
//@@Usage
// The @|try| and @|catch| statements are used for error handling
// and control.  A concept present in @|C++|, the @|try| and @|catch|
// statements are used with two statement blocks as follows
//@[
//   try
//     statements_1
//   catch
//     statements_2
//   end
//@]
// The meaning of this construction is: try to execute @|statements_1|,
// and if any errors occur during the execution, then execute the
// code in @|statements_2|.  An error can either be a Nelson generated
// error (such as a syntax error in the use of a built in function), or
// an error raised with the @|error| command.
//@@Examples
// Here is an example of a function that uses error control via @|try|
// and @|catch| to check for failures in @|fopen|.
//@{ read_file.m
// function c = read_file(filename)
// try
//   fp = fopen(filename,'r');
//   c = fgetline(fp);
//   fclose(fp);
// catch
//   c = ['could not open file because of error :' lasterr]
// end
//@}
// Now we try it on an example file - first one that does not exist,
// and then on one that we create (so that we know it exists).
//@<
// read_file('this_filename_is_invalid')
// fp = fopen('test_text.txt','w');
// fprintf(fp,'a line of text\n');
// fclose(fp);
// read_file('test_text.txt')
//@>
//!
void
Evaluator::tryStatement(AbstractSyntaxTreePtr t)
{
    // Turn off autostop for this statement block
    bool autostop_save = autostop;
    autostop = false;
    // Get the state of the IDnum stack and the
    // contextStack and the cnameStack
    size_t stackdepth = callstack.size();
    try {
        block(t);
    } catch (const Exception& e) {
        while (callstack.size() > stackdepth) {
            callstack.pop_back();
        }
        t = t->right;
        if (t != nullptr) {
            if (t->type == id_node) {
                std::string variableName = t->text;
                ArrayOf mException = ExceptionToArrayOf(e);
                this->context->insertVariable(variableName, mException);
                t = t->down;
            }
            if (t != nullptr) {
                autostop = autostop_save;
                block(t);
            }
        }
    }
    autostop = autostop_save;
}
//=============================================================================
//!
//@Module SWITCH Switch statement
//@@Section FLOW
//@@Usage
// The @|switch| statement is used to selective execute code
// based on the value of either scalar value or a string.
// The general syntax for a @|switch| statement is
//@[
//  switch(expression)
//    case test_expression_1
//      statements
//    case test_expression_2
//      statements
//    otherwise:
//      statements
//  end
//@]
// The @|otherwise| clause is optional.  Note that each test
// expression can either be a scalar value, a string to test
// against (if the switch expression is a string), or a
//@|cell-array| of expressions to test against.  Note that
// unlike @|C| @|switch| statements, the Nelson @|switch|
// does not have fall-through, meaning that the statements
// associated with the first matching case are executed, and
// then the @|switch| ends.  Also, if the @|switch| expression
// matches multiple @|case| expressions, only the first one
// is executed.
//@@Examples
// Here is an example of a @|switch| expression that tests
// against a string input:
//@{ switch_test.m
// function c = switch_test(a)
//  switch(a)
//    case {'lima beans','root beer'}
//      c = 'food';
//    case {'red','green','blue'}
//      c = 'color';
//    otherwise
//      c = 'not sure';
//  end
//@}
// Now we exercise the switch statements
//@<
// switch_test('root beer')
// switch_test('red')
// switch_test('carpet')
//@>
//!
void
Evaluator::switchStatement(AbstractSyntaxTreePtr t)
{
    ArrayOf switchVal;
    callstack.pushID(t->getContext());
    // First, extract the value to perform the switch on.
    switchVal = expression(t);
    // Assess its type to determine if this is a scalar switch
    // or a string switch.
    if (!switchVal.isScalar() && !switchVal.isRowVectorCharacterArray()) {
        Error(ERROR_SWITCH_STATEMENTS);
    }
    // Move to the next node in the AST
    t = t->right;
    // Check for additional conditions
    if (t != nullptr) {
        bool caseMatched = false;
        if (t->opNum == (OP_CASEBLOCK)) {
            AbstractSyntaxTreePtr s = t->down;
            while (!caseMatched && s != nullptr) {
                caseMatched = testCaseStatement(s, switchVal);
                s = s->right;
            }
        }
        t = t->right;
        if (!(caseMatched || (t == nullptr)))
        // Do the "otherwise" code
        {
            block(t);
        }
    }
    callstack.popID();
}
//=============================================================================
void
Evaluator::statementType(AbstractSyntaxTreePtr t, bool printIt)
{
    // Process pending events if an event loop is active
    if (haveEventsLoop()) {
        ProcessEventsDynamicFunctionWithoutWait();
    }

    // Execute any queued command before processing the statement
    if (!commandQueue.isEmpty()) {
        std::wstring cmd;
        commandQueue.get(cmd);
        evaluateString(cmd);
    }

    if (t == nullptr) {
        return;
    }

    callstack.pushID((size_t)t->getContext());

    // Handle debugging (breakpoints, step mode, etc.)
    handleDebug(t->getContext());

    // Handle empty statement
    if (t->isEmpty()) {
        callstack.popID();
        return;
    }

    // Handle assignment statement
    if (t->opNum == OP_ASSIGN) {
        assignStatement(t->down, printIt);
        callstack.popID();
        return;
    }

    // Handle multi-function assignment (e.g. [a, b] = func(...))
    if (t->opNum == OP_MULTICALL) {
        multiFunctionCall(t->down, printIt);
        callstack.popID();
        return;
    }

    // Handle special function call syntax
    if (t->opNum == OP_SCALL) {
        ArrayOfVector m = specialFunctionCall(t->down, printIt);
        if (!m.empty()) {
            context->insertVariable("ans", m[0]);
            display(m[0], "ans", false, true);
        }
        callstack.popID();
        return;
    }

    // Handle reserved statements (for, while, if, break, continue, etc.)
    if (t->type == reserved_node) {
        switch (t->tokenNumber) {
        case NLS_KEYWORD_FOR:
            forStatement(t->down);
            break;
        case NLS_KEYWORD_WHILE:
            whileStatement(t->down);
            break;
        case NLS_KEYWORD_IF:
            ifStatement(t->down);
            break;
        case NLS_KEYWORD_BREAK:
            if (context->inLoop())
                state = NLS_STATE_BREAK;
            break;
        case NLS_KEYWORD_CONTINUE:
            if (context->inLoop())
                state = NLS_STATE_CONTINUE;
            break;
        case NLS_KEYWORD_RETURN:
            state = NLS_STATE_RETURN;
            break;
        case NLS_KEYWORD_SWITCH:
            switchStatement(t->down);
            break;
        case NLS_KEYWORD_TRY:
            tryStatement(t->down);
            break;
        case NLS_KEYWORD_ABORT:
            state = NLS_STATE_ABORT;
            depth = 0;
            break;
        case NLS_KEYWORD_KEYBOARD:
            depth++;
            evalCLI();
            if (state < NLS_STATE_QUIT)
                resetState();
            depth--;
            break;
        case NLS_KEYWORD_ENDFUNCTION:
            // Only allowed inside a function scope
            if (context->getCurrentScope()->getName() == "base") {
                Error(ERROR_ENDFUNCTION_WRONG_USE);
            }
            state = NLS_STATE_RETURN;
            break;
        default:
            Error(ERROR_UNRECOGNIZED_STATEMENT);
        }
        callstack.popID();
        return;
    }

    // Handle function call or expression statement
    ArrayOf b;
    ArrayOfVector m;
    bool bUpdateAns = true;
    FunctionDef* fdef = nullptr;

    // Function call as statement (no output required)
    if (t->opNum == OP_RHS && !context->lookupVariable(t->down->text, b)
        && lookupFunction(t->down->text, fdef)) {
        m = functionExpression(fdef, t->down, 0, true);
        if (!m.empty()) {
            b = m[0];
        } else {

            bUpdateAns = false;
        }
        if (printIt && !m.empty() && state < NLS_STATE_QUIT) {
            display(b, "ans", false, true);
        }
    }
    // Variable or function handle call
    else if (t->opNum == OP_RHS) {
        bUpdateAns = false;
        if (context->lookupVariable(t->down->text, b) && b.isFunctionHandle()) {
            m = rhsExpression(t->down, 0);
        } else {
            if (b.isCell()) {
                try {
                    m = rhsExpression(t->down);
                } catch (Exception& e) {
                    if (!e.matches(ERROR_EMPTY_EXPRESSION))
                        throw;
                }
            } else {
                m = rhsExpression(t->down);
            }
        }
        if (m.empty()) {
            b = ArrayOf::emptyConstructor();
        } else {
            b = m[0];
            if (printIt && state < NLS_STATE_QUIT) {
                if (b.name().empty()) {
                    bUpdateAns = true;
                }
                for (size_t j = 0; j < m.size(); j++) {
                    if (m.size() > 1) {
                        std::string message = fmt::format(_("\n{} of {}:\n"),
                            static_cast<int>(j) + 1, static_cast<int>(m.size()));
                        io->outputMessage(message);
                    }
                    display(m[j], m[j].name().empty() ? "ans" : m[j].name(), false, true);
                }
            }
        }
    }
    // General expression statement
    else {
        b = expression(t);
        if (printIt && state < NLS_STATE_QUIT) {
            display(b, "ans", false, true);
        }
    }

    if (isQuitOrForceQuitState() || state == NLS_STATE_ABORT) {
        callstack.popID();
        return;
    }
    // Update "ans" variable if required
    if (bUpdateAns) {
        context->insertVariable("ans", b);
    }
    callstack.popID();
}
//=============================================================================
// Trapping at the statement level is much better! - two
// problems... try/catch and multiline statements (i.e.,atell.m)
// The try-catch one is easy, I think...  When a try occurs,
// we capture the stack depth... if an exception occurs, we
// unwind the stack to this depth..
// The second one is trickier - suppose we have a conditional
// statement
// if (a == 3)
//    bfunc
// else
//    cfunc
// end
// this is represented in the parse tree as a single construct...
//
void
Evaluator::statement(AbstractSyntaxTreePtr t)
{
    try {
        callstack.pushID((size_t)t->getContext());
        if (t->opNum == (OP_QSTATEMENT)) {
            statementType(t->down, false);
        } else if (t->opNum == (OP_RSTATEMENT)) {
            statementType(t->down, true && bEchoMode);
        }
        callstack.popID();
    } catch (const Exception&) {
        callstack.popID();
        throw;
    }
}
//=============================================================================
// This function executes a block of code represented by an AbstractSyntaxTree.
void
Evaluator::block(AbstractSyntaxTreePtr t)
{
    if (t == nullptr) {
        return;
    }
    try {
        AbstractSyntaxTreePtr s = t->down;
        if (state < NLS_STATE_QUIT) {
            resetState();
        }
        while ((state < NLS_STATE_QUIT || state == NLS_STATE_CANCEL_QUIT) && s != nullptr) {
            if (NelsonConfiguration::getInstance()->getInterruptPending(ID)) {
                if (ID == 0) {
                    NelsonConfiguration::getInstance()->setInterruptPending(false, ID);
                    CallbackQueue::getInstance()->clear();
                    setState(NLS_STATE_ABORT);
                    Error(MSG_CTRL_C_DETECTED);
                } else {
                    Error(_W("Execution of the future was cancelled."),
                        L"parallel:fevalqueue:ExecutionCancelled");
                }
            }
            statement(s);
            if (state == NLS_STATE_BREAK || state == NLS_STATE_CONTINUE || state == NLS_STATE_RETURN
                || state == NLS_STATE_ABORT || isQuitOrForceQuitState()) {
                break;
            }
            s = s->right;
        }
    } catch (Exception& e) {
        if (!e.isEmpty()) {
            setLastErrorException(e);
            throw;
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
