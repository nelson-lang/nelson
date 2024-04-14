//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <boost/interprocess/managed_shared_memory.hpp>
#include <algorithm>
#include <cstdio>
#include <cerrno>
#include <iostream>
#include <cmath>
#include "StringHelpers.hpp"
#include "Evaluator.hpp"
#include "Exception.hpp"
#include "Keywords.hpp"
#include "ArrayOf.hpp"
#include "ParserInterface.hpp"
#include "LexerInterface.hpp"
#include "Interface.hpp"
#include "MacroFunctionDef.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
#include "FileParser.hpp"
#include "MainEvaluator.hpp"
#include "CommandQueue.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "CheckIfWhileCondition.hpp"
#include "Warning.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
#include "ClassToString.hpp"
#include "IsValidVariableName.hpp"
#include "NelsonReadyNamedMutex.hpp"
#include "TextToNumber.hpp"
#include "MException.hpp"
#include "FileSystemWrapper.hpp"
#include "PredefinedErrorMessages.hpp"
#include "Operators.hpp"
#include "Transpose.hpp"
#include "ComplexTranspose.hpp"
#include "UnaryMinus.hpp"
#include "UnaryPlus.hpp"
#include "Colon.hpp"
#include "DotPower.hpp"
#include "Or.hpp"
#include "OverloadName.hpp"
#include "BuiltInFunctionDefManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Stores the current array for which to apply the "end" expression to.
 */
class endData
{
public:
    ArrayOf endArray;
    int index = 0;
    size_t count = 0;
    endData(ArrayOf p, int ndx, size_t cnt) : endArray(p), index(ndx), count(cnt) { }
    ~endData() = default;
    ;
};
std::vector<endData> endStack;
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
bool
Evaluator::isQuitOrForceQuitState()
{
    return ((state == NLS_STATE_QUIT) || (state == NLS_STATE_FORCE_QUIT));
}
//=============================================================================
State
Evaluator::getState()
{
    return state;
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
ArrayOfVector
Evaluator::rowDefinition(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    if (t->opNum != OP_SEMICOLON) {
        Error(ERROR_AST_SYNTAX_ERROR);
    }
    ArrayOfVector retval = expressionList(t->down);
    callstack.popID();
    return retval;
}
//=============================================================================
//!
//@Module MATRIX Matrix Definitions
//@@Section VARIABLES
//@@Usage
// The matrix is the basic datatype of Nelson.  Matrices can be
// defined using the following syntax
//@[
//  A = [row_def1;row_def2;...,row_defN]
//@]
// where each row consists of one or more elements, seperated by
// commas
//@[
//  row_defi = element_i1,element_i2,...,element_iM
//@]
// Each element can either be a scalar value or another matrix,
// provided that the resulting matrix definition makes sense.
// In general this means that all of the elements belonging
// to a row have the same number of rows themselves, and that
// all of the row definitions have the same number of columns.
// Matrices are actually special cases of N-dimensional arrays
// where @|N<=2|.  Higher dimensional arrays cannot be constructed
// using the bracket notation described above.  The type of a
// matrix defined in this way (using the bracket notation) is
// determined by examining the types of the elements.  The resulting
// type is chosen so no information is lost on any of the elements
//(or equivalently, by choosing the highest order type from those
// present in the elements).
//@@Examples
// Here is an example of a matrix of @|int32| elements (note that
// untyped integer constants default to type @|int32|).
//@<
// A = [1,2;5,8]
//@>
// Now we define a new matrix by adding a column to the right of
//@|A|, and using float constants.
//@<
// B = [A,[3.2f;5.1f]]
//@>
// Next, we add extend @|B| by adding a row at the bottom.  Note
// how the use of an untyped floating point constant forces the
// result to be of type @|double|
//@<
// C = [B;5.2,1.0,0.0]
//@>
// If we instead add a row of @|complex| values (recall that @|i| is
// a @|complex| constant, not a @|dcomplex| constant)
//@<
// D = [B;2.0f+3.0f*i,i,0.0f]
//@>
// Likewise, but using @|dcomplex| constants
//@<
// E = [B;2.0+3.0*i,i,0.0]
//@>
// Finally, in Nelson, you can construct matrices with strings
// as contents, but you have to make sure that if the matrix has
// more than one row, that all the strings have the same length.
//@<
// F = ['hello';'there']
//@>
//!
ArrayOf
Evaluator::matrixDefinition(AbstractSyntaxTreePtr t)
{
    ArrayOfMatrix m;
    if (t->opNum != OP_BRACKETS) {
        Error(ERROR_AST_SYNTAX_ERROR);
    }
    AbstractSyntaxTreePtr s = t->down;
    callstack.pushID((size_t)s->getContext());
    while (s != nullptr) {
        m.emplace_back(rowDefinition(s));
        s = s->right;
    }
    ArrayOfVector v(m.size());
    for (auto& k : m) {
        v << horzcatOperator(k);
    }
    ArrayOf res = vertcatOperator(v);
    callstack.popID();
    return res;
}
//=============================================================================
//!
//@Module CELL Cell ArrayOf Definitions
//@@Section VARIABLES
//@@Usage
// The cell array is a fairly powerful array type that is available
// in Nelson.  Generally speaking, a cell array is a heterogenous
// array type, meaning that different elements in the array can
// contain variables of different type (including other cell arrays).
// For those of you familiar with @|C|, it is the equivalent to the
//@|void *| array.  The general syntax for their construction is
//@[
//   A = {row_def1;row_def2;...;row_defN}
//@]
// where each row consists of one or more elements, seperated by
// commas
//@[
//  row_defi = element_i1,element_i2,...,element_iM
//@]
// Each element can be any type of Nelson variable, including
// matrices, arrays, cell-arrays, structures, strings, etc.  The
// restriction on the definition is that each row must have the
// same number of elements in it.
//@@Examples
// Here is an example of a cell-array that contains a number,
// a string, and an array
//@<
// A = {14,'hello',[1:10]}
//@>
// Note that in the output, the number and string are explicitly
// printed, but the array is summarized.
// We can create a 2-dimensional cell-array by adding another
// row definition
//@<
// B = {pi,i;e,-1}
//@>
// Finally, we create a new cell array by placing @|A| and @|B|
// together
//@<
// C = {A,B}
//@>
//!
ArrayOf
Evaluator::cellDefinition(AbstractSyntaxTreePtr t)
{
    ArrayOfMatrix m;
    if (t->opNum != OP_BRACES) {
        Error(ERROR_AST_SYNTAX_ERROR);
    }
    AbstractSyntaxTreePtr s = t->down;
    callstack.pushID((size_t)s->getContext());
    while (s != nullptr) {
        m.emplace_back(rowDefinition(s));
        s = s->right;
    }
    ArrayOf retval(ArrayOf::cellConstructor(m));
    callstack.popID();
    return retval;
}
//=============================================================================
bool
Evaluator::needToOverloadOperator(const ArrayOf& a)
{
    return ((a.getDataClass() == NLS_STRUCT_ARRAY) || (a.getDataClass() == NLS_CELL_ARRAY)
        || (a.getDataClass() == NLS_STRING_ARRAY) || a.isSparse() || a.isHandle()
        || (a.getDataClass() == NLS_CLASS_ARRAY) || (a.getDataClass() == NLS_FUNCTION_HANDLE));
}
//=============================================================================
ArrayOf
Evaluator::EndReference(const ArrayOf& v, indexType index, size_t count)
{
    Dimensions dim(v.getDimensions());
    ArrayOf res;
    if (count == 1) {
        res = ArrayOf::doubleConstructor(static_cast<double>(dim.getElementCount()));
    } else {
        res = ArrayOf::doubleConstructor(static_cast<double>(dim.getDimensionLength(index)));
    }
    return res;
}
//=============================================================================
static bool
approximatelyEqual(double a, double b, double epsilon)
{
    return fabs(a - b) <= ((fabs(a) < fabs(b) ? fabs(b) : fabs(a)) * epsilon);
}
//=============================================================================
ArrayOf
Evaluator::expressionOperator(AbstractSyntaxTreePtr t)
{
    ArrayOf retval;
    uint64 ticProfiling = Profiler::getInstance()->tic();
    std::string operatorName;
    switch (t->opNum) {
    case OP_COLON:
        if (ticProfiling != 0U) {
            operatorName = COLON_OPERATOR_STR;
        }
        if ((t->down != nullptr) && (t->down->opNum == (OP_COLON))) {
            retval = colonOperator(t);
        } else {
            retval = colonUnitOperator(t);
        }
        break;
    case OP_EMPTY: {
        retval = ArrayOf::emptyConstructor();
    } break;
    case OP_EMPTY_CELL: {
        ArrayOf a(ArrayOf::emptyConstructor());
        a.promoteType(NLS_CELL_ARRAY);
        retval = a;
    } break;
    case OP_BRACKETS: {
        retval = matrixDefinition(t);
    } break;
    case OP_BRACES: {
        retval = cellDefinition(t);
    } break;
    case OP_PLUS: {
        if (ticProfiling != 0U) {
            operatorName = PLUS_OPERATOR_STR;
        }
        retval = plusOperator(t);
    } break;
    case OP_SUBTRACT: {
        if (ticProfiling != 0U) {
            operatorName = MINUS_OPERATOR_STR;
        }
        retval = minusOperator(t);
    } break;
    case OP_TIMES: {
        if (ticProfiling != 0U) {
            operatorName = MTIMES_OPERATOR_STR;
        }
        retval = mtimesOperator(t);
    } break;
    case OP_SOR: {
        if (ticProfiling != 0U) {
            operatorName = SHORTCUTOR_OPERATOR_STR;
        }
        retval = shortCutOrOperator(t);
    } break;
    case OP_OR: {
        if (ticProfiling != 0U) {
            operatorName = OR_OPERATOR_STR;
        }
        retval = orOperator(t);
    } break;
    case OP_SAND: {
        if (ticProfiling != 0U) {
            operatorName = SHORTCUTAND_OPERATOR_STR;
        }
        retval = shortCutAndOperator(t);
    } break;
    case OP_AND: {
        if (ticProfiling != 0U) {
            operatorName = AND_OPERATOR_STR;
        }
        retval = andOperator(t);
    } break;
    case OP_LT: {
        if (ticProfiling != 0U) {
            operatorName = LT_OPERATOR_STR;
        }
        retval = ltOperator(t);
    } break;
    case OP_LEQ: {
        if (ticProfiling != 0U) {
            operatorName = LE_OPERATOR_STR;
        }
        retval = leOperator(t);
    } break;
    case OP_GT: {
        if (ticProfiling != 0U) {
            operatorName = GT_OPERATOR_STR;
        }
        retval = gtOperator(t);
    } break;
    case OP_GEQ: {
        if (ticProfiling != 0U) {
            operatorName = GE_OPERATOR_STR;
        }
        retval = geOperator(t);
    } break;
    case OP_EQ: {
        if (ticProfiling != 0U) {
            operatorName = EQ_OPERATOR_STR;
        }
        retval = eqOperator(t);
    } break;
    case OP_NEQ: {
        if (ticProfiling != 0U) {
            operatorName = NE_OPERATOR_STR;
        }
        retval = neOperator(t);
    } break;
    case OP_DOT_TIMES: {
        if (ticProfiling != 0U) {
            operatorName = TIMES_OPERATOR_STR;
        }
        retval = timesOperator(t);
    } break;
    case OP_UPLUS: {
        if (ticProfiling != 0U) {
            operatorName = UPLUS_OPERATOR_STR;
        }
        retval = uplusOperator(t);
    } break;
    case OP_UMINUS: {
        if (ticProfiling != 0U) {
            operatorName = UMINUS_OPERATOR_STR;
        }
        retval = uminusOperator(t);
    } break;
    case OP_NOT: {
        if (ticProfiling != 0U) {
            operatorName = NOT_OPERATOR_STR;
        }
        retval = notOperator(t);
    } break;
    case OP_TRANSPOSE: {
        if (ticProfiling != 0U) {
            operatorName = CTRANSPOSE_OPERATOR_STR;
        }
        retval = complexTransposeOperator(t);
    } break;
    case OP_DOT_TRANSPOSE: {
        if (ticProfiling != 0U) {
            operatorName = TRANSPOSE_OPERATOR_STR;
        }
        retval = transposeOperator(t);
    } break;
    case OP_RHS: {
        // Test for simple variable lookup
        if (t->down->down == nullptr) {
            retval = rhsExpressionSimple(t->down);
        } else {
            ArrayOfVector m(rhsExpression(t->down));
            if (m.empty()) {
                retval = ArrayOf::emptyConstructor();
            } else {
                retval = m[0];
            }
        }
    } break;
    case OP_RDIV: {
        if (ticProfiling != 0U) {
            operatorName = MRDIVIDE_OPERATOR_STR;
        }
        retval = rightDivideOperator(t);
    } break;
    case OP_LDIV: {
        if (ticProfiling != 0U) {
            operatorName = MLDIVIDE_OPERATOR_STR;
        }
        retval = leftDivideOperator(t);
    } break;
    case OP_DOT_RDIV: {
        if (ticProfiling != 0U) {
            operatorName = RDIVIDE_OPERATOR_STR;
        }
        retval = dotRightDivideOperator(t);
    } break;
    case OP_DOT_LDIV: {
        if (ticProfiling != 0U) {
            operatorName = LDIVIDE_OPERATOR_STR;
        }
        retval = dotLeftDivideOperator(t);
    } break;
    case OP_MPOWER: {
        if (ticProfiling != 0U) {
            operatorName = MPOWER_OPERATOR_STR;
        }
        retval = mpowerOperator(t);
    } break;
    case OP_POWER: {
        if (ticProfiling != 0U) {
            operatorName = POWER_OPERATOR_STR;
        }
        retval = powerOperator(t);
    } break;
    case OP_FUNCTION_HANDLE_NAMED: {
        if (ticProfiling != 0U) {
            operatorName = FUNCTION_HANDLE_NAMED_STR;
        }
        retval = functionHandleNamedOperator(t);
    } break;
    case OP_FUNCTION_HANDLE_ANONYMOUS: {
        if (ticProfiling != 0U) {
            operatorName = FUNCTION_HANDLE_ANONYMOUS_STR;
        }
        retval = functionHandleAnonymousOperator(t);
    } break;
    default: {
        callstack.pushID((size_t)t->getContext());
        std::wstring msg;
        msg = ERROR_UNRECOGNIZED_EXPRESSION + L"\ncode: " + std::to_wstring(t->type);
        if (!t->text.empty()) {
            msg = msg + L"\ntext: " + utf8_to_wstring(t->text);
        }
        Error(msg);
        callstack.popID();
    } break;
    }
    if (ticProfiling != 0 && !operatorName.empty()) {
        internalProfileFunction stack
            = computeProfileStack(this, operatorName, utf8_to_wstring(callstack.getLastContext()));
        Profiler::getInstance()->toc(ticProfiling, stack);
    }
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::expressionReserved(AbstractSyntaxTreePtr t)
{
    ArrayOf retval;
    callstack.pushID((size_t)t->getContext());
    if (t->tokenNumber == NLS_KEYWORD_END) {
        if (endStack.empty()) {
            Error(ERROR_END_ILLEGAL);
        }
        endData enddatat(endStack.back());
        retval = EndReference(enddatat.endArray, (indexType)enddatat.index, enddatat.count);
    } else {
        Error(ERROR_UNRECOGNIZED_NODE);
    }
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::expression(AbstractSyntaxTreePtr t)
{
    ArrayOf retval;
    switch (t->type) {
    case const_double_node: {
        callstack.pushID((size_t)t->getContext());
        retval = ArrayOf::doubleConstructor(textToDouble(t->text));
        callstack.popID();
    } break;
    case const_float_node: {
        callstack.pushID((size_t)t->getContext());
        retval = ArrayOf::singleConstructor((textToSingle(t->text)));
        callstack.popID();
    } break;
    case const_uint8_node: {
        callstack.pushID((size_t)t->getContext());
        retval = ArrayOf::uint8Constructor(textToUint8(t->text));
        callstack.popID();
    } break;
    case const_int8_node: {
        callstack.pushID((size_t)t->getContext());
        retval = ArrayOf::int8Constructor(textToInt8(t->text));
        callstack.popID();
    } break;
    case const_uint16_node: {
        callstack.pushID((size_t)t->getContext());
        retval = ArrayOf::uint16Constructor(textToUint16(t->text));
        callstack.popID();
    } break;
    case const_int16_node: {
        callstack.pushID((size_t)t->getContext());
        retval = ArrayOf::int16Constructor(textToInt16(t->text));
        callstack.popID();
    } break;
    case const_uint32_node: {
        callstack.pushID((size_t)t->getContext());
        retval = ArrayOf::uint32Constructor(textToUint32(t->text));
        callstack.popID();
    } break;
    case const_int32_node: {
        callstack.pushID((size_t)t->getContext());
        retval = ArrayOf::int32Constructor(textToInt32(t->text));
        callstack.popID();
    } break;
    case const_int64_node: {
        callstack.pushID((size_t)t->getContext());
        retval = ArrayOf::int64Constructor(textToInt64(t->text));
        callstack.popID();
    } break;
    case const_uint64_node: {
        callstack.pushID((size_t)t->getContext());
        retval = ArrayOf::uint64Constructor(textToUint64(t->text));
        callstack.popID();
    } break;
    case const_int_node: {
        callstack.pushID((size_t)t->getContext());
        retval = ArrayOf::doubleConstructor(textToDouble(t->text));
        callstack.popID();
    } break;
    case const_character_array_node: {
        callstack.pushID((size_t)t->getContext());
        retval = ArrayOf::characterArrayConstructor(t->text);
        callstack.popID();
    } break;
    case const_string_node: {
        callstack.pushID((size_t)t->getContext());
        retval = ArrayOf::stringArrayConstructor(t->text);
        callstack.popID();
    } break;
    case const_dcomplex_node: {
        callstack.pushID((size_t)t->getContext());
        double val = textToDouble(t->text);
        if (approximatelyEqual(val, 0, std::numeric_limits<double>::epsilon())) {
            retval = ArrayOf::doubleConstructor(0.);
        } else {
            retval = ArrayOf::dcomplexConstructor(0, val);
        }
        callstack.popID();
    } break;
    case const_complex_node: {
        callstack.pushID((size_t)t->getContext());
        single val = textToSingle(t->text);
        if (approximatelyEqual(val, 0, std::numeric_limits<single>::epsilon())) {
            retval = ArrayOf::singleConstructor(0.);
        } else {
            retval = ArrayOf::complexConstructor(0, val);
        }
        callstack.popID();
    } break;
    case reserved_node: {
        return expressionReserved(t);
    }
    case non_terminal:
    case id_node:
    case null_node:
    default: {
        return expressionOperator(t);
    } break;
    }
    return retval;
}
//=============================================================================
/**
 * An expressionList allows for expansion of cell-arrays
 * and structure arrays.  Works by first screening rhs-expressions
 * through rhsExpression, which can return
 * a vector of variables.
 */
ArrayOfVector
Evaluator::expressionList(AbstractSyntaxTreePtr t)
{
    ArrayOfVector m;
    ArrayOfVector n;
    indexType tmp = 0;
    indexType endVal = 0;
    if (t == nullptr) {
        return m;
    }
    callstack.pushID(t->getContext());
    AbstractSyntaxTreePtr root = t;
    while (t != nullptr) {
        if (t->opNum == OP_KEYWORD) {
            t = t->right;
            continue;
        }
        if (t->type == non_terminal && t->opNum == (OP_RHS)) {
            try {
                n = rhsExpression(t->down);
            } catch (Exception& e) {
                if (!e.matches(ERROR_EMPTY_EXPRESSION)) {
                    throw;
                }
                n = ArrayOfVector();
            }
            m.reserve(n.size());
            for (const auto& i : n) {
                m.push_back(i);
            }
        } else if (t->type == non_terminal && t->opNum == (OP_ALL)) {
            Error(_W("Illegal use of the ':' operator"));
        } else {
            // Call the expression
            m.push_back(expression(t));
        }
        t = t->right;
    }
    callstack.popID();
    return m;
}
//=============================================================================
ArrayOfVector
Evaluator::expressionList(AbstractSyntaxTreePtr t, ArrayOf subRoot)
{
    ArrayOfVector m;
    ArrayOfVector n;
    AbstractSyntaxTreePtr root;
    indexType index = 0;
    indexType tmp = 0;
    indexType endVal = 0;
    if (t == nullptr) {
        return m;
    }
    callstack.pushID(t->getContext());
    size_t count = countSubExpressions(t);
    root = t;
    index = 0;
    while (t != nullptr) {
        if (t->opNum == OP_KEYWORD) {
            t = t->right;
            continue;
        }
        if (t->type == non_terminal && t->opNum == (OP_RHS)) {
            try {
                n = rhsExpression(t->down);
            } catch (Exception& e) {
                if (!e.matches(ERROR_EMPTY_EXPRESSION)) {
                    throw;
                }
                n = ArrayOfVector();
            }
            m.reserve(n.size());
            for (const auto& i : n) {
                m.push_back(i);
            }
        } else if ((t->type == non_terminal && t->opNum == OP_ALL)
            || (t->type == const_character_array_node && t->text == ":")) {
            Dimensions dim = subRoot.getDimensions();
            if (root->right == nullptr) {
                // Singleton reference, with ':' - return 1:length as column vector...
                tmp = dim.getElementCount();
                if (tmp == 0) {
                    m.push_back(ArrayOf::characterArrayConstructor(":"));
                } else {
                    m.push_back(ArrayOf::integerRangeConstructor(1, 1, tmp, true));
                }
            } else {
                tmp = dim.getDimensionLength(index);
                if (tmp == 0) {
                    m.push_back(ArrayOf::characterArrayConstructor(":"));
                } else {
                    m.push_back(ArrayOf::integerRangeConstructor(1, 1, tmp, false));
                }
            }
        } else {
            // Set up the value of the "end" token
            endStack.emplace_back(subRoot, (int)index, count);
            // Call the expression
            m.push_back(expression(t));
            endStack.pop_back();
        }
        index++;
        t = t->right;
    }
    callstack.popID();
    return m;
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
bool
Evaluator::AutoStop()
{
    return autostop;
}
//=============================================================================
void
Evaluator::AutoStop(bool a)
{
    autostop = a;
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
//!
//@Module CONTINUE Continue Execution In Loop
//@@Section FLOW
//@@Usage
// The @|continue| statement is used to change the order of
// execution within a loop.  The @|continue| statement can
// be used inside a @|for| loop or a @|while| loop.  The
// syntax for its use is
//@[
//   continue
//@]
// inside the body of the loop.  The @|continue| statement
// forces execution to start at the top of the loop with
// the next iteration.  The examples section shows how
// the @|continue| statement works.
//@@Example
// Here is a simple example of using a @|continue| statement.
// We want to sum the integers from @|1| to @|10|, but not
// the number @|5|.  We will use a @|for| loop and a continue
// statement.
//@{ continue_ex.m
// function accum = continue_ex
//  accum = 0;
//  for i=1:10
//    if (i==5)
//      continue;
//    end
//    accum = accum + 1; %skipped if i == 5!
//  end
//@}
// The function is exercised here:
//@<
// continue_ex
// sum([1:4,6:10])
//@>
//!

//!
//@Module BREAK Exit Execution In Loop
//@@Section FLOW
//@@Usage
// The @|break| statement is used to exit a loop prematurely.
// It can be used inside a @|for| loop or a @|while| loop.  The
// syntax for its use is
//@[
//   break
//@]
// inside the body of the loop.  The @|break| statement forces
// execution to exit the loop immediately.
//@@Example
// Here is a simple example of how @|break| exits the loop.
// We have a loop that sums integers from @|1| to @|10|, but
// that stops prematurely at @|5| using a @|break|.  We will
// use a @|while| loop.
//@{ break_ex.m
// function accum = break_ex
//  accum = 0;
//  i = 1;
//  while (i<=10)
//    accum = accum + i;
//    if (i == 5)
//      break;
//    end
//    i = i + 1;
//  end
//@}
// The function is exercised here:
//@<
// break_ex
// sum(1:5)
//@>
//!

//!
//@Module RETURN Return From Function
//@@Section FLOW
//@@Usage
// The @|return| statement is used to immediately return from
// a function, or to return from a @|pause| session.  The
// syntax for its use is
//@[
//  return
//@]
// Inside a function, a @|return| statement causes Nelson
// to exit the function immediately.  When a @|pause| session
// is active, the @|return| statement causes execution to
// resume where the @|pause| session started.
//@@Example
// In the first example, we define a function that uses a
//@|return| to exit the function if a certain test condition
// is satisfied.
//@{ return_func.m
// function ret = return_func(a,b)
//  ret = 'a is greater';
//  if (a > b)
//    return;
//  end
//  ret = 'b is greater';
//  printf('finishing up...\n');
//@}
// Next we exercise the function with a few simple test
// cases:
//@<
// return_func(1,3)
// return_func(5,2)
//@>
// In the second example, we take the function and rewrite
// it to use a @|keyboard| statement inside the @|if| statement.
//@{ return_func2.m
// function ret = return_func2(a,b)
//  if (a > b)
//     ret = 'a is greater';
//     keyboard;
//  else
//     ret = 'b is greater';
//  end
//  printf('finishing up...\n');
//@}
// Now, we call the function with a larger first argument, which
// triggers the @|keyboard| session.  After verifying a few
// values inside the @|keyboard| session, we issue a @|return|
// statement to resume execution.
//@<
// return_func2(2,4)
// return_func2(5,1)
// ret
// a
// b
// return
//@>
//!

//!
//@Module QUIT Quit Program
//@@Section Nelson
//@@Usage
// The @|quit| statement is used to immediately exit the Nelson
// application.  The syntax for its use is
//@[
//   quit
//@]
//!

//!
//@Module ABORT Return From All keyboard Sessions
//@@Section FLOW
//@@Usage
// The @|abort| statement is used to return to the base workspace
// from a nested @|keyboard| session.  It is equivalent to forcing
// execution to return to the main prompt, regardless of the level
// of nesting of @|keyboard| sessions, or which functions are
// running.  The syntax is simple
//@[
//   abort
//@]
// The @|abort| is a convenient way to stop debugging.  In the
// process of debugging a complex program or set of functions,
// you may find yourself 5 function calls down into the program
// only to discover the problem.  After fixing it, issueing
// a @|abort| effectively forces Nelson to exit your program
// and return to the interactive prompt.
//@@Example
// Here we demonstrate an extreme example of @|abort|.  We
// are debugging a recursive function @|self| to calculate the sum
// of the first N integers.  When the function is called,
// a @|keyboard| session is initiated after the function
// has called itself N times.  At this @|keyboard| prompt,
// we issue another call to @|self| and get another @|keyboard|
// prompt, this time with a depth of 2.  A @|abort| statement
// returns us to the top level without executing the remainder
// of either the first or second call to @|self|:
//@{ self.m
// function y = self(n)
//  if (n>1)
//    y = n + self(n-1);
//    printf('y is %d\n',y);
//  else
//    y = 1;
//    printf('y is initialized to one\n');
//    keyboard
//  end
//@}
//@<
// self(4)
// self(6)
// abort
//@>
//!

//!
//@Module keyboard Initiate Interactive Debug Session
//@@Section FLOW
//@@Usage
// The @|keyboard| statement is used to initiate an
// interactive session at a specific point in a function.
// The general syntax for the @|keyboard| statement is
//@[
//   keyboard
//@]
// A @|keyboard| statement can be issued in a @|script|,
// in a @|function|, or from within another @|pause| session.
// The result of a @|keyboard| statement is that execution
// of the program is halted, and you are given a prompt
// of the form:
//@[
// [scope,n] -->
//@]
// where @|scope| is the current scope of execution (either
// the name of the function we are executing, or @|base| otherwise).
// And @|n| is the depth of the @|keyboard| session. If, for example,
// we are in a @|keyboard| session, and we call a function that issues
// another @|pause| session, the depth of that second session
// will be one higher.  Put another way, @|n| is the number of @|return|
// statements you have to issue to get back to the base workspace.
// Incidentally, a @|return| is how you exit the @|pause| session
// and resume execution of the program from where it left off.  A
//@|abort| can be used to shortcut execution and return to the base
// workspace.
//
// The @|pause| statement is an excellent tool for debugging
// Nelson code, and along with @|eval| provide a unique set of
// capabilities not usually found in compiled environments.  Indeed,
// the @|pause| statement is equivalent to a debugger breakpoint in
// more traditional environments, but with significantly more inspection
// power.
//@@Example
// Here we demonstrate a two-level @|keyboard| situation.  We have
// a simple function that calls @|keyboard| internally:
//@{ key_one.m
// function c = key_one(a,b)
// c = a + b;
// keyboard
//@}
// Now, we execute the function from the base workspace, and
// at the @|keyboard| prompt, we call it again.  This action
// puts us at depth 2.  We can confirm that we are in the second
// invocation of the function by examining the arguments.  We
// then issue two @|return| statements to return to the base
// workspace.
//@<
// key_one(1,2)
// key_one(5,7)
// a
// b
// c
// return
// a
// b
// c
// return
//@>
//!

void
Evaluator::debugCLI()
{
    depth++;
    bpActive = true;
    evalCLI();
    bpActive = false;
    if (state == NLS_STATE_RETURN) {
        resetState();
    }
    depth--;
}
//=============================================================================
void
Evaluator::handleDebug(int fullcontext)
{
    if (debugActive) {
        int linenumber = fullcontext & 0xffff;
        if (inStepMode) {
            if ((stepTrap.cname == callstack.getLastContext()) && (stepTrap.tokid == linenumber)) {
                // Finished stepping...
                inStepMode = false;
                char buffer[2048];
                sprintf(buffer, _("Finished stepping to %s, line %d\n").c_str(),
                    stepTrap.cname.c_str(), linenumber);
                io->outputMessage(buffer);
                debugCLI();
            }
        } else {
            // check the breakpoint list
            bool found = false;
            size_t j = 0;
            while ((j < bpStack.size()) && !found) {
                // Is this a resolved breakpoint?
                if ((bpStack[j].tokid >> 16) != 0) {
                    if ((bpStack[j].cname == callstack.getLastContext())
                        && (bpStack[j].tokid == fullcontext)) {
                        found = true;
                    } else {
                        found = false;
                        j++;
                    }
                } else {
                    if ((bpStack[j].cname == callstack.getLastContext())
                        && (bpStack[j].tokid == linenumber)) {
                        found = true;
                        bpStack[j].tokid = fullcontext;
                    } else {
                        found = false;
                        j++;
                    }
                }
            }
            if (found) {
                stepTrap = bpStack[j];
                char buffer[2048];
                sprintf(buffer, _("Encountered breakpoint at %s, line %d\n").c_str(),
                    bpStack[j].cname.c_str(), linenumber);
                io->outputMessage(buffer);
                debugCLI();
            }
        }
    }
}
//=============================================================================
void
Evaluator::assignStatement(AbstractSyntaxTreePtr t, bool printIt)
{
    uint64 ticProfiling = Profiler::getInstance()->tic();
    ArrayOf b = expression(t->right);
    std::string variableName = t->text;
    if (t->down != nullptr) {
        b = assignExpression(t, b);
    }
    ArrayOf* var = context->lookupVariable(variableName);
    if (var == nullptr) {
        b.ensureSingleOwner();
        bool bInserted = context->insertVariable(variableName, b);
        if (!bInserted) {
            if (IsValidVariableName(variableName, true)) {
                Error(_W("Redefining permanent variable."));
            }
            Error(_W("Valid variable name expected."));
        }
    } else {
        if (context->isLockedVariable(variableName)) {
            Error(_W("Redefining permanent variable."));
        }
        var->setValue(b);
    }
    if (printIt) {
        display(b, variableName, false, false);
    }
    if (ticProfiling != 0) {
        internalProfileFunction stack = computeProfileStack(
            this, SUBSASGN_OPERATOR_STR, utf8_to_wstring(callstack.getLastContext()));
        Profiler::getInstance()->toc(ticProfiling, stack);
    }
}
//=============================================================================
void
Evaluator::statementType(AbstractSyntaxTreePtr t, bool printIt)
{
    ArrayOfVector m;
    FunctionDef* fdef = nullptr;
    if (!commandQueue.isEmpty()) {
        std::wstring cmd;
        commandQueue.get(cmd);
        evaluateString(cmd);
    }
    if (haveEventsLoop()) {
        ProcessEventsDynamicFunctionWithoutWait();
    }
    if (t == nullptr) {
        return;
    }
    callstack.pushID((size_t)t->getContext());
    // check the debug flag
    int fullcontext = t->getContext();
    handleDebug(fullcontext);
    if (t->isEmpty()) {
        /* Empty statement */
    } else if (t->opNum == (OP_ASSIGN)) {
        assignStatement(t->down, printIt);
    } else if (t->opNum == (OP_MULTICALL)) {
        multiFunctionCall(t->down, printIt);
    } else if (t->opNum == (OP_SCALL)) {
        ArrayOfVector m = specialFunctionCall(t->down, printIt);
        if (m.size() > 0) {
            context->insertVariable("ans", m[0]);
            display(m[0], "ans", false, true);
        }
    } else if (t->type == reserved_node) {
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
            if (context->inLoop()) {
                state = NLS_STATE_BREAK;
            }
            break;
        case NLS_KEYWORD_CONTINUE:
            if (context->inLoop()) {
                state = NLS_STATE_CONTINUE;
            }
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
            if (depth != 0) {
                depth = 0;
            }
            break;
        case NLS_KEYWORD_KEYBOARD:
            depth++;
            evalCLI();
            if (state < NLS_STATE_QUIT) {
                resetState();
            }
            depth--;
            break;
        case NLS_KEYWORD_ENDFUNCTION:
            /* a workaround to have a endfunction keyword */
            if (context->getCurrentScope()->getName() == "base") {
                Error(ERROR_ENDFUNCTION_WRONG_USE);
            }
            state = NLS_STATE_RETURN;
            break;
        default:
            Error(ERROR_UNRECOGNIZED_STATEMENT);
        }
    } else {
        // There is a special case to consider here - when a
        // function call is made as a statement, we do not require
        // that the function have an output.
        ArrayOf b;
        bool bUpdateAns = true;
        if (t->opNum == (OP_RHS) && !context->lookupVariable(t->down->text, b)
            && lookupFunction(t->down->text, fdef)) {
            m = functionExpression(fdef, t->down, 0, true);
            if (m.size() > 0) {
                b = m[0];
            } else {
                bUpdateAns = false;
            }
            if (printIt && (m.size() > 0) && (state < NLS_STATE_QUIT)) {
                display(b, "ans", false, true);
            }
        } else if (t->opNum == OP_RHS) {
            if (context->lookupVariable(t->down->text, b) && b.isFunctionHandle()) {
                m = rhsExpression(t->down, 0);
            } else {
                if (b.isCell()) {
                    try {
                        m = rhsExpression(t->down);
                    } catch (Exception& e) {
                        if (!e.matches(ERROR_EMPTY_EXPRESSION)) {
                            throw;
                        }
                    }
                } else {
                    m = rhsExpression(t->down);
                }
            }
            if (m.size() == 0) {
                b = ArrayOf::emptyConstructor();
            } else {
                b = m[0];
                if (printIt && (state < NLS_STATE_QUIT)) {
                    for (size_t j = 0; j < m.size(); j++) {
                        if (m.size() > 1) {
                            char buffer[1000];
                            sprintf(
                                buffer, _("\n%d of %d:\n").c_str(), (int)j + (int)1, (int)m.size());
                            io->outputMessage(buffer);
                        }
                        display(m[j], m[j].name().empty() ? "ans" : m[j].name(), false, true);
                    }
                }
            }
        } else {
            b = expression(t);
            if (printIt && (state < NLS_STATE_QUIT)) {
                display(b, "ans", false, true);
            }
        }
        if (isQuitOrForceQuitState() || state == NLS_STATE_ABORT) {
            callstack.popID();
            return;
        }
        if (bUpdateAns) {
            context->insertVariable("ans", b);
        }
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
Context*
Evaluator::getContext()
{
    return context;
}
//=============================================================================
ArrayOf
Evaluator::simpleSubindexExpression(ArrayOf& r, AbstractSyntaxTreePtr t)
{
    Dimensions rhsDimensions;
    ArrayOfVector m;
    rhsDimensions = r.getDimensions();
    if (t->opNum == (OP_PARENS)) {
        m = expressionList(t->down, r);
        if (m.size() == 0) {
            Error(ERROR_INDEX_EXPRESSION_EXPECTED);
        } else if (m.size() == 1) {
            if (r.isClassType()) {
                Error(ERROR_NEED_OVERLOAD);
            } else {
                try {
                    return (r.getVectorSubset(m[0]));
                } catch (const Exception&) {
                    return (ArrayOf::emptyConstructor());
                }
            }
        } else {
            try {
                return (r.getNDimSubset(m));
            } catch (const Exception&) {
                return (ArrayOf::emptyConstructor());
            }
        }
    }
    if (t->opNum == (OP_BRACES)) {
        m = expressionList(t->down, r);
        if (m.size() == 0) {
            Error(ERROR_INDEX_EXPRESSION_EXPECTED);
        } else if (m.size() == 1) {
            try {
                return (r.getVectorContents(m[0]));
            } catch (const Exception&) {
                return (ArrayOf::emptyConstructor());
            }
        } else {
            try {
                return (r.getNDimContents(m));
            } catch (const Exception&) {
                return (ArrayOf::emptyConstructor());
            }
        }
    }
    if (t->opNum == (OP_DOT)) {
        try {
            return (r.getField(t->down->text));
        } catch (const Exception&) {
            return (ArrayOf::emptyConstructor());
        }
    }
    if (t->opNum == (OP_DOTDYN)) {
        std::string field = "";
        try {
            ArrayOf fname(expression(t->down));
            field = fname.getContentAsCString();
        } catch (const Exception&) {
            Error(ERROR_DYNAMIC_FIELD_STRING_EXPECTED);
        }
        try {
            ArrayOf R = r.getField(field);
            return R;
        } catch (const Exception&) {
            return (ArrayOf::emptyConstructor());
        }
    }
    return (ArrayOf::emptyConstructor());
}
//=============================================================================
void
Evaluator::simpleAssign(ArrayOf& r, AbstractSyntaxTreePtr t, ArrayOf& value)
{
    ArrayOfVector vec;
    vec.push_back(value);
    simpleAssign(r, t, vec);
}
//=============================================================================
ArrayOfVector
Evaluator::simpleClassAssign(
    const std::string& subtype, const ArrayOf& r, const ArrayOfVector& m, ArrayOfVector& value)
{

    if (value.size() != 1) {
        Error(_W("Right hand values must satisfy left hand side expression."));
    }
    std::string currentClass;
    ClassName(r, currentClass);
    std::string functionNamesimpleAssignClass
        = getOverloadFunctionName(currentClass, SUBSASGN_OPERATOR_STR);
    Context* _context = this->getContext();
    FunctionDef* funcDef = nullptr;
    _context->lookupFunction(functionNamesimpleAssignClass, funcDef);
    if (!funcDef) {
        std::string functionNamesimpleAssignDefault
            = getOverloadFunctionName(NLS_CLASS_ARRAY_STR, SUBSASGN_OPERATOR_STR);
        _context->lookupFunction(functionNamesimpleAssignDefault, funcDef);
    }
    if (!funcDef) {
        Error(_("Function not found:") + " " + functionNamesimpleAssignClass);
    }
    if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION))) {
        Error(_W("Type function not valid."));
    }
    int nLhs = 0;
    ArrayOfVector argIn;
    argIn.push_back(r);

    stringVector fieldnames = { "type", "subs" };
    ArrayOfVector fieldvalues;
    fieldvalues.push_back(ArrayOf::characterArrayConstructor(subtype));
    if (subtype == ".") {
        fieldvalues.push_back(m[0]);
    } else {
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, m.size());
        ArrayOf cell = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, m.size()), elements);
        for (size_t k = 0; k < m.size(); k++) {
            elements[k] = m[k];
        }
        fieldvalues.push_back(cell);
    }
    ArrayOf substruct = ArrayOf::structScalarConstructor(fieldnames, fieldvalues);
    argIn.push_back(substruct);
    argIn.push_back(value[0]);
    CallStack backupCallStack = callstack;
    ArrayOfVector res = funcDef->evaluateFunction(this, argIn, nLhs);
    callstack = backupCallStack;
    return res;
}
//=============================================================================
void
Evaluator::simpleAssign(ArrayOf& r, AbstractSyntaxTreePtr t, ArrayOfVector& value)
{
    Dimensions rhsDimensions;
    ArrayOfVector m;
    callstack.pushID((size_t)t->getContext());
    if (!r.isEmpty()) {
        rhsDimensions = r.getDimensions();
    } else if (t->opNum != OP_BRACES) {
        rhsDimensions = value[0].getDimensions();
    } else {
        rhsDimensions.makeScalar();
    }
    if (t->opNum == (OP_PARENS)) {
        m = expressionList(t->down, r);
        if (r.isClassType()) {
            ArrayOfVector res = simpleClassAssign("()", r, m, value);
            r = res[0];
            callstack.popID();
            return;
        }
        if (m.size() == 0) {
            Error(ERROR_INDEX_EXPRESSION_EXPECTED);
        } else if (m.size() == 1) {
            r.setVectorSubset(m[0], value[0]);
            callstack.popID();
            return;
        } else {
            if (r.isClassType()) {
                ArrayOfVector res = simpleClassAssign("()", r, m, value);
                r = res[0];
            } else {
                r.setNDimSubset(m, value[0]);
            }
            callstack.popID();
            return;
        }
    }
    if (t->opNum == (OP_BRACES)) {
        m = expressionList(t->down, r);
        if (r.isClassType()) {
            ArrayOfVector res = simpleClassAssign("{}", r, m, value);
            r = res[0];
            callstack.popID();
            return;
        }
        if (m.size() == 0) {
            Error(ERROR_INDEX_EXPRESSION_EXPECTED);
        } else if (m.size() == 1) {
            r.setVectorContentsAsList(m[0], value);
            callstack.popID();
            return;
        } else {
            r.setNDimContentsAsList(m, value);
            callstack.popID();
            return;
        }
    }
    if (t->opNum == (OP_DOT)) {
        if (r.isClassType()) {
            std::string fieldname = t->down->text;
            ArrayOfVector m;
            m.push_back(ArrayOf::characterArrayConstructor(fieldname));
            ArrayOfVector res = simpleClassAssign(".", r, m, value);
            if (res.size() != 1) {
                Error(_("Invalid LHS."));
            }
            r = res[0];
            callstack.popID();
            return;
        }
        std::string fieldname = t->down->text;
        if (r.isHandle() || r.isGraphicsObject()) {
            setHandle(r, fieldname, value);
        } else if (r.isClassType()) {
            r.setFieldAsList(fieldname, value);
        } else if (r.isStruct() || r.isEmpty()) {
            r.setFieldAsList(fieldname, value);
        } else {
            Error(ERROR_ASSIGN_TO_NON_STRUCT);
        }
        callstack.popID();
        return;
    }
    if (t->opNum == (OP_DOTDYN)) {
        std::string field;
        try {
            ArrayOf fname(expression(t->down));
            field = fname.getContentAsCString();
        } catch (const Exception&) {
            Error(ERROR_DYNAMIC_FIELD_STRING_EXPECTED);
        }
        if (r.isClassType()) {
            ArrayOfVector m;
            m.push_back(ArrayOf::characterArrayConstructor(field));
            ArrayOfVector res = simpleClassAssign(".", r, m, value);
            if (res.size() != 1) {
                Error(_("Invalid LHS."));
            }
            r = res[0];
        } else if (r.isHandle()) {
            setHandle(r, field, value);
        } else {
            r.setFieldAsList(field, value);
        }
        callstack.popID();
        return;
    }
    callstack.popID();
}
//=============================================================================
indexType
Evaluator::countLeftHandSides(AbstractSyntaxTreePtr t)
{
    ArrayOf lhs;
    if (t == nullptr) {
        Error(_W("Syntax error."));
    }
    if (!context->lookupVariable(t->text, lhs)) { //-V1004
        lhs = ArrayOf::emptyConstructor();
    }
    AbstractSyntaxTreePtr s = t->down;
    if (s == nullptr) {
        return 1;
    }
    callstack.pushID((size_t)s->getContext());
    while (s->right != nullptr) {
        if (!lhs.isEmpty()) {
            lhs = simpleSubindexExpression(lhs, s);
        }
        s = s->right;
    }
    // We are down to the last subindexing expression...
    // We have to special case this one
    Dimensions rhsDimensions(lhs.getDimensions());
    ArrayOfVector m;
    if (s->opNum == (OP_PARENS)) {
        m = expressionList(s->down, lhs);
        if (m.size() == 0) {
            Error(ERROR_INDEX_EXPRESSION_EXPECTED);
        }
        if (m.size() == 1) {
            // m[0] should have only one element...
            m[0].toOrdinalType();
            if (m[0].getElementCount() > 1) {
                Error(ERROR_PARENTHETICAL_EXPRESSION);
            }
            callstack.popID();
            return (m[0].getElementCount());
        }
        size_t i = 0;
        indexType outputCount = 1;
        while (i < m.size()) {
            m[i].toOrdinalType();
            outputCount *= m[i].getElementCount();
            i++;
        }
        if (outputCount > 1) {
            Error(ERROR_PARENTHETICAL_EXPRESSION);
        }
        callstack.popID();
        return (outputCount);
    }
    if (s->opNum == (OP_BRACES)) {
        m = expressionList(s->down, lhs);
        if (m.size() == 0) {
            Error(ERROR_INDEX_EXPRESSION_EXPECTED);
        }
        if (m.size() == 1) {
            // m[0] should have only one element...
            m[0].toOrdinalType();
            callstack.popID();
            return (m[0].getElementCount());
        }
        size_t i = 0;
        indexType outputCount = 1;
        while (i < m.size()) {
            m[i].toOrdinalType();
            outputCount *= m[i].getElementCount();
            i++;
        }
        callstack.popID();
        return (outputCount);
    }
    if (s->opNum == (OP_DOT)) {
        callstack.popID();
        return lhs.getElementCount();
    }
    callstack.popID();
    return static_cast<indexType>(1);
}
//=============================================================================
ArrayOf
Evaluator::assignExpression(AbstractSyntaxTreePtr t, ArrayOf& val)
{
    ArrayOfVector vec;
    vec.push_back(val);
    return assignExpression(t, vec);
}
//=============================================================================
// If we got this far, we must have at least one subindex
ArrayOf
Evaluator::assignExpression(AbstractSyntaxTreePtr t, ArrayOfVector& value)
{
    callstack.pushID((size_t)t->getContext());
    if (t->down == nullptr) {
        ArrayOf retval(value[0]);
        value.pop_front();
        callstack.popID();
        return retval;
    }
    // Get the variable in question
    ArrayOf* var = context->lookupVariable(t->text);
    ArrayOf lhs;
    ArrayOf data;
    if (var == nullptr) {
        lhs = ArrayOf::emptyConstructor();
        data = lhs;
    } else {
        lhs.setValue(*var);
        data.setValue(*var);
    }
    // Set up a stack
    ArrayOfVector stack;
    AbstractSyntaxTreePtrVector ref;
    AbstractSyntaxTreePtr s = t->down;
    // Subindex
    while (s->right != nullptr) {
        if (!data.isEmpty()) {
            data = simpleSubindexExpression(data, s);
        }
        stack.push_back(data);
        ref.push_back(s);
        s = s->right;
    }
    // Do the assignment on the last temporary
    ArrayOf tmp(data);
    simpleAssign(tmp, s, value);
    ArrayOf rhs(tmp);
    if (stack.size() > 0) {
        stack.pop_back();
        // Now we have to "unwind" the stack
        while (stack.size() > 0) {
            // Grab the next temporary off the stack
            tmp = stack.back();
            // Make the assignment
            simpleAssign(tmp, ref.back(), rhs);
            // Assign this temporary to be the RHS of the next temporary
            rhs = tmp;
            // Pop the two stacks
            stack.pop_back();
            ref.pop_back();
        }
        // Complete the final assignment:
        // Last assignment is to lhs
        simpleAssign(lhs, ref.back(), tmp);
    } else {
        lhs = tmp;
    }
    callstack.popID();
    return lhs;
}
//=============================================================================
ArrayOfVector
Evaluator::specialFunctionCall(AbstractSyntaxTreePtr t, bool printIt)
{
    ArrayOfVector m;
    stringVector args;
    args.push_back(t->text);
    AbstractSyntaxTreePtr s = t->right;
    while (s != nullptr) {
        args.push_back(s->text);
        s = s->right;
    }
    if (args.empty()) {
        return m;
    }
    ArrayOfVector n;
    n.reserve(args.size());
    for (size_t i = 1; i < args.size(); i++) {
        n.push_back(ArrayOf::characterArrayConstructor(args[i].c_str()));
    }
    FunctionDefPtr val;
    callstack.pushID((size_t)t->getContext());
    if (!lookupFunction(args[0], val)) {
        Error(utf8_to_wstring(_("unable to resolve ") + args[0] + _(" to a function call")));
    }
    bool CLIFlagsave = InCLI;
    InCLI = false;
    try {
        m = val->evaluateFunction(this, n, 0);
    } catch (const Exception&) {
        InCLI = CLIFlagsave;
        callstack.popID();
        throw;
    }
    InCLI = CLIFlagsave;
    callstack.popID();
    return m;
}
//=============================================================================
void
Evaluator::addBreakpoint(StackEntry& bp)
{
    bpStack.push_back(bp);
    adjustBreakpoints();
    debugActive = true;
}
//=============================================================================
void
Evaluator::multiFunctionCall(AbstractSyntaxTreePtr t, bool printIt)
{
    ArrayOfVector m;
    AbstractSyntaxTreePtr s;
    AbstractSyntaxTreePtr fAST;
    AbstractSyntaxTreePtr saveLHS;
    AbstractSyntaxTreePtr cAST;
    ArrayOf c;
    // int lhsSize;
    FunctionDef* fptr;
    cAST = t;
    fAST = t->right;
    bool bDeal = false;
    callstack.pushID(fAST->getContext());
    ArrayOf r;
    if (!lookupFunction(fAST->text, fptr)) {
        bool isVar = context->lookupVariable(fAST->text, r);
        if (isVar) {
            if (r.isFunctionHandle()) {
                std::string extractionFunctionName
                    = getOverloadFunctionName(NLS_FUNCTION_HANDLE_STR, SUBSREF_OPERATOR_STR);
                bool isFun = lookupFunction(extractionFunctionName, fptr);
                if (!isFun) {
                    Error(utf8_to_wstring(_("Undefined function") + " " + extractionFunctionName));
                }
            } else if (r.isClassType()) {
                std::string className = r.getClassType();
                std::string extractionFunctionName
                    = getOverloadFunctionName(className, SUBSREF_OPERATOR_STR);
                bool isFun = lookupFunction(extractionFunctionName, fptr);
                if (!isFun) {
                    Error(utf8_to_wstring(_("Undefined function") + " " + extractionFunctionName));
                }
            } else if (r.isCell()) {
                // C = {rand(3),nan(3),zeros(3),inf(3)}
                // [a, b, c, d] = C{ : }
                if (t->opNum != OP_BRACKETS) {
                    Error(ERROR_ILLEGAL_LEFT_MULTIFUNCTION_EXPRESSION);
                }
                bDeal = true;
            } else {
                Error(utf8_to_wstring(_("Undefined function") + " " + fAST->text));
            }
        } else {
            Error(utf8_to_wstring(_("Undefined function") + " " + fAST->text));
        }
    }
    if (t->opNum != OP_BRACKETS) {
        Error(ERROR_ILLEGAL_LEFT_MULTIFUNCTION_EXPRESSION);
    }
    s = t->down;
    if (s->opNum != OP_SEMICOLON) {
        Error(ERROR_ILLEGAL_LEFT_MULTIFUNCTION_EXPRESSION);
    }
    if (s->right != nullptr) {
        Error(ERROR_MULTIPLE_ROWS_NOT_ALLOWED);
    }
    // We have to make multiple passes through the LHS part of the AST.
    // The first pass is to count how many function outputs are actually
    // being requested.
    // Calculate how many lhs objects there are
    // lhsSize = s->peerCount();
    s = s->down;
    saveLHS = s;
    // Get the lhs objects into rset
    indexType lhsCount = 0;
    AbstractSyntaxTreePtr mptr = s;
    while (mptr != nullptr) {
        indexType dmp = countLeftHandSides(mptr->down);
        lhsCount += dmp;
        mptr = mptr->right;
    }
    if (bDeal) {
        // C = {rand(3),nan(3),zeros(3),inf(3)}
        // [a, b, c, d] = C{ : }
        Dimensions rhsDimensions;
        rhsDimensions = r.getDimensions();
        m = expressionList(fAST->down->down, r);
        if (m.size() == 0) {
            Error(ERROR_INDEX_EXPRESSION_EXPECTED);
        } else if (m.size() == 1) {
            ArrayOfVector m2 = r.getVectorContentsAsList(m[0]);
            if ((indexType)m2.size() < lhsCount) {
                Error(_W("Insufficient number of outputs."));
            } else {
                m = m2;
            }
        } else {
            Error(_W("Case not managed."));
        }
    } else {
        CallStack backupCallStack = callstack;
        m = functionExpression(fptr, fAST, (int)lhsCount, false);
        callstack = backupCallStack;
    }
    s = saveLHS; //-V1048
    while ((s != nullptr) && (m.size() > 0)) {
        ArrayOf c(assignExpression(s->down, m));
        if (!context->insertVariable(s->down->text, c)) {
            if (IsValidVariableName(s->down->text, true)) {
                Error(_W("Redefining permanent variable."));
            }
            Error(_W("Valid variable name expected."));
        }
        if (printIt) {
            display(c, s->down->text, false, true);
        }
        s = s->right;
    }
    if (s != nullptr) {
        std::wstring message = _W("Function") + L" : " + utf8_to_wstring(fAST->text) + L"\n"
            + WARNING_OUTPUTS_NOT_ASSIGNED;
        Warning(message);
    }
    callstack.popID();
}
//=============================================================================
int
getArgumentIndex(const stringVector& list, const std::string& t)
{
    bool foundArg = false;
    std::string q;
    size_t i = 0;
    while (i < list.size() && !foundArg) {
        q = list[i];
        if (q[0] == '&') {
            q.erase(0, 1);
        }
        foundArg = (q == t);
        if (!foundArg) {
            i++;
        }
    }
    if (foundArg) {
        return static_cast<int>(i);
    }
    return -1;
}
//=============================================================================
//!
//@Module FUNCTION Function Declarations
//@@Section FUNCTIONS
//@@Usage
// There are several forms for function declarations in Nelson.
// The most general syntax for a function declaration is the
// following:
//@[
//  function [out_1,...,out_M,varargout] = fname(in_1,...,in_N,varargin)
//@]
// where @|out_i| are the output parameters, @|in_i| are the input
// parameters, and @|varargout| and @|varargin| are special keywords
// used for functions that have variable inputs or outputs.  For
// functions with a fixed number of input or output parameters, the
// syntax is somewhat simpler:
//@[
//  function [out_1,...,out_M] = fname(in_1,...,in_N)
//@]
// Note that functions that have no return arguments can omit
// the return argument list (of @|out_i|) and the equals sign:
//@[
//  function fname(in_1,...,in_N)
//@]
// Likewise, a function with no arguments can eliminate the list
// of parameters in the declaration:
//@[
//  function [out_1,...,out_M] = fname
//@]
// Functions that return only a single value can omit the brackets
//@[
//  function out_1 = fname(in_1,...,in_N)
//@]
//
// In the body of the function @|in_i| are initialized with the
// values passed when the function is called.  Also, the function
// must assign values for @|out_i| to pass values to the caller.
// Note that by default, Nelson passes arguments by value, meaning
// that if we modify the contents of @|in_i| inside the function,
// it has no effect on any variables used by the caller.  Arguments
// can be passed by reference by prepending an ampersand @|&|
// before the name of the input, e.g.
//@[
//  function [out1,...,out_M] = fname(in_1,&in_2,in_3,...,in_N)
//@]
// in which case @|in_2| is passed by reference and not by value.
// Also, Nelson works like @|C| in that the caller does not have
// to supply the full list of arguments.  Also, when @|keywords|
//(see help @|keywords|) are used, an arbitrary subset of the
// parameters may be unspecified. To assist in deciphering
// the exact parameters that were passed,
// Nelson also defines two variables inside the function context:
//@|nargin| and @|nargout|, which provide the number of input
// and output parameters of the caller, respectively. See help for
//@|nargin| and @|nargout| for more details.  In some
// circumstances, it is necessary to have functions that
// take a variable number of arguments, or that return a variable
// number of results.  In these cases, the last argument to the
// parameter list is the special argument @|varargin|.  Inside
// the function, @|varargin| is a cell-array that contains
// all arguments passed to the function that have not already
// been accounted for.  Similarly, the function can create a
// cell array named @|varargout| for variable length output lists.
// See help @|varargin| and @|varargout| for more details.
//
// The function name @|fname| can be any legal Nelson identifier.
// Functions are stored in files with the @|.m| extension.  Note
// that the name of the file (and not the function name @|fname|
// used in the declaration) is how the function appears in Nelson.
// So, for example, if the file is named @|foo.m|, but the declaration
// uses @|bar| for the name of the function, in Nelson, it will
// still appear as function @|foo|.  Note that this is only true
// for the first function that appears in a @|.m| file.  Additional
// functions that appear after the first function are known as
//@|helper functions| or @|local| functions.  These are functions that
// can only be called by other functions in the same @|.m| file.  Furthermore
// the names of these helper functions are determined by their declaration
// and not by the name of the @|.m| file.  An example of using
// helper functions is included in the examples.
//
// Another important feature of functions, as opposed to, say @|scripts|,
// is that they have their own @|scope|.  That means that variables
// defined or modified inside a function do not affect the scope of the
// caller.  That means that a function can freely define and use variables
// without unintentionally using a variable name reserved elsewhere.  The
// flip side of this fact is that functions are harder to debug than
// scripts without using the @|keyboard| function, because the intermediate
// calculations used in the function are not available once the function
// exits.
//@@Examples
// Here is an example of a trivial function that adds its
// first argument to twice its second argument:
//@{ addtest.m
// function c = addtest(a,b)
//  c = a + 2*b;
//@}
//@<
// addtest(1,3)
// addtest(3,0)
//@>
// Suppose, however, we want to replace the value of the first
// argument by the computed sum.  A first attempt at doing so
// has no effect:
//@{ addtest2.m
// function addtest2(a,b)
//  a = a + 2*b;
//@}
//@<
// arg1 = 1
// arg2 = 3
// addtest2(arg1,arg2)
// arg1
// arg2
//@>
// The values of @|arg1| and @|arg2| are unchanged, because they are
// passed by value, so that any changes to @|a| and @|b| inside
// the function do not affect @|arg1| and @|arg2|.  We can change
// that by passing the first argument by reference:
//@{ addtest3.m
// function addtest3(&a,b)
//  a = a + 2*b
//@}
// Note that it is now illegal to pass a literal value for @|a| when
// calling @|addtest3|:
//@<
// addtest(3,4)
// addtest(arg1,arg2)
// arg1
// arg2
//@>
// The first example fails because we cannot pass a literal like the
// number @|3| by reference.  However, the second call succeeds, and
// note that @|arg1| has now changed.  Note: please be careful when
// passing by reference - this feature is not available in MATLAB
// and you must be clear that you are using it.
//
// As variable argument and return functions are covered elsewhere,
// as are keywords, we include one final example that demonstrates
// the use of helper functions, or local functions, where
// multiple function declarations occur in the same file.
//@{ euclidlength.m
// function y = foo(x,y)
//  square_me(x);
//  square_me(y);
//  y = sqrt(x+y);
//
// function square_me(&t)
//  t = t^2;
//@}
//@<
// euclidlength(3,4)
// euclidlength(2,0)
//@>
//!

//!
//@Module KEYWORDS Function Keywords
//@@Section FUNCTIONS
//@@Usage
// A feature of IDL that Nelson has adopted is a modified
// form of @|keywords|.  The purpose of @|keywords| is to
// allow you to call a function with the arguments to the
// function specified in an arbitrary order.  To specify
// the syntax of @|keywords|, suppose there is a function
// with prototype
//@[
//  function [out_1,...,out_M] = foo(in_1,...,in_N)
//@]
// Then the general syntax for calling function @|foo| using keywords
// is
//@[
//  foo(val_1, val_2, /in_k=3)
//@]
// which is exactly equivalent to
//@[
//  foo(val_1, val_2, [], [], ..., [], 3),
//@]
// where the 3 is passed as the k-th argument, or alternately,
//@[
//  foo(val_1, val_2, /in_k)
//@]
// which is exactly equivalent to
//@[
//  foo(val_1, val_2, [], [], ..., [], logical(1)),
//@]
// Note that you can even pass reference arguments using keywords.
//@@Example
// The most common use of keywords is in controlling options for
// functions.  For example, the following function takes a number
// of binary options that control its behavior.  For example,
// consider the following function with two arguments and two
// options.  The function has been written to properly use and
// handle keywords.  The result is much cleaner than the MATLAB
// approach involving testing all possible values of @|nargin|,
// and forcing explicit empty brackets for don't care parameters.
//@{ keyfunc.m
// function c = keyfunc(a,b,operation,printit)
//  if (~exist('a') | ~exist('b'))
//    error('keyfunc requires at least the first two 2 arguments');
//  end;
//  if (~exist('operation'))
//    % user did not define the operation, default to '+'
//    operation = '+';
//  end
//  if (~exist('printit'))
//    % user did not specify the printit flag, default is false
//    printit = 0;
//  end
//  % simple operation...
//  eval(['c = a ' operation ' b;']);
//  if (printit)
//    printf('%f %s %f = %f',a,operation,b,c);
//  end
//@}
// Now some examples of how this function can be called using
//@|keywords|.
//@<
// keyfunc(1,3)                % specify a and b, defaults for the others
// keyfunc(1,3,/printit)       % specify printit is true
// keyfunc(/operation='-',2,3) % assigns a=2, b=3
// keyfunc(4,/operation='*',/printit) % error as b is unspecified
//@>
//!

//!
//@Module VARARGIN Variable Input Arguments
//@@Section FUNCTIONS
//@@Usage
// Nelson functions can take a variable number of input arguments
// by setting the last argument in the argument list to @|varargin|.
// This special keyword indicates that all arguments to the
// function (beyond the last non-@|varargin| keyword) are assigned
// to a cell array named @|varargin| available to the function.
// Variable argument functions are usually used when writing
// driver functions, i.e., functions that need to pass arguments
// to another function.  The general syntax for a function that
// takes a variable number of arguments is
//@[
//  function [out_1,...,out_M] = fname(in_1,..,in_M,varargin)
//@]
// Inside the function body, @|varargin| collects the arguments
// to @|fname| that are not assigned to the @|in_k|.
//@@Example
// Here is a simple wrapper to @|feval| that demonstrates the
// use of variable arguments functions.
//@{ wrapcall.m
// function wrapcall(fname,varargin)
//  feval(fname,varargin{:});
//@}
// Now we show a call of the @|wrapcall| function with a number
// of arguments
//@<
// wrapcall('printf','%f...%f\n',pi,e)
//@>
// A more serious driver routine could, for example, optimize
// a one dimensional function that takes a number of auxilliary
// parameters that are passed through @|varargin|.
//!

//!
//@Module VARARGOUT Variable Output Arguments
//@@Section FUNCTIONS
//@@Usage
// Nelson functions can return a variable number of output arguments
// by setting the last argument in the argument list to @|varargout|.
// This special keyword indicates that the number of return values
// is variable.  The general syntax for a function that returns
// a variable number of outputs is
//@[
//  function [out_1,...,out_M,varargout] = fname(in_1,...,in_M)
//@]
// The function is responsible for ensuring that @|varargout| is
// a cell array that contains the values to assign to the outputs
// beyond @|out_M|.  Generally, variable output functions use
//@|nargout| to figure out how many outputs have been requested.
//@@Example
// This is a function that returns a varying number of values
// depending on the value of the argument.
//@{ varoutfunc.m
// function [varargout] = varoutfunc
//  switch(nargout)
//    case 1
//      varargout = {'one of one'};
//    case 2
//      varargout = {'one of two','two of two'};
//    case 3
//      varargout = {'one of three','two of three','three of three'};
//  end
//@}
// Here are some examples of exercising @|varoutfunc|:
//@<
//[c1] = varoutfunc
//[c1,c2] = varoutfunc
//[c1,c2,c3] = varoutfunc
//@>
//!

//!
//@Module SCRIPT Script Files
//@@Section FUNCTIONS
//@@Usage
// A script is a sequence of Nelson commands contained in a
//@|.m| file.  When the script is called (via the name of the
// file), the effect is the same as if the commands inside the
// script file were issued one at a time from the keyboard.
// Unlike @|function| files (which have the same extension,
// but have a @|function| declaration), script files share
// the same environment as their callers.  Hence, assignments,
// etc, made inside a script are visible to the caller (which
// is not the case for functions.
//@@Example
// Here is an example of a script that makes some simple
// assignments and @|printf| statements.
//@{ tscript.m
// a = 13;
// printf('a is %d\n',a);
// b = a + 32
//@}
// If we execute the script and then look at the defined variables
//@<
// tscript
// who
//@>
// we see that @|a| and @|b| are defined appropriately.
//!

//!
//@Module NARGIN Number of Input Arguments
//@@Section FUNCTIONS
//@@Usage
// The special variable @|nargin| is defined inside of all
// functions.  It indicates how many arguments were passed
// to the function when it was called.  Nelson allows for
// fewer arguments to be passed to a function than were declared,
// and @|nargin|, along with @|isset| can be used to determine
// exactly what subset of the arguments were defined.
// There is no syntax for the use of @|nargin| - it is
// automatically defined inside the function body.
//@@Example
// Here is a function that is declared to take five
// arguments, and that simply prints the value of @|nargin|
// each time it is called.
//@{ nargintest.m
// function nargintest(a1,a2,a3,a4,a5)
//  printf('nargin = %d\n',nargin);
//@}
//@<
// nargintest(3);
// nargintest(3,'h');
// nargintest(3,'h',1.34);
// nargintest(3,'h',1.34,pi,e);
//@>
//!

//!
//@Module NARGOUT Number of Output Arguments
//@@Section FUNCTIONS
//@@Usage
// The special variable @|nargout| is defined inside of all
// functions.  It indicates how many return values were requested from
// the function when it was called.  Nelson allows for
// fewer return values to be requested from a function than were declared,
// and @|nargout| can be used to determine exactly what subset of
// the functions outputs are required.  There is no syntax for
// the use of @|nargout| - it is automatically defined inside
// the function body.
//@@Example
// Here is a function that is declared to return five
// values, and that simply prints the value of @|nargout|
// each time it is called.
//@{ nargouttest.m
// function [a1,a2,a3,a4,a5] = nargouttest
//  printf('nargout = %d\n',nargout);
//  a1 = 1; a2 = 2; a3 = 3; a4 = 4; a5 = 5;
//@}
//@<
// a1 = nargouttest
//[a1,a2] = nargouttest
//[a1,a2,a3] = nargouttest
//[a1,a2,a3,a4,a5] = nargouttest
//@>
//!

//!
//@Module SPECIAL Special Calling Syntax
//@@Section FUNCTIONS
//@@Usage
// To reduce the effort to call certain functions, Nelson supports
// a special calling syntax for functions that take string arguments.
// In particular, the three following syntaxes are equivalent, with
// one caveat:
//@[
//   functionname('arg1','arg2',...,'argn')
//@]
// or the parenthesis and commas can be removed
//@[
//   functionname 'arg1' 'arg2' ... 'argn'
//@]
// The quotes are also optional (providing, of course, that the
// argument strings have no spaces in them)
//@[
//   functionname arg1 arg2 ... argn
//@]
// This special syntax enables you to type @|hold on| instead of
// the more cumbersome @|hold('on')|.  The caveat is that Nelson
// currently only recognizes the special calling syntax as the
// first statement on a line of input.  Thus, the following construction
//@[
//  for i=1:10; plot(vec(i)); hold on; end
//@]
// would not work.  This limitation may be removed in a future
// version.
//@@Example
// Here is a function that takes two string arguments and
// returns the concatenation of them.
//@{ strcattest.m
// function strcattest(str1,str2)
//  str3 = [str1,str2];
//  printf('str1 = %s, str2 = %s, str3 = %s\n',str1,str2,str3);
//@}
// We call @|strcattest| using all three syntaxes.
//@<
// strcattest('hi','ho')
// strcattest 'hi' 'ho'
// strcattest hi ho
//@>
//!
ArrayOfVector
Evaluator::functionExpression(
    FunctionDef* funcDef, AbstractSyntaxTreePtr t, int narg_out, bool outputOptional)
{
    ArrayOfVector m;
    ArrayOfVector n;
    AbstractSyntaxTreePtr s = nullptr;
    AbstractSyntaxTreePtr q = nullptr;
    AbstractSyntaxTreePtr p = nullptr;
    stringVector keywords;
    ArrayOfVector keyvals;
    AbstractSyntaxTreePtrVector keyexpr;
    int* keywordNdx = nullptr;
    int* argTypeMap = nullptr;
    callstack.pushID(t->getContext());
    bool CLIFlagsave = InCLI;
    try {
        {
            // Look for arguments
            if (t->down != nullptr) {
                s = t->down;
                if (s->opNum == (OP_PARENS)) {
                    s = s->down;
                    // Search for the keyword uses -
                    // To handle keywords, we make one pass through the arguments,
                    // recording a list of keywords used and using ::expression to
                    // evaluate their values.
                    q = s;
                    while (q != nullptr) {
                        if (q->opNum == OP_KEYWORD) {
                            keywords.push_back(q->down->text);
                            if (q->down->right != nullptr) {
                                keyvals.push_back(expression(q->down->right));
                            } else {
                                keyvals.push_back(ArrayOf::logicalConstructor(true));
                            }
                            keyexpr.push_back(q->down->right);
                        }
                        q = q->right;
                    }
                    // If any keywords were found, make another pass through the
                    // arguments and remove them.
                    m = expressionList(s);
                    // Check for keywords
                    if (keywords.size() > 0) {
                        // If keywords were used, we have to permute the
                        // entries of the arrayvector to the correct order.
                        stringVector arguments;
                        // Get the arguments from the MacroFunction pointer.
                        arguments = funcDef->arguments;
                        try {
                            keywordNdx = new int[keywords.size()];
                        } catch (std::bad_alloc&) {
                            Error(ERROR_MEMORY_ALLOCATION);
                        }
                        int maxndx;
                        maxndx = 0;
                        // Map each keyword to an argument number
                        for (size_t i = 0; i < keywords.size(); ++i) {
                            int ndx;
                            ndx = getArgumentIndex(arguments, keywords[i]);
                            if (ndx == -1) {
                                if (keywordNdx) {
                                    delete[] keywordNdx;
                                    keywordNdx = nullptr;
                                }
                                Error(utf8_to_wstring(_("out-of-order argument /") + keywords[i]
                                    + _(" is not defined in the called function!")));
                            }
                            keywordNdx[i] = ndx; //-V522
                            if (ndx > maxndx) {
                                maxndx = ndx;
                            }
                        }
                        // Next, we have to determine how many "holes" there are
                        // in the argument list - we get the maximum list
                        size_t holes = maxndx + 1 - keywords.size();
                        // At this point, holes is the number of missing arguments
                        // If holes > m.size(), then the total number of arguments
                        // is just maxndx+1.  Otherwise, its
                        // maxndx+1+(m.size() - holes)
                        size_t totalCount;
                        if (holes > m.size()) {
                            totalCount = ((size_t)maxndx + (size_t)1);
                        } else {
                            totalCount = (size_t)(maxndx + 1 + (m.size() - holes));
                        }
                        // Next, we allocate a vector to hold the values
                        ArrayOfVector toFill(totalCount);
                        bool* filled = new bool[totalCount];
                        argTypeMap = new int[totalCount];
                        for (size_t i = 0; i < totalCount; i++) {
                            filled[i] = false;
                            argTypeMap[i] = -1;
                        }
                        // Finally...
                        // Copy the keyword values in
                        for (size_t i = 0; i < keywords.size(); i++) {
                            toFill[keywordNdx[i]] = keyvals[i];
                            filled[keywordNdx[i]] = true;
                            argTypeMap[keywordNdx[i]] = (int)i;
                        }
                        // Fill out the rest of the values from m
                        size_t n = 0;
                        int p = 0;
                        while (n < m.size()) {
                            if (!filled[p]) {
                                toFill[p] = m[n];
                                filled[p] = true;
                                argTypeMap[p] = -2;
                                n++;
                            }
                            p++;
                        }
                        // Finally, fill in empty matrices for the
                        // remaining arguments
                        for (size_t i = 0; i < totalCount; i++) {
                            if (!filled[i]) {
                                toFill[i] = ArrayOf::emptyConstructor();
                            }
                        }
                        // Clean up
                        delete[] filled;
                        // delete[] keywordNdx;
                        // Reassign
                        m = toFill;
                    }
                } else {
                    ArrayOf r;
                    bool isVar = context->lookupVariable(t->text, r);
                    if (isVar) {
                        if (r.isClassType()) {
                            s = t->down; //-V1048
                            if (s->opNum == (OP_DOT) || s->opNum == (OP_DOTDYN)) {
                                s = s->down;
                                return scalarArrayOfToArrayOfVector(r.getField(s->text));
                            }
                        }
                    }
                    // exception for "py" name, need to be more generic
                    if (t->text == "py") {
                        FunctionDefPtr fdef;
                        if (BuiltInFunctionDefManager::getInstance()->find(t->text, fdef)) {
                            ArrayOfVector inputs;
                            return fdef->evaluateFunction(this, inputs, 1);
                        }
                    }
                    Error(ERROR_ILLEGAL_EXPRESSION_IN_FUNCTION);
                }
            } else {
                m = ArrayOfVector();
            }
            CLIFlagsave = InCLI;
            InCLI = false;
            ArrayOf r;
            bool isVar = context->lookupVariable(t->text, r);
            bool haveDown = (t->down != nullptr);
            if (isVar) {
                // if it is a class C
                if (r.isClassType() || r.isFunctionHandle()) {
                    // C(X1, ..., XN)
                    // we call : C_extract(C, X1, ..., XN)
                    if (m.size() > 0) {
                        m.push_front(r);
                    } else {
                        // C() or C
                        if (t->down != nullptr) {
                            s = t->down;
                            // C()
                            if (s->opNum == (OP_PARENS)) {
                                m.push_front(r);
                            }
                            // C others ? C{}, C. ?
                            // we do currently nothing
                        } else {
                            if (keywordNdx != nullptr) {
                                delete[] keywordNdx;
                                keywordNdx = nullptr;
                            }
                            if (argTypeMap != nullptr) {
                                delete[] argTypeMap;
                                argTypeMap = nullptr;
                            }
                            return scalarArrayOfToArrayOfVector(r);
                        }
                    }
                }
                n = funcDef->evaluateFunction(this, m, narg_out);
            } else {
                if (funcDef == nullptr) {
                    if (m.size() > 0 && m[0].isHandle() && m[0].isScalar()
                        && m[0].isHandleMethod(t->text)) {
                        HandleGenericObject* obj = m[0].getContentAsHandleScalar();
                        if (obj) {
                            n = obj->invokeMethod(m, narg_out, t->text);
                        }
                    } else {
                        Error(
                            utf8_to_wstring(_("Undefined variable or function:") + " " + t->text));
                    }
                } else {
                    n = funcDef->evaluateFunction(this, m, narg_out);
                }
            }
            InCLI = CLIFlagsave;
            if (state == NLS_STATE_ABORT) {
                if (keywordNdx != nullptr) {
                    delete[] keywordNdx;
                    keywordNdx = nullptr;
                }
                if (argTypeMap != nullptr) {
                    delete[] argTypeMap;
                    argTypeMap = nullptr;
                }
                return n;
            }
            // Check for any pass by reference
            if (!haveDown && (funcDef->arguments.size() > 0)) {
                // Get the argument list
                stringVector arguments;
                arguments = funcDef->arguments;
                // M functions can modify their arguments
                size_t maxsearch = m.size();
                if (maxsearch > arguments.size()) {
                    maxsearch = arguments.size();
                }
                if (maxsearch != 0) {
                    q = s;
                }
                for (size_t i = 0; i < maxsearch; i++) {
                    // Was this argument passed out of order?
                    if (argTypeMap != nullptr) {
                        if ((keywords.size() > 0) && (argTypeMap[i] == -1)) {
                            continue;
                        }
                        if ((keywords.size() > 0) && (argTypeMap[i] >= 0)) {
                            p = keyexpr[argTypeMap[i]];
                        } else {
                            p = q;
                            if (q != nullptr) {
                                q = q->right;
                            }
                        }
                    }
                    std::string args(arguments[i]);
                    if (args[0] == '&') {
                        args.erase(0, 1);
                        // This argument was passed by reference
                        if (p == nullptr) {
                            if (argTypeMap != nullptr) {
                                delete[] argTypeMap;
                                argTypeMap = nullptr;
                            }
                            Error(ERROR_MUST_HAVE_LVALUE);
                            return {};
                        }
                        if (!(p->type == non_terminal && p->opNum == OP_RHS)) {
                            if (argTypeMap != nullptr) {
                                delete[] argTypeMap;
                                argTypeMap = nullptr;
                            }
                            Error(ERROR_MUST_HAVE_LVALUE);
                            return {};
                        }
                        std::string variableName = p->down->text;
                        ArrayOf c;
                        if (p->down->down == nullptr && p->down->type == id_node) {
                            c = m[i];
                        } else {
                            c = assignExpression(p->down, m[i]);
                        }
                        ArrayOf* ptrVar = context->lookupVariable(variableName);
                        if (ptrVar != nullptr) {
                            ptrVar->setValue(c);
                        } else {
                            c.name(variableName);
                            if (!context->insertVariable(variableName, c)) {
                                if (argTypeMap != nullptr) {
                                    delete[] argTypeMap;
                                    argTypeMap = nullptr;
                                }
                                if (IsValidVariableName(variableName, true)) {
                                    Error(_W("Redefining permanent variable."));
                                }
                                Error(_W("Valid variable name expected."));
                                return {};
                            }
                        }
                    }
                }
            }
        }
        // Some routines (e.g., min and max) will return more outputs
        // than were actually requested... so here we have to trim
        // any elements received that we didn't ask for.
        //	  narg_out = (int)n.size();
        /*
        while ((int)n.size() > narg_out)	n.pop_back();
        popID();
        return n;
        */
    } catch (const Exception&) {
        InCLI = CLIFlagsave;
        throw;
    }
    callstack.popID();
    if (keywordNdx != nullptr) {
        delete[] keywordNdx;
        keywordNdx = nullptr;
    }
    if (argTypeMap != nullptr) {
        delete[] argTypeMap;
        argTypeMap = nullptr;
    }
    return n;
}
//=============================================================================
int
COST(int a, int b)
{
    return (((a) >= (b)) ? ((a) - (b)) : 10000);
}
//=============================================================================
int
GetClosestLineNumber(AbstractSyntaxTreePtr t, int lineno)
{
    if (t == nullptr) {
        return 10000;
    }
    int linedwn = GetClosestLineNumber(t->down, lineno);
    int linerght = GetClosestLineNumber(t->right, lineno);
    int retval = (t->getContext() & 0xffff);
    ;
    int costthis = COST(retval, lineno);
    return (std::min(linedwn, std::min(linerght, costthis)));
}
//=============================================================================
void
Evaluator::listBreakpoints()
{
    for (size_t i = 0; i < bpStack.size(); i++) {
        char buffer[2048];
        sprintf(buffer, _("%d   %s line %d\n").c_str(), i + 1, bpStack[i].cname.c_str(),
            bpStack[i].tokid & 0xffff);
        io->outputMessage(buffer);
    }
}
//=============================================================================
void
Evaluator::deleteBreakpoint(int number)
{
    if ((number >= 1) && (number <= (int)bpStack.size())) {
        bpStack.erase(bpStack.begin() + number - 1);
    } else {
        char buffer[2048];
        sprintf(buffer, _("Unable to delete breakpoint %d").c_str(), number);
        Error(utf8_to_wstring(buffer));
    }
}
//=============================================================================
bool
Evaluator::adjustBreakpoint(StackEntry& bp, bool dbstep)
{
    bool isFun;
    FunctionDefPtr val;
    std::string cname = bp.detail;
    isFun = context->lookupFunction(cname, val);
    if (!isFun) {
        return false;
    }
    if (val->type() == NLS_MACRO_FUNCTION) {
        MacroFunctionDef* mptr;
        mptr = (MacroFunctionDef*)val;
        int clinenum = 10000;
        while (mptr != nullptr) {
            AbstractSyntaxTreePtr code = mptr->code;
            int nxt = GetClosestLineNumber(code, bp.tokid & 0xffff);
            clinenum = std::min(clinenum, nxt);
            mptr = mptr->nextFunction;
        }
        if (clinenum == 10000) {
            char buffer[2048];
            if (dbstep) {
                sprintf(buffer, "%s",
                    _("Unable to step the specified number of lines, execution will continue\n")
                        .c_str());
                inStepMode = false;
            } else {
                sprintf(buffer,
                    _("Failed to set breakpoint in %s at line %d - breakpoint is disabled\n")
                        .c_str(),
                    cname.c_str(), bp.tokid & 0xffff);
            }
            Warning(std::string(buffer));
            return false;
        }
        if (clinenum != 0) {
            bp.tokid = (bp.tokid & 0xffff) + clinenum;
        }
    } else {
        return false;
    }
    return true;
}
//=============================================================================
void
Evaluator::adjustBreakpoints()
{
    std::vector<StackEntry>::iterator i = bpStack.begin();
    while (i != bpStack.end()) {
        if (!adjustBreakpoint(*i, false)) {
            std::vector<StackEntry>::iterator b = i;
            bpStack.erase(b);
            ++i;
        } else {
            ++i;
        }
    }
    if (inStepMode) {
        adjustBreakpoint(stepTrap, true);
    }
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
void
Evaluator::setInterface(Interface* _io)
{
    io = _io;
}
//======================================================================
Interface*
Evaluator::getInterface()
{
    return io;
}
//=============================================================================
bool
Evaluator::lookupFunction(const std::string& funcName, FunctionDefPtr& val)
{
    return context->lookupFunction(funcName, val);
}
//=============================================================================
ArrayOf
Evaluator::rhsExpressionSimple(AbstractSyntaxTreePtr t)
{
    ArrayOf r;
    ArrayOfVector m;
    bool isVar = false;
    bool isFun = false;
    FunctionDef* funcDef = nullptr;
    callstack.pushID((size_t)t->getContext());
    // Try to satisfy the rhs expression with what functions we have already
    // loaded.
    isVar = context->lookupVariable(t->text, r);
    if (isVar && (t->down == nullptr)) {
        callstack.popID();
        return r;
    }
    if (!isVar) {
        isFun = lookupFunction(t->text, funcDef);
    }
    if (!isVar && isFun && funcDef != nullptr) {
        m = functionExpression(funcDef, t, 1, false);
        if (m.empty()) {
            callstack.popID();
            return ArrayOf::emptyConstructor();
        }
        callstack.popID();
        return m[0];
    }
    if (!isVar) {
        Error(utf8_to_wstring(_("Undefined variable:") + " " + t->text));
    }
    if (!isFun) {
        Error(utf8_to_wstring(_("Undefined function:") + " " + t->text));
    }
    callstack.popID();
    return r;
}
//=============================================================================
//!
//@Module INDEXING Indexing Expressions
//@@Section VARIABLES
//@@Usage
// There are three classes of indexing expressions available
// in Nelson: @|()|, @|{}|, and @|.|  Each is explained below
// in some detail, and with its own example section.
//@@ArrayOf Indexing
// We start with array indexing @|()|,
// which is the most general indexing expression, and can be
// used on any array.  There are two general forms for the
// indexing expression - the N-dimensional form, for which
// the general syntax is
//@[
//  variable(index_1,index_2,...,index_n)
//@]
// and the vector form, for which the general syntax is
//@[
//  variable(index)
//@]
// Here each index expression is either a scalar, a range
// of integer values, or the special token @|:|, which is
// shorthand for @|1:end|.  The keyword @|end|, when included
// in an indexing expression, is assigned the length of the
// array in that dimension.  The concept is easier to demonstrate
// than explain.  Consider the following examples:
//@<
// A = zeros(4)
// B = float(randn(2))
// A(2:3,2:3) = B
//@>
// Here the array indexing was used on the left hand side only.
// It can also be used for right hand side indexing, as in
//@<
// C = A(2:3,1:end)
//@>
// Note that we used the @|end| keyword to avoid having to know
// that @|A| has 4 columns.  Of course, we could also use the
//@|:| token instead:
//@<
// C = A(2:3,:)
//@>
// An extremely useful example of @|:| with array indexing is for
// slicing.  Suppose we have a 3-D array, that is @|2 x 2 x 3|,
// and we want to set the middle slice:
//@<
// D = zeros(2,2,3)
// D(:,:,2) = int32(10*rand(2,2))
//@>
// In another level of nuance, the assignment expression will
// automatically fill in the indexed rectangle on the left using
// data from the right hand side, as long as the lengths match.
// So we can take a vector and roll it into a matrix using this
// approach:
//@<
// A = zeros(4)
// v = [1;2;3;4]
// A(2:3,2:3) = v
//@>
//
// The N-dimensional form of the variable index is limited
// to accessing only (hyper-) rectangular regions of the
// array.  You cannot, for example, use it to access only
// the diagonal elements of the array.  To do that, you use
// the second form of the array access (or a loop).  The
// vector form treats an arbitrary N-dimensional array as though
// it were a column vector.  You can then access arbitrary
// subsets of the arrays elements (for example, through a @|find|
// expression) efficiently.  Note that in vector form, the @|end|
// keyword takes the meaning of the total length of the array
//(defined as the product of its dimensions), as opposed to the
// size along the first dimension.
//@@Cell Indexing
// The second form of indexing operates, to a large extent, in
// the same manner as the array indexing, but it is by no means
// interchangable.  As the name implies, @|cell|-indexing applies
// only to @|cell| arrays.  For those familiar with @|C|, cell-
// indexing is equivalent to pointer derefencing in @|C|.  First,
// the syntax:
//@[
//  variable{index_1,index_2,...,index_n}
//@]
// and the vector form, for which the general syntax is
//@[
//  variable{index}
//@]
// The rules and interpretation for N-dimensional and vector indexing
// are identical to @|()|, so we will describe only the differences.
// In simple terms, applying @|()| to a cell-array returns another
// cell array that is a subset of the original array.  On the other
// hand, applying @|{}| to a cell-array returns the contents of that
// cell array.  A simple example makes the difference quite clear:
//@<
// A = {1, 'hello', [1:4]}
// A(1:2)
// A{1:2}
//@>
// You may be surprised by the response to the last line.  The output
// is multiple assignments to @|ans|!.  The output of a cell-array
// dereference can be used anywhere a list of expressions is required.
// This includes arguments and returns for function calls, matrix
// construction, etc.  Here is an example of using cell-arrays to pass
// parameters to a function:
//@<
// A = {[1,3,0],[5,2,7]}
// max(A{1:end})
//@>
// And here, cell-arrays are used to capture the return.
//@<
//[K{1:2}] = max(randn(1,4))
//@>
// Here, cell-arrays are used in the matrix construction process:
//@<
// C = [A{1};A{2}]
//@>
// Note that this form of indexing is used to implement variable
// length arguments to function.  See @|varargin| and @|varargout|
// for more details.
//@@Structure Indexing
// The third form of indexing is structure indexing.  It can only
// be applied to structure arrays, and has the general syntax
//@[
//  variable.fieldname
//@]
// where @|fieldname| is one of the fields on the structure.  Note that
// in Nelson, fields are allocated dynamically, so if you reference
// a field that does not exist in an assignment, it is created automatically
// for you.  If variable is an array, then the result of the @|.|
// reference is an expression list, exactly like the @|{}| operator.  Hence,
// we can use structure indexing in a simple fashion:
//@<
// clear A
// A.color = 'blue'
// B = A.color
//@>
// Or in more complicated ways using expression lists for function arguments
//@<
// clear A
// A(1).maxargs = [1,6,7,3]
// A(2).maxargs = [5,2,9,0]
// max(A.maxargs)
//@>
// or to store function outputs
//@<
// clear A
// A(1).maxreturn = [];
// A(2).maxreturn = [];
//[A.maxreturn] = max(randn(1,4))
//@>
// Nelson now also supports the so called dynamic-field indexing
// expressions.  In this mode, the fieldname is supplied through
// an expression instead of being explicitly provided.  For example,
// suppose we have a set of structure indexed by color,
//@<
// x.red = 430;
// x.green = 240;
// x.blue = 53;
// x.yello = 105
//@>
// Then we can index into the structure @|x| using a dynamic field
// reference:
//@<
// y = 'green'
// a = x.(y)
//@>
// Note that the indexing expression has to resolve to a string for
// dynamic field indexing to work.
//@@Complex Indexing
// The indexing expressions described above can be freely combined
// to affect complicated indexing expressions.  Here is an example
// that exercises all three indexing expressions in one assignment.
//@<
// Z{3}.foo(2) = pi
//@>
// From this statement, Nelson infers that Z is a cell-array of
// length 3, that the third element is a structure array (with one
// element), and that this structure array contains a field named
//'foo' with two double elements, the second of which is assigned
// a value of pi.
//!
ArrayOfVector
Evaluator::rhsExpression(AbstractSyntaxTreePtr t, int nLhs)
{
    ArrayOf r;
    ArrayOfVector m;
    ArrayOfVector rv;
    Dimensions rhsDimensions;
    FunctionDef* funcDef = nullptr;
    callstack.pushID((size_t)t->getContext());
    // Try to satisfy the rhs expression with what functions we have already
    // loaded.
    if (context->lookupVariable(t->text, r)) {
        if (t->down == nullptr) {
            callstack.popID();
            return ArrayOfVector(r);
        }
        AbstractSyntaxTreePtr tt = t->down;
        if (tt->opNum == OP_PARENS && tt->down == nullptr && tt->right != nullptr) {
            tt = tt->right;
            if (tt->opNum == OP_DOT) {
                ArrayOfVector rv(r.getField(tt->down->text));
                callstack.popID();
                return rv;
            }
        }
    } else {
        if (lookupFunction(t->text, funcDef)) {
            if (funcDef->outputArgCount() == 0) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS, utf8_to_wstring(funcDef->getName()));
            }
            m = functionExpression(funcDef, t, 1, false);
            callstack.popID();
            return m;
        }
        /* try for method of handle */
        if (t->down && t->down->opNum == OP_PARENS) {
            funcDef = nullptr;
            m = functionExpression(funcDef, t, 1, false);
            callstack.popID();
            return m;
        }
        Error(utf8_to_wstring(_("Undefined variable or function:") + " " + t->text));
    }

    t = t->down;
    while (t != nullptr) {
        rhsDimensions = r.getDimensions();
        if (!rv.empty()) {
            Error(_W("Cannot reindex an expression that returns multiple values."));
        }
        if (t->opNum == (OP_PARENS)) {
            bool isFinished = false;
            rhsExpressionParens(rhsDimensions, rv, t, r, nLhs, isFinished);
            if (isFinished) {
                return rv;
            }
        }
        if (t->opNum == (OP_BRACES)) {
            rhsExpressionBraces(rhsDimensions, rv, t, r, nLhs);
        }
        if (t->opNum == (OP_DOT)) {
            rhsExpressionDot(rv, t, r, nLhs);
        }
        if (t->opNum == (OP_DOTDYN)) {
            rhsExpressionDynDot(rv, t, r, nLhs);
        }
        t = t->right;
    }
    if (rv.empty()) {
        if (nLhs != 0) {
            rv.push_back(r);
        }
    }
    callstack.popID();
    return rv;
}
//=============================================================================
void
Evaluator::rhsExpressionParens(Dimensions& rhsDimensions, ArrayOfVector& rv,
    AbstractSyntaxTreePtr& t, ArrayOf& r, int nLhs, bool& isFinished)
{
    isFinished = false;
    ArrayOfVector m = expressionList(t->down, r);
    if (rhsDimensions.getLength() > (indexType)m.size()) {
        Dimensions nDim;
        if (m.size() >= 2) {
            size_t d = 0;
            for (d = 0; d < m.size() - 1; d++) {
                nDim[d] = rhsDimensions[d];
            }
            indexType L = 1;
            for (indexType k = d; k < rhsDimensions.getLength(); k++) {
                L = L * rhsDimensions[k];
            }
            nDim[d] = L;
            r.reshape(nDim);
            rhsDimensions = r.getDimensions();
            m = expressionList(t->down, r);
        }
    }
    if (r.isFunctionHandle()) {
        if (t->right != nullptr && t->right->opNum != OP_PARENS) {
            Error(_("Invalid indexing."));
        }
        std::string extractionFunctionName
            = getOverloadFunctionName(NLS_FUNCTION_HANDLE_STR, SUBSREF_OPERATOR_STR);
        FunctionDef* funcDef = nullptr;
        if (lookupFunction(extractionFunctionName, funcDef)) {
            CallStack backupCallStack = this->callstack;
            ArrayOfVector paramsIn(m);
            paramsIn.push_front(r);
            rv = funcDef->evaluateFunction(this, paramsIn, nLhs);
            callstack = backupCallStack;
            callstack.popID();
            isFinished = true;
            return;
        }
    }
    if (m.size() == 0) {
        if (t->right == nullptr) {
            callstack.popID();
            rv = ArrayOfVector(r);
            isFinished = true;
            return;
        }
        Error(_W("index expected."));

    } else if (m.size() == 1) {
        if (r.isClassType()) {
            stringVector substype;
            ArrayOfVector subsindices;
            substype.push_back("()");
            ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, m.size() + 1);
            ArrayOf cell = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, m.size()), elements);
            for (size_t k = 0; k < m.size(); k++) {
                elements[k] = m[k];
            }
            subsindices.push_back(cell);

            bool haveFunction;
            ArrayOfVector rr = extractClass(r, substype, subsindices, haveFunction);
            if (haveFunction) {
                r = rr[0];
            } else {
                r = r.getVectorSubset(m[0]);
            }
        } else {
            r = r.getVectorSubset(m[0]);
        }
    } else {
        if (r.isClassType()) {
            stringVector substype;
            ArrayOfVector subsindices;
            substype.push_back("()");
            ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, m.size() + 1);
            ArrayOf cell = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, m.size()), elements);
            for (size_t k = 0; k < m.size(); k++) {
                elements[k] = m[k];
            }
            subsindices.push_back(cell);

            bool haveFunction;
            ArrayOfVector rr = extractClass(r, substype, subsindices, haveFunction);
            if (haveFunction) {
                r = rr[0];
            } else {
                r = r.getNDimSubset(m);
            }
        } else {
            r = r.getNDimSubset(m);
        }
    }
}
//=============================================================================
void
Evaluator::rhsExpressionBraces(
    Dimensions& rhsDimensions, ArrayOfVector& rv, AbstractSyntaxTreePtr& t, ArrayOf& r, int nLhs)
{
    ArrayOfVector m = expressionList(t->down, r);
    if (rhsDimensions.getLength() > (indexType)m.size()) {
        Dimensions nDim;
        if (m.size() >= 2) {
            size_t d = 0;
            for (d = 0; d < m.size() - 1; d++) {
                nDim[d] = rhsDimensions[d];
            }
            indexType L = 1;
            for (indexType k = d; k < rhsDimensions.getLength(); k++) {
                L = L * rhsDimensions[k];
            }
            nDim[d] = L;
            r.reshape(nDim);
            rhsDimensions = r.getDimensions();
            m = expressionList(t->down, r);
        }
    }
    if (m.size() == 0) {
        Error(ERROR_INDEX_EXPRESSION_EXPECTED);
    } else if (m.size() == 1) {
        if (r.isClassType()) {
            stringVector substype;
            ArrayOfVector subsindices;
            substype.push_back("{}");
            ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, m.size() + 1);
            ArrayOf cell = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, m.size()), elements);
            for (size_t k = 0; k < m.size(); k++) {
                elements[k] = m[k];
            }
            subsindices.push_back(cell);

            bool haveFunction;
            ArrayOfVector rr = extractClass(r, substype, subsindices, haveFunction);
            if (haveFunction) {
                rv = rr[0];
            } else {
                rv = r.getVectorContentsAsList(m[0]);
            }
        } else {
            rv = r.getVectorContentsAsList(m[0]);
        }
    } else {
        if (r.isClassType()) {
            stringVector substype;
            ArrayOfVector subsindices;
            substype.push_back("{}");
            ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, m.size() + 1);
            ArrayOf cell = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, m.size()), elements);
            for (size_t k = 0; k < m.size(); k++) {
                elements[k] = m[k];
            }
            subsindices.push_back(cell);

            bool haveFunction;
            ArrayOfVector rr = extractClass(r, substype, subsindices, haveFunction);
            if (haveFunction) {
                rv = rr[0];
            } else {
                rv = r.getNDimContentsAsList(m);
            }
        } else {
            rv = r.getNDimContentsAsList(m);
        }
    }

    if (rv.size() == 1) {
        r = rv[0];
        rv = ArrayOfVector();
    } else if (rv.size() == 0) {
        r = ArrayOf::emptyConstructor();

        if (nLhs == 1) {
            Error(ERROR_EMPTY_EXPRESSION);
        }
    }
}
//=============================================================================
void
Evaluator::rhsExpressionDot(ArrayOfVector& rv, AbstractSyntaxTreePtr& t, ArrayOf& r, int nLhs)
{
    std::string fieldname = t->down->text;

    if (r.isClassType()) {
        stringVector substype;
        ArrayOfVector subsindices;

        substype.push_back(".");
        subsindices.push_back(ArrayOf::characterArrayConstructor(fieldname));
        if (t->right != nullptr) {
            if (t->right->opNum == OP_DOT) {
                substype.push_back(".");
            } else if (t->right->opNum == OP_BRACES) {
                substype.push_back("{}");
            } else if (t->right->opNum == OP_PARENS) {
                substype.push_back("()");
            }
            ArrayOfVector childrendIndices = expressionList(t->right->down, r);
            ArrayOf* elements
                = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, childrendIndices.size());
            ArrayOf cell
                = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, childrendIndices.size()), elements);
            for (size_t k = 0; k < childrendIndices.size(); k++) {
                elements[k] = childrendIndices[k];
            }
            subsindices.push_back(cell);
            t = t->right;
        }
        bool haveFunction;
        rv = extractClass(r, substype, subsindices, haveFunction);
        if (!haveFunction) {
            rv = r.getFieldAsList(fieldname);
        }
    } else if (r.isHandle() || r.isGraphicsObject()) {
        ArrayOfVector params;
        logical isValidMethod = false;
        try {
            isValidMethod = isObjectMethod(r, fieldname);
        } catch (const Exception&) {
            if (r.isHandle()) {
                Error(_("Please define: ")
                    + getOverloadFunctionName(r.getHandleCategory(), "ismethod"));
            }
            isValidMethod = false;
        }
        if (isValidMethod != 0U) {
            if (t->right != nullptr) {
                params = expressionList(t->right->down, r);
                t = t->right;
            }
        }
        if (isValidMethod) {
            rv = invokeMethod(r, fieldname, params);
        } else {
            rv = getHandle(r, fieldname, params);
        }

    } else {
        rv = r.getFieldAsList(fieldname);
    }
    if (rv.size() == 1) {
        r = rv[0];
        rv = ArrayOfVector();
    } else if (rv.size() == 0) {
        r = ArrayOf::emptyConstructor();
        rv = ArrayOfVector();
    }
}
//=============================================================================
void
Evaluator::rhsExpressionDynDot(ArrayOfVector& rv, AbstractSyntaxTreePtr& t, ArrayOf& r, int nLhs)
{
    std::string field;
    try {
        ArrayOf fname(expression(t->down));
        field = fname.getContentAsCString();
    } catch (const Exception&) {
        Error(ERROR_DYNAMIC_FIELD_STRING_EXPECTED);
    }
    if (r.isClassType()) {
        ArrayOfVector v;

        stringVector substype;
        ArrayOfVector subsindices;
        substype.push_back(".");
        subsindices.push_back(ArrayOf::characterArrayConstructor(field));

        bool haveFunction;
        rv = extractClass(r, substype, subsindices, haveFunction);
        if (!haveFunction) {
            rv = r.getFieldAsList(field);
        }
    } else if (r.isHandle()) {
        ArrayOfVector v;
        rv = getHandle(r, field, v);
    } else {
        rv = r.getFieldAsList(field);
    }
    if (rv.size() <= 1) {
        r = rv[0];
        rv = ArrayOfVector();
    }
}
//=============================================================================
size_t
Evaluator::getID()
{
    return ID;
}
//=============================================================================
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
Evaluator::~Evaluator()
{
    clearStacks();
    commandLineArguments.clear();
    resetLastErrorException();
    resetLastWarningException();
}
//=============================================================================
void
Evaluator::dbstep(int linecount)
{
    if (bpActive) {
        stepTrap.tokid = (stepTrap.tokid & 0xffff) + linecount;
        inStepMode = true;
        adjustBreakpoints();
        state = NLS_STATE_RETURN;
    }
}
//=============================================================================
bool
Evaluator::evaluateString(const std::wstring& line, bool propogateException)
{
    return evaluateString(wstring_to_utf8(line), propogateException);
}
//=============================================================================
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
        parserState = parseString(command);
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
            throw; //-V667
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
bool
Evaluator::setLastErrorException(const Exception& e)
{
    Exception* ptrPreviousException
        = (Exception*)NelsonConfiguration::getInstance()->getLastErrorException(getID());
    if (ptrPreviousException) {
        delete ptrPreviousException;
    }
    try {
        Exception* ptrException = new Exception(e);
        NelsonConfiguration::getInstance()->setLastErrorException(getID(), ptrException);
    } catch (const std::bad_alloc&) {
        NelsonConfiguration::getInstance()->setLastErrorException(getID(), nullptr);
        return false;
    }
    return true;
}
//=============================================================================
Exception
Evaluator::getLastErrorException()
{
    Exception* ptrException = static_cast<Exception*>(
        NelsonConfiguration::getInstance()->getLastErrorException(getID()));
    Exception lastException;
    if (ptrException) {
        lastException = *ptrException;
    }
    return lastException;
}
//=============================================================================
void
Evaluator::resetLastErrorException()
{
    Exception* ptrException
        = (Exception*)NelsonConfiguration::getInstance()->getLastErrorException(getID());
    if (ptrException) {
        delete ptrException;
    }
    NelsonConfiguration::getInstance()->setLastErrorException(getID(), nullptr);
}
//=============================================================================
Exception
Evaluator::getLastWarningException()
{
    Exception* ptrException = static_cast<Exception*>(
        NelsonConfiguration::getInstance()->getLastWarningException(getID()));
    Exception lastException;
    if (ptrException) {
        lastException = *ptrException;
    }
    return lastException;
}
//=============================================================================
void
Evaluator::resetLastWarningException()
{
    Exception* ptrException = static_cast<Exception*>(
        NelsonConfiguration::getInstance()->getLastWarningException(getID()));
    if (ptrException) {
        delete ptrException;
    }
    NelsonConfiguration::getInstance()->setLastWarningException(getID(), nullptr);
}
//=============================================================================
bool
Evaluator::setLastWarningException(const Exception& e)
{
    Exception* ptrPreviousException = static_cast<Exception*>(
        NelsonConfiguration::getInstance()->getLastWarningException(getID()));
    if (ptrPreviousException) {
        delete ptrPreviousException;
    }
    try {
        Exception* ptrException = new Exception(e);
        NelsonConfiguration::getInstance()->setLastWarningException(getID(), ptrException);
    } catch (const std::bad_alloc&) {
        NelsonConfiguration::getInstance()->setLastWarningException(getID(), nullptr);
        return false;
    }
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
int
Evaluator::getDebugDepth()
{
    return depth;
}
//=============================================================================
void
Evaluator::increaseDebugDepth()
{
    depth++;
}
//=============================================================================
void
Evaluator::decreaseDebugDepth()
{
    depth--;
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
void
Evaluator::setCLI(bool bCLI)
{
    InCLI = bCLI;
}
//=============================================================================
bool
Evaluator::getCLI()
{
    return InCLI;
}
//=============================================================================
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
static bool doOnce = true;
//=============================================================================
void
setNamedMutexNelsonReady()
{
    openIsReadyNelsonMutex((int)boost::interprocess::ipcdetail::get_current_process_id());
}
//=============================================================================
void
Evaluator::evalCLI()
{
    while (true) {
        if (!bpActive) {
            clearStacks();
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
        // scan the line and tokenize it
        AbstractSyntaxTree::clearReferences();
        setLexBuffer(commandLine);
        try {
            bool bContinueLine = lexCheckForMoreInput(0);
            AbstractSyntaxTree::deleteReferences();
            if (bContinueLine) {
                int lastCount = getContinuationCount();
                std::wstring lines = commandLine;
                bool enoughInput = false;
                while (!enoughInput) {
                    commandLine = io->getLine(L"");
                    if (commandLine == L"\n" || commandLine.empty()) {
                        if (NelsonConfiguration::getInstance()->getInterruptPending(ID)) {
                            commandLine.clear();
                            return;
                        }
                        enoughInput = true;
                    } else {
                        lines.append(commandLine);
                        AbstractSyntaxTree::clearReferences();
                        setLexBuffer(lines);
                        enoughInput = !lexCheckForMoreInput(lastCount);
                        AbstractSyntaxTree::deleteReferences();
                        lastCount = getContinuationCount();
                        if (enoughInput) {
                            lines.append(L"\n");
                        }
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
ArrayOfVector
Evaluator::extractClass(const ArrayOf& r, const stringVector& subtypes,
    const ArrayOfVector& subsindices, bool& haveFunction)
{
    haveFunction = false;
    std::string currentClass = ClassName(r);
    Context* _context = this->getContext();
    FunctionDef* funcDef = nullptr;
    std::string functionNamesimpleExtractClass
        = getOverloadFunctionName(currentClass, SUBSREF_OPERATOR_STR);
    if (_context->lookupFunction(functionNamesimpleExtractClass, funcDef)) {
        haveFunction = true;
        if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                || (funcDef->type() == NLS_MACRO_FUNCTION))) {
            Error(_W("Type function not valid."));
        }
        int nLhs = 1;

        ArrayOfVector argIn;
        argIn.push_back(r);

        stringVector fieldnames = { "type", "subs" };
        ArrayOfVector fieldvalues;

        Dimensions dims(1, subtypes.size());

        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames, false));
        ArrayOf substruct = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);

        ArrayOfVector typesVector(subtypes.size());
        ArrayOfVector subsVector(subsindices.size());

        for (auto& t : subtypes) {
            typesVector.push_back(ArrayOf::characterArrayConstructor(t));
        }
        for (auto& t : subsindices) {
            subsVector.push_back(t);
        }
        if (!typesVector.empty()) {
            substruct.setFieldAsList("type", typesVector);
            substruct.setFieldAsList("subs", subsVector);
        }

        argIn.push_back(substruct);

        CallStack backupCallStack = callstack;
        ArrayOfVector rv = funcDef->evaluateFunction(this, argIn, nLhs);
        callstack = backupCallStack;
        return rv;
    }
    return {};
}
//=============================================================================
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
size_t
Evaluator::countSubExpressions(AbstractSyntaxTreePtr t)
{
    size_t count = 0;
    while (t != nullptr) {
        t = t->right;
        count++;
    }
    return count;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
