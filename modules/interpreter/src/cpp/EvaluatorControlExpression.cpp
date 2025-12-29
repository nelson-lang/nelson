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
#include "Evaluator.hpp"
#include "TextToNumber.hpp"
#include "i18n.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
#include "Operators.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
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
};
std::vector<endData> endStack;
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
    Profiler* profiler = Profiler::getInstance();
    uint64 ticProfiling = profiler->tic();
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
    case OP_NULL: {
        if (t->text.empty()) {
            callstack.pushID((size_t)t->getContext());
            std::wstring msg;
            msg = ERROR_UNRECOGNIZED_EXPRESSION + L"\ncode: " + std::to_wstring(t->type);
            if (!t->text.empty()) {
                msg = msg + L"\ntext: " + utf8_to_wstring(t->text);
            }
            Error(msg);
            callstack.popID();
        }
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
        profiler->toc(ticProfiling, stack);
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
        const endData& enddatat = endStack.back();
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
        retval = ArrayOf::doubleConstructor(textToDouble(t->text));
    } break;
    case const_float_node: {
        retval = ArrayOf::singleConstructor((textToSingle(t->text)));
    } break;
    case const_uint8_node: {
        retval = ArrayOf::uint8Constructor(textToUint8(t->text));
    } break;
    case const_int8_node: {
        retval = ArrayOf::int8Constructor(textToInt8(t->text));
    } break;
    case const_uint16_node: {
        retval = ArrayOf::uint16Constructor(textToUint16(t->text));
    } break;
    case const_int16_node: {
        retval = ArrayOf::int16Constructor(textToInt16(t->text));
    } break;
    case const_uint32_node: {
        retval = ArrayOf::uint32Constructor(textToUint32(t->text));
    } break;
    case const_int32_node: {
        retval = ArrayOf::int32Constructor(textToInt32(t->text));
    } break;
    case const_int64_node: {
        retval = ArrayOf::int64Constructor(textToInt64(t->text));
    } break;
    case const_uint64_node: {
        retval = ArrayOf::uint64Constructor(textToUint64(t->text));
    } break;
    case const_int_node: {
        retval = ArrayOf::doubleConstructor(textToDouble(t->text));
    } break;
    case const_character_array_node: {
        retval = ArrayOf::characterArrayConstructor(t->text);
    } break;
    case const_string_node: {
        retval = ArrayOf::stringArrayConstructor(t->text);
    } break;
    case const_dcomplex_node: {
        double val = textToDouble(t->text);
        if (approximatelyEqual(val, 0, std::numeric_limits<double>::epsilon())) {
            retval = ArrayOf::doubleConstructor(0.);
        } else {
            retval = ArrayOf::dcomplexConstructor(0, val);
        }
    } break;
    case const_complex_node: {
        single val = textToSingle(t->text);
        if (approximatelyEqual(val, 0, std::numeric_limits<single>::epsilon())) {
            retval = ArrayOf::singleConstructor(0.);
        } else {
            retval = ArrayOf::complexConstructor(0, val);
        }
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
    if (t == nullptr) {
        return m;
    }
    callstack.pushID(t->getContext());
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
                if (subRoot.isClassType()) {
                    tmp = 0;
                } else {
                    tmp = dim.getElementCount();
                }
                if (tmp == 0) {
                    m.push_back(ArrayOf::characterArrayConstructor(":"));
                } else {
                    m.push_back(ArrayOf::integerRangeConstructor(1, 1, tmp, true));
                }
            } else {
                if (subRoot.isClassType()) {
                    tmp = 0;
                } else {
                    tmp = dim.getDimensionLength(index);
                }
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

} // namespace Nelson
//=============================================================================
