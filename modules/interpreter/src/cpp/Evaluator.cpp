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
//=============================================================================
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <stdio.h>
#include <errno.h>
#include <iostream>
#include <math.h>
#include "Evaluator.hpp"
#include "Exception.hpp"
#include "LessEquals.hpp"
#include "Transpose.hpp"
#include "DotLeftDivide.hpp"
#include "DotRightDivide.hpp"
#include "RightDivide.hpp"
#include "LeftDivide.hpp"
#include "Negate.hpp"
#include "DotPower.hpp"
#include "NotEquals.hpp"
#include "And.hpp"
#include "Not.hpp"
#include "Or.hpp"
#include "Keywords.hpp"
#include "ArrayOf.hpp"
#include "ParserInterface.hpp"
#include "LexerInterface.hpp"
#include "Interface.hpp"
#include "Serialize.hpp"
#include "MacroFunctionDef.hpp"
#include "ClassName.hpp"
#include "OverloadDisplay.hpp"
#include "characters_encoding.hpp"
#include "FileParser.hpp"
#include "MainEvaluator.hpp"
#include "CommandQueue.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "Error.hpp"
#include "VertCatOperator.hpp"
#include "HorzCatOperator.hpp"
#include "AstManager.hpp"
#include "PathFuncManager.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "OverloadBinaryOperator.hpp"
#include "OverloadUnaryOperator.hpp"
#include "OverloadTernaryOperator.hpp"
#include "ComplexTranspose.hpp"
#include "OverloadRequired.hpp"
#include "NotEquals.hpp"
#include "PathFuncManager.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "Error.hpp"
#include "VertCat.hpp"
#include "HorzCat.hpp"
#include "AstManager.hpp"
#include "PathFuncManager.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "CheckIfWhileCondition.hpp"
#include "Warning.hpp"
#include "characters_encoding.hpp"
#include "UnaryMinus.hpp"
#include "UnaryPlus.hpp"
#include "NelsonConfiguration.hpp"
#include "Colon.hpp"
//=============================================================================
#ifdef _MSC_VER
#define strdup _strdup
#endif
namespace Nelson {

/**
 * Stores the current array for which to apply the "end" expression to.
 */
class endData
{
public:
    ArrayOf endArray;
    int index = 0;
    size_t count = 0;
    endData(ArrayOf p, int ndx, size_t cnt)
    {
        endArray = p;
        index = ndx;
        count = cnt;
    }
    ~endData(){};
};
std::vector<endData> endStack;

void
sigInterrupt(int arg)
{
    NelsonConfiguration::getInstance()->setInterruptPending(true);
}

void
Evaluator::pushID(int a)
{
    if (!cstack.empty()) {
        if (cstack.size() > cstack.capacity() - 100) {
            cstack.reserve(cstack.capacity() * 2);
        }
        cstack.push_back(StackEntry(cstack.back().cname, cstack.back().detail, a));
    } else {
        cstack.push_back(StackEntry("base", "base", a));
    }
}

void
Evaluator::popID()
{
    if (!cstack.empty()) {
        cstack.pop_back();
    } else {
        io->outputMessage("IDERROR\n");
    }
}

void
Evaluator::resetState()
{
    state = NLS_STATE_OK;
}

void
Evaluator::clearStacks()
{
    cstack.clear();
    cstack.reserve(64);
}

State
Evaluator::setState(State newState)
{
    State previousState = state;
    state = newState;
    return previousState;
}

State
Evaluator::getState()
{
    return state;
}

int
Evaluator::getExitCode()
{
    return exitCode;
}

void
Evaluator::setExitCode(int _exitCode)
{
    exitCode = _exitCode;
}

ArrayOfVector
Evaluator::rowDefinition(ASTPtr t)
{
    pushID(t->context());
    if (t->opNum != OP_SEMICOLON) {
        Error(ERROR_AST_SYNTAX_ERROR);
    }
    ASTPtr s = t->down;
    ArrayOfVector expl = expressionList(s);
    ArrayOfVector retval(expl);
    popID();
    return retval;
}

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
Evaluator::matrixDefinition(ASTPtr t)
{
    ArrayOfMatrix m;
    // m.reserve(4096);
    if (t->opNum != OP_BRACKETS) {
        Error(ERROR_AST_SYNTAX_ERROR);
    }
    ASTPtr s = t->down;
    pushID(s->context());
    while (s != nullptr) {
        m.push_back(rowDefinition(s));
        s = s->right;
    }
    ArrayOfVector v;
    for (indexType k = 0; k < m.size(); k++) {
        ArrayOf h = HorzCatOperator(this, m[k]);
        v.push_back(h);
    }
    ArrayOf res = VertCatOperator(this, v);
    popID();
    return res;
}

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
Evaluator::cellDefinition(ASTPtr t)
{
    ArrayOfMatrix m;
    if (t->opNum != OP_BRACES) {
        Error(ERROR_AST_SYNTAX_ERROR);
    }
    ASTPtr s = t->down;
    pushID(s->context());
    while (s != nullptr) {
        m.push_back(rowDefinition(s));
        s = s->right;
    }
    ArrayOf retval(ArrayOf::cellConstructor(m));
    popID();
    return retval;
}

bool
Evaluator::needToOverloadOperator(const ArrayOf& a)
{
    return ((a.getDataClass() == NLS_STRUCT_ARRAY) || (a.getDataClass() == NLS_CELL_ARRAY)
        || (a.getDataClass() == NLS_STRING_ARRAY) || a.isSparse() || a.isHandle());
}

ArrayOf
Evaluator::EndReference(ArrayOf v, indexType index, size_t count)
{
    Dimensions dim(v.getDimensions());
    ArrayOf res;
    if (count == 1) {
        res = ArrayOf::doubleConstructor((double)dim.getElementCount());
    } else {
        res = ArrayOf::doubleConstructor((double)dim.getDimensionLength(index));
    }
    return res;
}

ArrayOf
Evaluator::expression(ASTPtr t)
{
    pushID(t->context());
    ArrayOf retval;
    // by default as the target we create double
    if (t->type == const_int_node) {
        retval = ArrayOf::doubleConstructor(atof(t->text.c_str()));
    } else if (t->type == const_uint64_node) {
        char* endptr = nullptr;
        unsigned long long int v = strtoull(t->text.c_str(), &endptr, 10);
        uint64 r = (uint64)v;
        retval = ArrayOf::uint64Constructor(r);
    } else if (t->type == const_float_node) {
        boost::replace_all(t->text, "D", "e");
        boost::replace_all(t->text, "d", "e");
        retval = ArrayOf::singleConstructor(((float)atof(t->text.c_str())));
    } else if (t->type == const_double_node) {
        boost::replace_all(t->text, "D", "e");
        boost::replace_all(t->text, "d", "e");
        retval = ArrayOf::doubleConstructor(atof(t->text.c_str()));
    } else if (t->type == const_character_array_node) {
        retval = ArrayOf::characterArrayConstructor(std::string(t->text.c_str()));
    } else if (t->type == const_string_node) {
        retval = ArrayOf::stringArrayConstructor(std::string(t->text.c_str()));
    } else if (t->type == const_complex_node || t->type == const_dcomplex_node) {
        boost::replace_all(t->text, "D", "e");
        boost::replace_all(t->text, "d", "e");
        double val = atof(t->text.c_str());
        if (val == 0.) {
            retval = ArrayOf::doubleConstructor(0.);
        } else {
            retval = ArrayOf::dcomplexConstructor(0, val);
        }
    } else if (t->type == reserved_node) {
        if (t->tokenNumber == NLS_KEYWORD_END) {
            if (endStack.empty()) {
                Error(ERROR_END_ILLEGAL);
            }
            endData enddatat(endStack.back());
            retval = EndReference(enddatat.endArray, enddatat.index, enddatat.count);
        } else {
            Error(ERROR_UNRECOGNIZED_NODE);
        }
    } else {
        switch (t->opNum) {
        case OP_COLON:
            if ((t->down != nullptr) && (t->down->opNum == (OP_COLON))) {
                retval = doubleColon(t);
            } else {
                retval = unitColon(t);
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
            retval = additionOperator(t);
        } break;
        case OP_SUBTRACT: {
            retval = subtractionOperator(t);
        } break;
        case OP_TIMES: {
            retval = mtimesOperator(t);
        } break;
        case OP_SOR: {
            retval = shortCutOrOperator(t);
        } break;
        case OP_OR: {
            retval = orOperator(t);
        } break;
        case OP_SAND: {
            retval = shortCutAndOperator(t);
        } break;
        case OP_AND: {
            retval = andOperator(t);
        } break;
        case OP_LT: {
            retval = ltOperator(t);
        } break;
        case OP_LEQ: {
            retval = leOperator(t);
        } break;
        case OP_GT: {
            retval = gtOperator(t);
        } break;
        case OP_GEQ: {
            retval = geOperator(t);
        } break;
        case OP_EQ: {
            retval = eqOperator(t);
        } break;
        case OP_NEQ: {
            retval = neOperator(t);
        } break;
        case OP_DOT_TIMES: {
            retval = timesOperator(t);
        } break;
        case OP_POS: {
            bool bSuccess = false;
            ArrayOf a = expression(t->down);
            if (overloadOnBasicTypes) {
                retval = OverloadUnaryOperator(this, a, "uplus", bSuccess);
            }
            if (!bSuccess) {
                bool needToOverload = false;
                retval = UnaryPlus(a, needToOverload);
                if (needToOverload) {
                    retval = OverloadUnaryOperator(this, a, "uplus");
                }
            }
        } break;
        case OP_NEG: {
            bool bSuccess = false;
            ArrayOf a = expression(t->down);
            if (overloadOnBasicTypes) {
                retval = OverloadUnaryOperator(this, a, "uminus", bSuccess);
            }
            if (!bSuccess) {
                bool needToOverload = false;
                retval = UnaryMinus(a, needToOverload);
                if (needToOverload) {
                    retval = OverloadUnaryOperator(this, a, "uminus");
                }
            }
        } break;
        case OP_NOT: {
            retval = notOperator(t);
        } break;
        case OP_TRANSPOSE: {
            bool bSuccess = false;
            ArrayOf a = expression(t->down);
            if (overloadOnBasicTypes) {
                retval = OverloadUnaryOperator(this, a, "ctranspose", bSuccess);
            }
            if (!bSuccess) {
                bool needToOverload = false;
                retval = ComplexTranspose(a, needToOverload);
                if (needToOverload) {
                    retval = OverloadUnaryOperator(this, a, "ctranspose");
                }
            }
        } break;
        case OP_DOT_TRANSPOSE: {
            bool bSuccess = false;
            ArrayOf a = expression(t->down);
            if (overloadOnBasicTypes) {
                retval = OverloadUnaryOperator(this, a, "transpose", bSuccess);
            }
            if (!bSuccess) {
                bool needToOverload = false;
                retval = Transpose(a, needToOverload);
                if (needToOverload) {
                    retval = OverloadUnaryOperator(this, a, "transpose");
                }
            }
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
            retval = OverloadBinaryOperator(
                this, expression(t->down), expression(t->down->right), "mrdivide");
        } break;
        case OP_LDIV: {
            retval = OverloadBinaryOperator(
                this, expression(t->down), expression(t->down->right), "mldivide");
        } break;
        case OP_DOT_RDIV: {
            retval = OverloadBinaryOperator(
                this, expression(t->down), expression(t->down->right), "rdivide");
        } break;
        case OP_DOT_LDIV: {
            retval = OverloadBinaryOperator(
                this, expression(t->down), expression(t->down->right), "ldivide");
        } break;
        case OP_POWER: {
            FunctionDef* mpowerFuncDef = nullptr;
            if (!context->lookupFunction("mpower", mpowerFuncDef)) {
                Error(_W("mpower function not found."));
            }
            if (!((mpowerFuncDef->type() == NLS_BUILT_IN_FUNCTION)
                    || (mpowerFuncDef->type() == NLS_MACRO_FUNCTION))) {
                Error(_W("Type function not valid."));
            }
            int nLhs = 1;
            ArrayOfVector args;
            args.push_back(expression(t->down));
            args.push_back(expression(t->down->right));
            ArrayOfVector res = mpowerFuncDef->evaluateFunction(this, args, nLhs);
            if (res.size() == 0) {
                Error(_W("mpower returned fewer outputs than expected."));
            }
            retval = res[0];
        } break;
        case OP_DOT_POWER: {
            ArrayOf A = expression(t->down);
            ArrayOf B = expression(t->down->right);
            bool bSuccess = false;
            if (overloadOnBasicTypes) {
                retval = OverloadBinaryOperator(this, A, B, "power", bSuccess);
            }
            if (!bSuccess) {
                bool needToOverload;
                retval = DotPower(A, B, needToOverload);
                if (needToOverload) {
                    ArrayOfVector args;
                    args.push_back(A);
                    args.push_back(B);
                    retval = OverloadBinaryOperator(
                        this, expression(t->down), expression(t->down->right), "power");
                }
            }
        } break;
        default:
            Error(ERROR_UNRECOGNIZED_EXPRESSION);
        }
    }
    popID();
    return retval;
}

//!
//@Module COLON Index Generation Operator
//@@Section OPERATORS
//@@Usage
// There are two distinct syntaxes for the colon @|:| operator - the two argument form
//@[
//  y = a : c
//@]
// and the three argument form
//@[
//  y = a : b : c
//@]
// The two argument form is exactly equivalent to @|a:1:c|.  The output @|y| is the vector
//\[
//  y = [a,a+b,a+2b,\ldots,a+nb]
//\]
// where @|a+nb <= c|.  There is a third form of the colon operator, the
// no-argument form used in indexing (see @|indexing| for more details).
//@@Examples
// Some simple examples of index generation.
//@<
// y = 1:4
//@>
// Now by half-steps:
//@<
// y = 1:.5:4
//@>
// Now going backwards (negative steps)
//@<
// y = 4:-.5:1
//@>
// If the endpoints are the same, one point is generated, regardless of the step size (middle
// argument)
//@<
// y = 4:1:4
//@>
// If the endpoints define an empty interval, the output is an empty matrix:
//@<
// y = 5:4
//@>
//!
ArrayOf
Evaluator::unitColon(ASTPtr t)
{
    ArrayOf a, b;
    pushID(t->context());
    a = expression(t->down);
    b = expression(t->down->right);
    bool bSuccess = false;
    ArrayOf retval;
    if (mustOverloadBasicTypes()) {
        retval = OverloadBinaryOperator(this, a, b, "colon", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        retval = Colon(a, b, needToOverload);
        if (needToOverload) {
            retval = OverloadBinaryOperator(this, a, b, "colon");
        }
    }
    popID();
    return retval;
}

ArrayOf
Evaluator::doubleColon(ASTPtr t)
{
    ArrayOf a, b, c;
    pushID(t->context());
    a = expression(t->down->down);
    b = expression(t->down->down->right);
    c = expression(t->down->right);
    ArrayOf retval;
    bool bSuccess = false;
    if (mustOverloadBasicTypes()) {
        retval = OverloadTernaryOperator(this, a, b, c, "colon", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        retval = Colon(a, b, c, needToOverload);
        if (needToOverload) {
            retval = OverloadTernaryOperator(this, a, b, c, "colon");
        }
    }
    popID();
    return retval;
}

/**
 * An expressionList allows for expansion of cell-arrays
 * and structure arrays.  Works by first screening rhs-expressions
 * through rhsExpression, which can return
 * a vector of variables.
 */
ArrayOfVector
Evaluator::expressionList(ASTPtr t)
{
    ArrayOfVector m;
    ArrayOfVector n;
    ASTPtr root;
    indexType tmp = 0;
    indexType endVal = 0;
    if (t == nullptr) {
        return m;
    }
    pushID(t->context());
    root = t;
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
                } else {
                    n = ArrayOfVector();
                }
            }
            for (size_t i = 0; i < n.size(); i++) {
                m.push_back(n[i]);
            }
        } else if (t->type == non_terminal && t->opNum == (OP_ALL)) {
            Error(_W("Illegal use of the ':' operator"));
        } else {
            // Call the expression
            m.push_back(expression(t));
        }
        t = t->right;
    }
    popID();
    return m;
}

ArrayOfVector
Evaluator::expressionList(ASTPtr t, ArrayOf subRoot)
{
    ArrayOfVector m;
    ArrayOfVector n;
    ASTPtr root;
    indexType index = 0, tmp = 0;
    indexType endVal = 0;
    if (t == nullptr) {
        return m;
    }
    pushID(t->context());
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
                } else {
                    n = ArrayOfVector();
                }
            }
            for (size_t i = 0; i < n.size(); i++) {
                m.push_back(n[i]);
            }
        } else if (t->type == non_terminal && t->opNum == (OP_ALL)) {
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
            endStack.push_back(endData(subRoot, (int)index, count));
            // Call the expression
            m.push_back(expression(t));
            endStack.pop_back();
        }
        index++;
        t = t->right;
    }
    popID();
    return m;
}

ArrayOfVector
Evaluator::subsindex(ArrayOfVector m)
{
    ArrayOfVector n;
    for (size_t k = 0; k < m.size(); k++) {
        ArrayOf t = OverloadUnaryOperator(this, m[k], "subsindex");
        t.promoteType(NLS_UINT32);
        size_t len = t.getLength();
        uint32* dp = (uint32*)t.getReadWriteDataPointer();
        for (size_t j = 0; j < len; j++) {
            dp[j]++;
        }
        n.push_back(t);
    }
    return n;
}

bool
Evaluator::conditionedStatement(ASTPtr t)
{
    bool conditionState;
    if (t->opNum != OP_CSTAT) {
        Error(ERROR_AST_SYNTAX_ERROR);
    }
    ASTPtr s = t->down;
    pushID(s->context());
    ArrayOf condVar;
    condVar = expression(s);
    conditionState = checkIfWhileCondition(condVar);
    ASTPtr codeBlock = s->right;
    if (conditionState) {
        block(codeBlock);
    }
    popID();
    return conditionState;
}

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
Evaluator::testCaseStatement(ASTPtr t, ArrayOf s)
{
    bool caseMatched;
    ArrayOf r;
    pushID(t->context());
    if (t->type != reserved_node || t->tokenNumber != NLS_KEYWORD_CASE) {
        Error(ERROR_AST_SYNTAX_ERROR);
    }
    t = t->down;
    r = expression(t);
    caseMatched = s.testForCaseMatch(r);
    if (caseMatched) {
        block(t->right);
    }
    popID();
    return caseMatched;
}

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
Evaluator::tryStatement(ASTPtr t)
{
    // Turn off autostop for this statement block
    bool autostop_save = autostop;
    autostop = false;
    // Get the state of the IDnum stack and the
    // contextStack and the cnameStack
    size_t stackdepth = cstack.size();
    try {
        block(t);
    } catch (const Exception&) {
        while (cstack.size() > stackdepth) {
            cstack.pop_back();
        }
        //      cname = cstack.back().cname;
        t = t->right;
        if (t != nullptr) {
            autostop = autostop_save;
            block(t);
        }
    }
    autostop = autostop_save;
}

bool
Evaluator::AutoStop()
{
    return autostop;
}

void
Evaluator::AutoStop(bool a)
{
    autostop = a;
}

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
Evaluator::switchStatement(ASTPtr t)
{
    ArrayOf switchVal;
    pushID(t->context());
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
            ASTPtr s = t->down;
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
    popID();
}

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
Evaluator::ifStatement(ASTPtr t)
{
    pushID(t->context());
    bool condStat = conditionedStatement(t);
    if (!condStat) {
        t = t->right;
        // Check for additional conditions
        if (t != nullptr) {
            bool elseifMatched = false;
            if (t->opNum == (OP_ELSEIFBLOCK)) {
                ASTPtr s = t->down;
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
    popID();
}

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
Evaluator::whileStatement(ASTPtr t)
{
    ASTPtr testCondition;
    ArrayOf condVar;
    ASTPtr codeBlock;
    bool conditionTrue;
    bool breakEncountered;
    pushID(t->context());
    testCondition = t;
    codeBlock = t->right;
    breakEncountered = false;
    condVar = expression(testCondition);
    conditionTrue = checkIfWhileCondition(condVar);
    context->enterLoop();
    while (conditionTrue && !breakEncountered) {
        block(codeBlock);
        if (state == NLS_STATE_RETURN || state == NLS_STATE_ABORT || state == NLS_STATE_QUIT) {
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
    popID();
}

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
//  for variable=expression
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
void
Evaluator::forStatement(ASTPtr t)
{
    ASTPtr codeBlock;
    ArrayOf indexSet;
    ArrayOf indexNum;
    std::string indexVarName;
    ArrayOf indexVar;
    indexType elementCount = 0;
    if (t == nullptr) {
        resetState();
        context->exitLoop();
        return;
    } else {
        pushID(t->context());
    }
    /* Get the name of the indexing variable */
    indexVarName = t->text;
    /* Evaluate the index set */
    indexSet = expression(t->down);
    if (indexSet.isEmpty()) {
        return;
    }
    /* Get the code block */
    codeBlock = t->right;
    bool isRowVector = indexSet.isRowVector();
    bool isColumnVector = indexSet.isColumnVector();
    if (isRowVector) {
        elementCount = indexSet.getLength();
    } else if (isColumnVector) {
        elementCount = 1;
    } else {
        elementCount = indexSet.getDimensions().getColumns();
    }
    context->enterLoop();
    for (indexType elementNumber = 0; elementNumber < elementCount; elementNumber++) {
        if (isRowVector) {
            indexVar = indexSet.getValueAtIndex(elementNumber);
        } else {
            indexType tmp = indexSet.getDimensions().getRows();
            ArrayOfVector m;
            m.push_back(ArrayOf::integerRangeConstructor(1, 1, tmp, false));
            m.push_back(ArrayOf::doubleConstructor(elementNumber + 1));
            indexVar = indexSet.getNDimSubset(m);
        }
        bool bInserted = context->insertVariable(indexVarName, indexVar);
        if (!bInserted) {
            Error(_W("Redefining permanent variable."));
        }
        block(codeBlock);
        if (state == NLS_STATE_RETURN || state == NLS_STATE_ABORT || state == NLS_STATE_QUIT) {
            break;
        }
        if (state == NLS_STATE_CONTINUE) {
            resetState();
        }
        if (state == NLS_STATE_BREAK) {
            resetState();
            break;
        }
    }
    context->exitLoop();
    popID();
}

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

void
Evaluator::handleDebug(int fullcontext)
{
    int linenumber = fullcontext & 0xffff;
    if (debugActive) {
        if (inStepMode) {
            if ((stepTrap.cname == cstack.back().cname) && (stepTrap.tokid == linenumber)) {
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
                    if ((bpStack[j].cname == cstack.back().cname)
                        && (bpStack[j].tokid == fullcontext)) {
                        found = true;
                    } else {
                        found = false;
                        j++;
                    }
                } else {
                    if ((bpStack[j].cname == cstack.back().cname)
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

void
Evaluator::statementType(ASTPtr t, bool printIt)
{
    ArrayOfVector m;
    FunctionDef* fdef;
    if (haveEventsLoop()) {
        ProcessEventsDynamicFunctionWithoutWait();
    }
    if (t == nullptr) {
        return;
    }
    pushID(t->context());
    // check the debug flag
    int fullcontext = t->context();
    handleDebug(fullcontext);
    if (t->isEmpty()) {
        /* Empty statement */
    } else if (t->opNum == (OP_ASSIGN)) {
        if (t->down->down == nullptr) {
            ArrayOf b(expression(t->down->right));
            bool bInserted = context->insertVariable(t->down->text, b);
            if (!bInserted) {
                Error(_W("Redefining permanent variable."));
            }
            if (printIt) {
                io->outputMessage(t->down->text);
                io->outputMessage(L" =\n\n");
                OverloadDisplay(this, b);
            }
        } else {
            ArrayOf expr(expression(t->down->right));
            ArrayOf c(assignExpression(t->down, expr));
            if (!c.isHandle()) {
                bool bInserted = context->insertVariable(t->down->text, c);
                if (!bInserted) {
                    Error(_W("Redefining permanent variable."));
                }
                if (printIt) {
                    io->outputMessage(t->down->text);
                    io->outputMessage(L" =\n\n");
                    OverloadDisplay(this, c);
                }
            }
        }
    } else if (t->opNum == (OP_MULTICALL)) {
        multiFunctionCall(t->down, printIt);
    } else if (t->opNum == (OP_SCALL)) {
        ArrayOfVector m = specialFunctionCall(t->down, printIt);
        if (m.size() > 0) {
            io->outputMessage(L"\nans =\n\n");
            OverloadDisplay(this, m[0]);
            context->insertVariable("ans", m[0]);
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
        case NLS_KEYWORD_QUIT:
            state = NLS_STATE_QUIT;
            break;
        case NLS_KEYWORD_ABORT:
            state = NLS_STATE_ABORT;
            if (depth) {
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
                io->outputMessage(L"\nans =\n\n");
                OverloadDisplay(this, b);
            }
        } else if (t->opNum == OP_RHS) {
            m = rhsExpression(t->down);
            if (m.size() == 0) {
                b = ArrayOf::emptyConstructor();
            } else {
                b = m[0];
                if (printIt && (state < NLS_STATE_QUIT)) {
                    // io->outputMessage(L"ans =\n\n");
                    io->outputMessage("\n");
                    for (size_t j = 0; j < m.size(); j++) {
                        if (m.size() > 1) {
                            char buffer[1000];
                            sprintf(buffer, _("\n%d of %d:\n").c_str(), j + 1, m.size());
                            io->outputMessage(buffer);
                        }
                        OverloadDisplay(this, m[j]);
                    }
                }
            }
        } else {
            b = expression(t);
            if (printIt && (state < NLS_STATE_QUIT)) {
                io->outputMessage(L"\nans =\n\n");
                OverloadDisplay(this, b);
            }
        }
        if (state == NLS_STATE_QUIT || state == NLS_STATE_ABORT) {
            popID();
            return;
        }
        if (bUpdateAns) {
            context->insertVariable("ans", b);
        }
    }
    popID();
}

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

//
void
Evaluator::statement(ASTPtr t)
{
    try {
        pushID(t->context());
        if (t->opNum == (OP_QSTATEMENT)) {
            statementType(t->down, false);
        } else if (t->opNum == (OP_RSTATEMENT)) {
            statementType(t->down, true && bEchoMode);
        } else {
            statementType(t->down, true && bEchoMode);
        }
        popID();
    } catch (const Exception&) {
        popID();
        throw;
        /*
        if (autostop && !InCLI)
        {
        e.printMe(io);
        stackTrace(true);
        debugCLI();
        if (state < NLS_STATE_QUIT)
        {
        resetState();
        }
        popID();
        }
        else
        {
        popID();
        throw;
        }
        */
    }
}

void
Evaluator::block(ASTPtr t)
{
    try {
        ASTPtr s = t->down;
        if (state < NLS_STATE_QUIT) {
            resetState();
        }
        while ((state < NLS_STATE_QUIT) && s != nullptr) {
            if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                io->outputMessage(L"\n" + MSG_CTRL_C_DETECTED);
                state = NLS_STATE_ABORT;
                NelsonConfiguration::getInstance()->setInterruptPending(false);
                return;
            } else {
                statement(s);
                if (state == NLS_STATE_BREAK || state == NLS_STATE_CONTINUE
                    || state == NLS_STATE_RETURN || state == NLS_STATE_ABORT
                    || state == NLS_STATE_QUIT) {
                    break;
                }
                s = s->right;
            }
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

ArrayOf
Evaluator::simpleSubindexExpression(ArrayOf& r, ASTPtr t)
{
    Dimensions rhsDimensions;
    ArrayOfVector m;
    rhsDimensions = r.getDimensions();
    if (t->opNum == (OP_PARENS)) {
        m = expressionList(t->down, r);
        if (m.size() == 0) {
            Error(ERROR_INDEX_EXPRESSION_EXPECTED);
        } else if (m.size() == 1) {
            if (r.isClassStruct()) {
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

void
Evaluator::simpleAssign(ArrayOf& r, ASTPtr t, ArrayOf& value)
{
    ArrayOfVector vec;
    vec.push_back(value);
    simpleAssign(r, t, vec);
}

void
Evaluator::simpleAssign(ArrayOf& r, ASTPtr t, ArrayOfVector& value)
{
    Dimensions rhsDimensions;
    ArrayOfVector m;
    pushID(t->context());
    if (!r.isEmpty()) {
        rhsDimensions = r.getDimensions();
    } else if (t->opNum != OP_BRACES) {
        rhsDimensions = value[0].getDimensions();
    } else {
        rhsDimensions.makeScalar();
    }
    if (t->opNum == (OP_PARENS)) {
        m = expressionList(t->down, r);
        if (m.size() == 0) {
            Error(ERROR_INDEX_EXPRESSION_EXPECTED);
        } else if (m.size() == 1) {
            r.setVectorSubset(m[0], value[0]);
            popID();
            return;
        } else {
            r.setNDimSubset(m, value[0]);
            popID();
            return;
        }
    }
    if (t->opNum == (OP_BRACES)) {
        m = expressionList(t->down, r);
        if (m.size() == 0) {
            Error(ERROR_INDEX_EXPRESSION_EXPECTED);
        } else if (m.size() == 1) {
            if (r.isEmpty()) {
                m[0] = ArrayOf::doubleConstructor(1);
            }
            r.setVectorContentsAsList(m[0], value);
            popID();
            return;
        } else {
            r.setNDimContentsAsList(m, value);
            popID();
            return;
        }
    }
    if (t->opNum == (OP_DOT)) {
        if (r.isClassStruct()) {
            // TO DO
            Error(ERROR_NEED_TO_IMPLEMENT_ASSIGN);
        } else {
            std::string fieldname = t->down->text;
            if (r.isHandle()) {
                setHandle(r, fieldname, value);
            } else if (r.isStruct() || r.isEmpty()) {
                r.setFieldAsList(fieldname, value);
            } else {
                Error(ERROR_ASSIGN_TO_NON_STRUCT);
            }
        }
        popID();
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
        if (r.isHandle()) {
            setHandle(r, field, value);
        } else {
            r.setFieldAsList(field, value);
        }
        popID();
        return;
    }
    popID();
}

indexType
Evaluator::countLeftHandSides(ASTPtr t)
{
    ArrayOf lhs;
    if (!context->lookupVariable(t->text, lhs)) {
        lhs = ArrayOf::emptyConstructor();
    }
    ASTPtr s = t->down;
    if (s == nullptr) {
        return 1;
    }
    pushID(s->context());
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
            if (m[0].getLength() > 1) {
                Error(ERROR_PARENTHETICAL_EXPRESSION);
            }
            popID();
            return (m[0].getLength());
        } else {
            size_t i = 0;
            indexType outputCount = 1;
            while (i < m.size()) {
                m[i].toOrdinalType();
                outputCount *= m[i].getLength();
                i++;
            }
            if (outputCount > 1) {
                Error(ERROR_PARENTHETICAL_EXPRESSION);
            }
            popID();
            return (outputCount);
        }
    }
    if (s->opNum == (OP_BRACES)) {
        m = expressionList(s->down, lhs);
        if (m.size() == 0) {
            Error(ERROR_INDEX_EXPRESSION_EXPECTED);
        }
        if (m.size() == 1) {
            // m[0] should have only one element...
            m[0].toOrdinalType();
            popID();
            return (m[0].getLength());
        } else {
            size_t i = 0;
            indexType outputCount = 1;
            while (i < m.size()) {
                m[i].toOrdinalType();
                outputCount *= m[i].getLength();
                i++;
            }
            popID();
            return (outputCount);
        }
    }
    if (s->opNum == (OP_DOT)) {
        popID();
        return lhs.getLength();
    }
    popID();
    return (indexType)1;
}

ArrayOf
Evaluator::assignExpression(ASTPtr t, ArrayOf& val)
{
    ArrayOfVector vec;
    vec.push_back(val);
    return assignExpression(t, vec);
}

// If we got this far, we must have at least one subindex
ArrayOf
Evaluator::assignExpression(ASTPtr t, ArrayOfVector& value)
{
    pushID(t->context());
    if (t->down == nullptr) {
        ArrayOf retval(value[0]);
        value.erase(value.begin());
        popID();
        return retval;
    }
    // Get the variable in question
    ArrayOf lhs;
    bool bVarAlreadyExist = context->lookupVariable(t->text, lhs);
    if (!bVarAlreadyExist) {
        lhs = ArrayOf::emptyConstructor();
    }
    // Set up a stack
    ArrayOfVector stack;
    ASTPtrVector ref;
    ASTPtr s = t->down;
    ArrayOf data;
    data = lhs;
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
    popID();
    return lhs;
}

ArrayOfVector
Evaluator::specialFunctionCall(ASTPtr t, bool printIt)
{
    ArrayOfVector m;
    stringVector args;
    args.push_back(t->text);
    ASTPtr s = t->right;
    while (s) {
        args.push_back(s->text);
        s = s->right;
    }
    if (args.empty()) {
        return m;
    }
    ArrayOfVector n;
    for (size_t i = 1; i < args.size(); i++) {
        n.push_back(ArrayOf::characterArrayConstructor(args[i].c_str()));
    }
    FuncPtr val;
    pushID(t->context());
    if (!lookupFunction(args[0], val)) {
        Error(utf8_to_wstring(_("unable to resolve ") + args[0] + _(" to a function call")));
    }
    bool CLIFlagsave = InCLI;
    InCLI = false;
    try {
        m = val->evaluateFunction(this, n, 0);
    } catch (const Exception&) {
        InCLI = CLIFlagsave;
        popID();
        throw;
    }
    InCLI = CLIFlagsave;
    popID();
    return m;
}

void
Evaluator::addBreakpoint(StackEntry& bp)
{
    bpStack.push_back(bp);
    adjustBreakpoints();
    debugActive = true;
}

void
Evaluator::multiFunctionCall(ASTPtr t, bool printIt)
{
    ArrayOfVector m;
    ASTPtr s, fAST, saveLHS, cAST;
    ArrayOf c;
    // int lhsSize;
    FunctionDef* fptr;
    cAST = t;
    fAST = t->right;
    bool bDeal = false;
    pushID(fAST->context());
    ArrayOf r;
    if (!lookupFunction(fAST->text, fptr)) {
        bool isVar = context->lookupVariable(fAST->text, r);
        if (isVar) {
            if (r.isClassStruct()) {
                std::string className = r.getStructType();
                std::string extractionFunctionName = className + "_extraction";
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
    ASTPtr mptr = s;
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
        m = functionExpression(fptr, fAST, (int)lhsCount, false);
    }
    s = saveLHS;
    while ((s != nullptr) && (m.size() > 0)) {
        ArrayOf c(assignExpression(s->down, m));
        bool bInserted = context->insertVariable(s->down->text, c);
        if (!bInserted) {
            Error(_W("Redefining permanent variable."));
        }
        if (printIt) {
            io->outputMessage(s->down->text);
            io->outputMessage(L" =\n\n");
            OverloadDisplay(this, c);
        }
        s = s->right;
    }
    if (s != nullptr) {
        std::wstring message = _W("Function") + L" : " + utf8_to_wstring(fAST->text) + L"\n"
            + WARNING_OUTPUTS_NOT_ASSIGNED;
        Warning(message);
    }
    popID();
}

int
getArgumentIndex(stringVector list, const std::string& t)
{
    bool foundArg = false;
    std::string q;
    uint32 i;
    i = 0;
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
        return i;
    } else {
        return -1;
    }
}

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
Evaluator::functionExpression(FunctionDef* funcDef, ASTPtr t, int narg_out, bool outputOptional)
{
    ArrayOfVector m, n;
    ASTPtr s, q, p;
    stringVector keywords;
    ArrayOfVector keyvals;
    ASTPtrVector keyexpr;
    int* keywordNdx = nullptr;
    int* argTypeMap = nullptr;
    pushID(t->context());
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
#if 0
                        if (keywords.size() > 0)
                        {
                            // 	    if (funcDef->type() != NLS_MACRO_FUNCTION)
                            // 	      Error(L"out of order argument passing only supported for M files");
                            while (s != nullptr && s->opNum == OP_KEYWORD)
                            {
                                s = s->right;
                            }
                            if (s != nullptr)
                            {
                                q = s;
                                while (q->right != nullptr)
                                {
                                    if (q->right->opNum == OP_KEYWORD)
                                    {
                                        q->right = q->right->right;
                                    }
                                    else
                                    {
                                        q = q->right;
                                    }
                                }
                            }
                        }
#endif
                    m = expressionList(s);
                    // Check for keywords
                    if (keywords.size() > 0) {
                        // If keywords were used, we have to permute the
                        // entries of the arrayvector to the correct order.
                        stringVector arguments;
                        // Get the arguments from the MacroFunction pointer.
                        arguments = funcDef->arguments;
                        keywordNdx = new int[keywords.size()];
                        int maxndx;
                        maxndx = 0;
                        // Map each keyword to an argument number
                        for (size_t i = 0; i < (int)keywords.size(); ++i) {
                            int ndx;
                            ndx = getArgumentIndex(arguments, keywords[i]);
                            if (ndx == -1) {
                                Error(utf8_to_wstring(_("out-of-order argument /") + keywords[i]
                                    + _(" is not defined in the called function!")));
                            }
                            keywordNdx[i] = ndx;
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
                            totalCount = maxndx + 1;
                        } else {
                            totalCount = maxndx + 1 + (m.size() - holes);
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
                        for (size_t i = 0; i < totalCount; i++)
                            if (!filled[i]) {
                                toFill[i] = ArrayOf::emptyConstructor();
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
                        if (r.isClassStruct()) {
                            s = t->down;
                            if (s) {
                                if (s->opNum == (OP_DOT)) {
                                    s = s->down;
                                    return scalarArrayOfToArrayOfVector(r.getField(s->text));
                                }
                            }
                        }
                    }
                    Error(ERROR_ILLEGAL_EXPRESSION_IN_FUNCTION);
                }
            } else {
                m = ArrayOfVector();
            }
            // int nRhs = (int)m.size();
            // int nLhs = funcDef->outputArgCount();
            /*
            if ((funcDef->inputArgCount() >= 0) &&
            ((int)(m.size()) > funcDef->inputArgCount()))
            Error(std::string("Too many inputs to function ")+t->text);
            if ((funcDef->outputArgCount() >= 0) &&
            (narg_out > funcDef->outputArgCount() && !outputOptional))
            Error(std::string("Too many outputs to function ")+t->text);
            */
            CLIFlagsave = InCLI;
            InCLI = false;
            ArrayOf r;
            bool isVar = context->lookupVariable(t->text, r);
            bool haveDown = (t->down != nullptr);
            if (isVar) {
                // if it is a class C
                if (r.isClassStruct()) {
                    // C(X1, ..., XN)
                    // we call : C_extract(C, X1, ..., XN)
                    if (m.size() > 0) {
                        m.insert(m.begin(), r);
                    } else {
                        // C() or C
                        if (t->down != nullptr) {
                            s = t->down;
                            // C()
                            if (s->opNum == (OP_PARENS)) {
                                m.insert(m.begin(), r);
                            }
                            // C others ? C{}, C. ?
                            // we do currently nothing
                        } else {
                            // C
                            return scalarArrayOfToArrayOfVector(r);
                            /*
                                                            if (r.isFunctionHandle())
                                                            {
                                                                return
                               scalarArrayOfToArrayOfVector(r);
                                                            }
                            */
                        }
                    }
                }
                n = funcDef->evaluateFunction(this, m, narg_out);
            } else {
                n = funcDef->evaluateFunction(this, m, narg_out);
            }
            InCLI = CLIFlagsave;
            if (state == NLS_STATE_ABORT) {
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
                    if (argTypeMap) {
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
                        if (p == nullptr || !(p->type == non_terminal && p->opNum == OP_RHS)) {
                            Error(ERROR_MUST_HAVE_LVALUE);
                        }
                        if (p->down->down == nullptr && p->down->type == id_node) {
                            bool bInserted = context->insertVariable(p->down->text, m[i]);
                            if (!bInserted) {
                                Error(_W("Redefining permanent variable."));
                            }
                        } else {
                            ArrayOf c(assignExpression(p->down, m[i]));
                            bool bInserted = context->insertVariable(p->down->text, c);
                            if (!bInserted) {
                                Error(_W("Redefining permanent variable."));
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
        */
        popID();
        return n;
    } catch (const Exception&) {
        InCLI = CLIFlagsave;
        throw;
    }
    popID();
    delete[] keywordNdx;
    delete[] argTypeMap;
}

int
COST(int a, int b)
{
    return (((a) >= (b)) ? ((a) - (b)) : 10000);
}

#define MIN(a, b) (((a) < (b)) ? (a) : (b))

int
GetClosestLineNumber(ASTPtr t, int lineno)
{
    if (t == nullptr) {
        return 10000;
    }
    int linedwn = GetClosestLineNumber(t->down, lineno);
    int linerght = GetClosestLineNumber(t->right, lineno);
    int retval = (t->context() & 0xffff);
    ;
    int costthis = COST(retval, lineno);
    return (MIN(linedwn, MIN(linerght, costthis)));
}

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

bool
Evaluator::adjustBreakpoint(StackEntry& bp, bool dbstep)
{
    bool isFun;
    FuncPtr val;
    std::string cname = bp.detail;
    isFun = context->lookupFunction(cname, val);
    if (!isFun) {
        return false;
    }
    if (val->type() == NLS_MACRO_FUNCTION) {
        MacroFunctionDef* mptr;
        mptr = (MacroFunctionDef*)val;
        int clinenum = 10000;
        while (mptr) {
            ASTPtr code = mptr->code;
            int nxt = GetClosestLineNumber(code, bp.tokid & 0xffff);
            clinenum = MIN(clinenum, nxt);
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
        } else if (clinenum != 0) {
            bp.tokid = (bp.tokid & 0xffff) + clinenum;
        }
    } else {
        return false;
    }
    return true;
}

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

stringVector
Evaluator::getCallers(bool includeCurrent)
{
    stringVector callersName;
    size_t i = 0;
    while (i < this->cstack.size()) {
        if (this->cstack[i].tokid == 0) {
            size_t j = i + 1;
            while ((j < this->cstack.size()) && (this->cstack[j].cname == this->cstack[i].cname)
                && (this->cstack[j].detail == this->cstack[i].detail)
                && (this->cstack[j].tokid != 0)) {
                j++;
            }
            std::string functionname = this->cstack[j - 1].detail.c_str();
            if (boost::algorithm::starts_with(functionname, "built-in ")) {
                boost::algorithm::replace_all(functionname, "built-in ", "");
            } else {
                // remove all that is not functions
                bool bOK = !boost::algorithm::contains(functionname, "(")
                    && !boost::algorithm::contains(functionname, ")")
                    && !boost::algorithm::contains(functionname, "'")
                    && !boost::algorithm::contains(functionname, "/")
                    && !boost::algorithm::contains(functionname, "\\")
                    && !boost::algorithm::contains(functionname, " ")
                    && !boost::algorithm::contains(functionname, ",");
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

void
Evaluator::pushDebug(std::string fname, std::string detail)
{
    cstack.push_back(StackEntry(fname, detail, 0));
}

void
Evaluator::popDebug()
{
    if (!cstack.empty()) {
        cstack.pop_back();
    } else {
        io->outputMessage("IDERROR\n");
    }
}

void
Evaluator::setInterface(Interface* _io)
{
    io = _io;
}

Interface*
Evaluator::getInterface()
{
    return io;
}

bool
Evaluator::lookupFunction(std::string funcName, FuncPtr& val)
{
    return context->lookupFunction(funcName, val);
}

ArrayOf
Evaluator::rhsExpressionSimple(ASTPtr t)
{
    ArrayOf r;
    ArrayOfVector m;
    bool isVar = false;
    bool isFun = false;
    FunctionDef* funcDef;
    pushID(t->context());
    // Try to satisfy the rhs expression with what functions we have already
    // loaded.
    isVar = context->lookupVariable(t->text, r);
    if (isVar && (t->down == nullptr)) {
        popID();
        return r;
    }
    if (!isVar) {
        isFun = lookupFunction(t->text, funcDef);
    }
    if (!isVar && isFun) {
        m = functionExpression(funcDef, t, 1, false);
        if (m.empty()) {
            popID();
            return ArrayOf::emptyConstructor();
        } else {
            popID();
            return m[0];
        }
    }
    if (!isVar) {
        Error(utf8_to_wstring(_("Undefined variable:") + " " + t->text));
    }
    if (!isFun) {
        Error(utf8_to_wstring(_("Undefined function:") + " " + t->text));
    }
    popID();
    return r;
}

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
Evaluator::rhsExpression(ASTPtr t)
{
    ArrayOf r, q;
    ArrayOf n, p;
    ArrayOfVector m;
    ArrayOfVector rv;
    bool isVar;
    bool isFun = false;
    Dimensions rhsDimensions;
    FunctionDef* funcDef = nullptr;
    pushID(t->context());
    // Try to satisfy the rhs expression with what functions we have already
    // loaded.
    isVar = context->lookupVariable(t->text, r);
    if (isVar) {
        if (r.isClassStruct()) {
            std::string className = r.getStructType();
            std::string extractionFunctionName = className + "_extraction";
            isFun = lookupFunction(extractionFunctionName, funcDef);
            if (isFun) {
                m = functionExpression(funcDef, t, 1, false);
                popID();
                return m;
            } else {
                Error(utf8_to_wstring(_("Undefined function:") + " " + extractionFunctionName));
            }
        } else {
            if (t->down == nullptr) {
                ArrayOfVector rv;
                rv.push_back(r);
                popID();
                return rv;
            } else {
                ASTPtr tt;
                tt = t->down;
                if (tt->opNum == OP_PARENS) {
                    if (tt->down == nullptr) {
                        if (tt->right) {
                            tt = tt->right;
                            if (tt->opNum == OP_DOT) {
                                ArrayOfVector rv;
                                rv.push_back(r.getField(tt->down->text));
                                popID();
                                return rv;
                            }
                        }
                    }
                }
            }
        }
    } else {
        isFun = lookupFunction(t->text, funcDef);
        if (isFun) {
            if (funcDef->outputArgCount() == 0) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS, utf8_to_wstring(funcDef->name));
            }
            m = functionExpression(funcDef, t, 1, false);
            popID();
            return m;
        } else {
            Error(utf8_to_wstring(_("Undefined variable or function:") + " " + t->text));
        }
    }
    t = t->down;
    while (t != nullptr) {
        rhsDimensions = r.getDimensions();
        if (!rv.empty()) {
            Error(_W("Cannot reindex an expression that returns multiple values."));
        }
        if (t->opNum == (OP_PARENS)) {
            m = expressionList(t->down, r);
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
                if (t->right == nullptr) {
                    ArrayOfVector rv;
                    rv.push_back(r);
                    popID();
                    return rv;
                } else {
                    Error(_W("index expected."));
                }
            } else if (m.size() == 1) {
                q = r.getVectorSubset(m[0]);
                r = q;
            } else {
                q = r.getNDimSubset(m);
                r = q;
            }
        }
        if (t->opNum == (OP_BRACES)) {
            m = expressionList(t->down, r);
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
                rv = r.getVectorContentsAsList(m[0]);
            } else {
                rv = r.getNDimContentsAsList(m);
            }
            if (rv.size() == 1) {
                r = rv[0];
                rv = ArrayOfVector();
            } else if (rv.size() == 0) {
                r = ArrayOf::emptyConstructor();
                Error(ERROR_EMPTY_EXPRESSION);
            }
        }
        if (t->opNum == (OP_DOT)) {
            std::string fieldname = t->down->text;
            if (r.isHandle()) {
                ArrayOfVector params;
                logical isValidMethod = false;
                try {
                    isValidMethod = r.isHandleMethod(utf8_to_wstring(fieldname));
                } catch (const Exception&) {
                    if (r.isHandle()) {
                        Error(_W("Please define: ") + r.getHandleCategory() + L"_ismethod");
                    }
                    isValidMethod = false;
                }
                if (isValidMethod) {
                    if (t->right) {
                        params = expressionList(t->right->down, r);
                        t = t->right;
                    }
                }
                rv = getHandle(r, fieldname, params);
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
        if (t->opNum == (OP_DOTDYN)) {
            std::string field;
            try {
                ArrayOf fname(expression(t->down));
                field = fname.getContentAsCString();
            } catch (const Exception&) {
                Error(_W("dynamic field reference to structure requires a string argument"));
            }
            if (r.isHandle()) {
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
        t = t->right;
    }
    if (rv.empty()) {
        rv.push_back(r);
    }
    popID();
    return rv;
}
//=============================================================================
Evaluator::Evaluator(Context* aContext, Interface* aInterface, int _engineMode)
{
    Exception e;
    lastErrorException = e;
    lastWarningException = e;
    engineMode = _engineMode;
    bAllowOverload = true;
    context = aContext;
    resetState();
    depth = 0;
    io = aInterface;
    autostop = true;
    InCLI = false;
    debugActive = false;
    inStepMode = false;
    bpActive = false;
    clearStacks();
    cstack.reserve(4096);
    commandLineArguments.clear();
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
    ASTPtr tree = nullptr;
    ParserState parserState = ParseError;
    NelsonConfiguration::getInstance()->setInterruptPending(false);
    if (line.size() == 0) {
        return false;
    }
    if (line == "\n") {
        return false;
    }
    char ch = *line.rbegin();
    std::string command;
    // we add <RETURN> at the end
    // the command need a <RETURN> to be correctly parser
    if (ch != '\n') {
        command = line + "\n";
    } else {
        command = line;
    }
    resetAstBackupPosition();
    std::vector<ASTPtr> pt;
    try {
        parserState = parseString(command);
        pt = getAstUsed();
    } catch (Exception& e) {
        deleteAstVector(getAstUsed());
        resetAstBackupPosition();
        resetParser();
        setLastErrorException(e);
        if (propogateException) {
            throw;
        }
        e.printMe(io);
        return false;
    }
    if (parserState != ScriptBlock) {
        deleteAstVector(pt);
        resetAstBackupPosition();
        resetParser();
        Exception e(_W("a valid script expected."));
        setLastErrorException(e);
        if (propogateException) {
            throw;
        }
        return false;
    }
    tree = getParsedScriptBlock();
    pushDebug("EvaluateString", command);
    if (tree == nullptr) {
        deleteAstVector(pt);
        resetAstBackupPosition();
        popDebug();
        return false;
    }
    try {
        block(tree);
        deleteAstVector(pt);
        resetAstBackupPosition();
        tree = nullptr;
        if (state == NLS_STATE_RETURN) {
            if (depth > 0) {
                popDebug();
                depth--;
                return true;
            }
        }
        if (state == NLS_STATE_QUIT || state == NLS_STATE_ABORT) {
            popDebug();
            return true;
        }
    } catch (Exception& e) {
        deleteAstVector(pt);
        resetAstBackupPosition();
        tree = nullptr;
        setLastErrorException(e);
        if (propogateException) {
            throw;
        }
        e.printMe(io);
        popDebug();
        return false;
    }
    popDebug();
    return true;
}
//=============================================================================
bool
Evaluator::setLastErrorException(const Exception& e)
{
    lastErrorException = e;
    return true;
}
//=============================================================================
Exception
Evaluator::getLastErrorException()
{
    return lastErrorException;
}
//=============================================================================
void
Evaluator::resetLastErrorException()
{
    Exception e;
    lastErrorException = e;
}
//=============================================================================
Exception
Evaluator::getLastWarningException()
{
    return lastWarningException;
}
//=============================================================================
void
Evaluator::resetLastWarningException()
{
    Exception e;
    lastWarningException = e;
}
//=============================================================================
bool
Evaluator::setLastWarningException(const Exception& e)
{
    lastWarningException = e;
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
    int ipos = (int)cstack.size() - 2;
    if (ipos >= 0) {
        return cstack[ipos].cname;
    }
    return std::string("");
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
    int ipos = (int)cstack.size() - 1;
    if (ipos >= 0) {
        std::string fullname = cstack[cstack.size() - 1].cname;
        if (boost::algorithm::ends_with(fullname, ".nlf")) {
            boost::filesystem::path pathForStem(fullname);
            return pathForStem.stem().string();
        }
        return fullname;
    }
    return std::string("");
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
bool
Evaluator::isOverloadAllowed()
{
    return bAllowOverload;
}
//=============================================================================
void
Evaluator::disableOverload()
{
    bAllowOverload = false;
}
//=============================================================================
void
Evaluator::enableOverload()
{
    bAllowOverload = true;
}
//=============================================================================
bool
Evaluator::mustOverloadBasicTypes()
{
    return overloadOnBasicTypes;
}
//=============================================================================
void
Evaluator::enableOverloadBasicTypes()
{
    overloadOnBasicTypes = true;
}
//=============================================================================
void
Evaluator::disableOverloadBasicTypes()
{
    overloadOnBasicTypes = false;
}
//=============================================================================
std::wstring
Evaluator::buildPrompt()
{
    std::wstring prompt;
    if (depth > 0) {
        if (bpActive) {
            prompt = L"-" + std::to_wstring(depth) + L"D-> ";
        } else {
            prompt = L"-" + std::to_wstring(depth) + L"-> ";
        }
    } else {
        if (bpActive) {
            prompt = L"D-> ";
        } else {
            prompt = L"--> ";
        }
    }
    return prompt;
}
//=============================================================================
void
Evaluator::evalCLI()
{
    while (1) {
        if (!bpActive) {
            // clear macros cache at the prompt
            stringVector exceptedFunctionsName = this->getCallers(true);
            PathFuncManager::getInstance()->clearCache(exceptedFunctionsName);
            FileWatcherManager::getInstance()->update();
            clearStacks();
        }
        std::wstring prompt = buildPrompt();
        std::wstring commandLine;
        commandQueue.get(commandLine);
        if (commandLine.empty()) {
            commandLine = io->getLine(prompt);
            if (commandLine.empty()) {
                InCLI = false;
                this->setState(NLS_STATE_QUIT);
                return;
            } else {
                wchar_t ch = *commandLine.rbegin();
                if (ch != L'\n') {
                    commandLine.push_back(L'\n');
                }
            }
        }
        // scan the line and tokenize it
        resetAstBackupPosition();
        setLexBuffer(commandLine);
        try {
            int lastCount = 0;
            bool bContinueLine = lexCheckForMoreInput(0);
            deleteAstVector(getAstUsed());
            resetAstBackupPosition();
            if (bContinueLine) {
                lastCount = getContinuationCount();
                std::wstring lines = commandLine;
                bool enoughInput = false;
                while (!enoughInput) {
                    commandLine = io->getLine(L"");
                    if (commandLine == L"\n" || commandLine.empty()) {
                        if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                            commandLine = L"";
                            return;
                        }
                        enoughInput = true;
                    } else {
                        lines.append(commandLine);
                        resetAstBackupPosition();
                        setLexBuffer(lines);
                        enoughInput = !lexCheckForMoreInput(lastCount);
                        deleteAstVector(getAstUsed());
                        resetAstBackupPosition();
                        lastCount = getContinuationCount();
                        if (enoughInput) {
                            lines.append(L"\n");
                        }
                    }
                }
                commandLine = lines;
            }
        } catch (Exception& e) {
            e.printMe(io);
            commandLine.clear();
        }
        InCLI = true;
        if (!commandLine.empty()) {
            size_t stackdepth = cstack.size();
            bool evalResult = evaluateString(commandLine, false);
            while (cstack.size() > stackdepth) {
                cstack.pop_back();
            }
            if (!evalResult || this->getState() == NLS_STATE_QUIT
                || this->getState() == NLS_STATE_ABORT) {
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
    return ((this->engineMode != BASIC_ENGINE) && (this->engineMode != BASIC_TERMINAL));
}
//=============================================================================
int
Evaluator::getNelsonEngineMode()
{
    return this->engineMode;
}
//=============================================================================
void
Evaluator::setCommandLineArguments(wstringVector args)
{
    this->commandLineArguments = args;
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
void
Evaluator::setHandle(ArrayOf r, std::string fieldname, ArrayOfVector fieldvalue)
{
    if (fieldvalue.size() != 1) {
        Error(_W("Right hand values must satisfy left hand side expression."));
    }
    std::wstring currentType = r.getHandleCategory();
    std::wstring ufunctionNameSetHandle = currentType + L"_set";
    std::string functionNameSetHandle = wstring_to_utf8(ufunctionNameSetHandle);
    Context* context = this->getContext();
    FunctionDef* funcDef = nullptr;
    if (!context->lookupFunction(functionNameSetHandle, funcDef)) {
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
ArrayOfVector
Evaluator::getHandle(ArrayOf r, std::string fieldname, ArrayOfVector params)
{
    ArrayOfVector argIn;
    std::wstring currentType = r.getHandleCategory();
    Context* context = this->getContext();
    FunctionDef* funcDef = nullptr;
    std::string functionNameCurrentType = wstring_to_utf8(currentType) + "_" + fieldname;
    if (context->lookupFunction(functionNameCurrentType, funcDef)) {
        if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                || (funcDef->type() == NLS_MACRO_FUNCTION))) {
            Error(_W("Type function not valid."));
        }
        int nLhs = 1;
        argIn.push_back(r);
        for (ArrayOf a : params) {
            argIn.push_back(a);
        }
        return funcDef->evaluateFunction(this, argIn, nLhs);
    }
    std::string functionNameGetHandle = wstring_to_utf8(currentType) + "_get";
    if (!context->lookupFunction(functionNameGetHandle, funcDef)) {
        Error(_W("Function not found."));
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
Evaluator::countSubExpressions(ASTPtr t)
{
    size_t count = 0;
    while (t != NULL) {
        t = t->right;
        count++;
    }
    return count;
}
//=============================================================================
ArrayOf
Evaluator::doBinaryOperatorOverload(
    ASTPtr t, BinaryFunction functionOperator, std::string functionName)
{
    ArrayOf A(expression(t->down));
    ArrayOf B(expression(t->down->right));
    return doBinaryOperatorOverload(A, B, functionOperator, functionName);
}
//=============================================================================
ArrayOf
Evaluator::doBinaryOperatorOverload(
    ArrayOf& A, ArrayOf& B, BinaryFunction functionOperator, std::string functionName)
{
    ArrayOf res;
    bool bSuccess = false;
    if (!overloadOnBasicTypes) {
        res = functionOperator(A, B, false, bSuccess);
        if (!bSuccess) {
            res = OverloadBinaryOperator(this, A, B, functionName, bSuccess);
            if (!bSuccess) {
                ArrayOfVector argsIn;
                argsIn.push_back(A);
                argsIn.push_back(B);
                OverloadRequired(this, argsIn, Overload::OverloadClass::BINARY, functionName);
            }
        }
    } else {
        res = OverloadBinaryOperator(this, A, B, functionName, bSuccess);
        if (!bSuccess) {
            res = functionOperator(A, B, true, bSuccess);
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
