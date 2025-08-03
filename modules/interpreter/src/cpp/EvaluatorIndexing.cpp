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
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "Operators.hpp"
#include "characters_encoding.hpp"
#include "OverloadName.hpp"
#include "ClassName.hpp"
#include "Warning.hpp"
//=============================================================================
namespace Nelson {
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
            if (r.isGraphicsObject()) {
                ArrayOfVector params;
                ArrayOfVector rv = getHandle(r, t->down->text, params);
                if (rv.size() > 0) {
                    return rv[0];
                } else {
                    return (ArrayOf::emptyConstructor());
                }
            }
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
            substype.push_back("()");
            ArrayOfVector subsindices;
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
                if (t->right->down && !t->right->down->text.empty()) {
                    subsindices.push_back(ArrayOf::characterArrayConstructor(t->right->down->text));
                }
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
indexType
Evaluator::countLeftHandSides(AbstractSyntaxTreePtr t)
{
    ArrayOf lhs;
    if (t == nullptr) {
        Error(_W("Syntax error."));
    }
    if (!context->lookupVariable(t->text, lhs)) {
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
ArrayOfVector
Evaluator::extractClass(const ArrayOf& r, const stringVector& subtypes,
    const ArrayOfVector& subsindices, bool& haveFunction)
{
    haveFunction = false;
    std::string currentClass = ClassName(r);
    Context* _context = this->getContext();
    FunctionDef* funcDef = nullptr;
    std::string functionNameSimpleExtractClass
        = getOverloadFunctionName(currentClass, SUBSREF_OPERATOR_STR);
    if (_context->lookupFunction(functionNameSimpleExtractClass, funcDef)) {
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
        rv[0].name("");
        callstack = backupCallStack;
        return rv;
    }
    return {};
}
//=============================================================================
ArrayOf
Evaluator::EndReference(const ArrayOf& v, indexType index, size_t count)
{
    Dimensions dim(v.getDimensions());
    ArrayOf res;
    bool useStandardMethod = true;
    if (v.isClassType()) {
        bool needToBeOverloaded = false;
        res = EndOverloadReference(v, index, count, needToBeOverloaded);
        useStandardMethod = needToBeOverloaded;
        if (useStandardMethod) {
            Warning(L"Nelson:End:Overloading:missing", _W("numel and size need to be overloaded."));
        }
    }
    if (useStandardMethod) {
        if (count == 1) {
            return ArrayOf::doubleConstructor(static_cast<double>(dim.getElementCount()));
        }
        return ArrayOf::doubleConstructor(static_cast<double>(dim.getDimensionLength(index)));
    }
    return res;
}
//=============================================================================
ArrayOf
Evaluator::EndOverloadReference(
    const ArrayOf& v, indexType index, size_t count, bool& needToBeOverloaded)
{
    std::string currentClass;
    ClassName(v, currentClass);
    std::string functionNameEndClass = getOverloadFunctionName(currentClass, "end");
    Context* _context = this->getContext();
    FunctionDef* funcDef = nullptr;
    _context->lookupFunction(functionNameEndClass, funcDef);
    if (funcDef) {
        ArrayOfVector input;
        input << v;
        input << ArrayOf::doubleConstructor((double)index + 1);
        input << ArrayOf::doubleConstructor((double)count);
        int nLhs = 1;
        CallStack backupCallStack = callstack;
        ArrayOfVector res = funcDef->evaluateFunction(this, input, nLhs);
        callstack = backupCallStack;
        if (res.size() == 1) {
            needToBeOverloaded = false;
            return res[0];
        }
    }
    needToBeOverloaded = true;
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
