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
ArrayOfVector
Evaluator::bytecodeGetHandle(ArrayOf r, const std::string& fieldname, const ArrayOfVector& params)
{
    return getHandle(r, fieldname, params);
}
//=============================================================================
ArrayOfVector
Evaluator::bytecodeGetOrInvokeHandle(
    const ArrayOf& r, const std::string& fieldname, const ArrayOfVector& params)
{
    if (isObjectMethod(r, fieldname)) {
        return invokeMethod(r, fieldname, params);
    }
    return getHandle(r, fieldname, params);
}
//=============================================================================
bool
Evaluator::bytecodeInvokeObjectMethodIfExists(const ArrayOf& r, const std::string& methodName,
    const ArrayOfVector& params, int nLhs, ArrayOfVector& result)
{
    if (!isObjectMethod(r, methodName)) {
        return false;
    }
    Context* _context = this->getContext();
    FunctionDef* funcDef = nullptr;
    std::string currentType = r.isHandle() ? r.getHandleCategory()
        : r.isClassType()                  ? r.getClassType()
                                           : "";
    if (currentType.empty()) {
        return false;
    }
    std::string functionNameCurrentType = getOverloadFunctionName(currentType, "invoke");
    if (!_context->lookupFunction(functionNameCurrentType, funcDef)) {
        return false;
    }
    if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION))) {
        Error(_W("Type function not valid."));
    }
    ArrayOfVector argIn;
    argIn.push_back(r);
    argIn.push_back(ArrayOf::characterArrayConstructor(methodName));
    for (const ArrayOf& a : params) {
        argIn.push_back(a);
    }
    result = funcDef->evaluateFunction(this, argIn, nLhs);
    return true;
}
//=============================================================================
void
Evaluator::bytecodeSetHandle(
    ArrayOf r, const std::string& fieldname, const ArrayOfVector& fieldvalue)
{
    setHandle(r, fieldname, fieldvalue);
}
//=============================================================================
ArrayOfVector
Evaluator::bytecodeExtractClass(const ArrayOf& r, const stringVector& subtypes,
    const ArrayOfVector& subsindices, bool& haveFunction)
{
    return extractClass(r, subtypes, subsindices, haveFunction);
}
//=============================================================================
ArrayOf
Evaluator::bytecodeAssignClass(const ArrayOf& r, const stringVector& subtypes,
    const ArrayOfVector& subsindices, const ArrayOf& value, bool& haveFunction)
{
    haveFunction = false;
    std::string currentClass = ClassName(r);
    FunctionDef* funcDef = nullptr;
    std::string functionName = getOverloadFunctionName(currentClass, SUBSASGN_OPERATOR_STR);
    if (!getContext()->lookupFunction(functionName, funcDef)) {
        functionName
            = getOverloadFunctionName(std::string(NLS_CLASS_ARRAY_STR), SUBSASGN_OPERATOR_STR);
        if (!getContext()->lookupFunction(functionName, funcDef)) {
            return {};
        }
    }
    haveFunction = true;
    if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION))) {
        Error(_W("Type function not valid."));
    }

    stringVector fieldnames = { "type", "subs" };
    Dimensions dims(1, subtypes.size());
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames, false));
    ArrayOf substruct = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);

    ArrayOfVector typesVector;
    ArrayOfVector subsVector;
    typesVector.reserve(subtypes.size());
    subsVector.reserve(subsindices.size());
    for (const auto& t : subtypes) {
        typesVector.push_back(ArrayOf::characterArrayConstructor(t));
    }
    for (const auto& t : subsindices) {
        subsVector.push_back(t);
    }
    substruct.setFieldAsList("type", typesVector);
    substruct.setFieldAsList("subs", subsVector);

    ArrayOfVector args;
    args.push_back(r);
    args.push_back(substruct);
    args.push_back(value);

    CallStack backupCallStack = callstack;
    ArrayOfVector rv = funcDef->evaluateFunction(this, args, 1);
    callstack = backupCallStack;
    return rv.empty() ? ArrayOf::emptyConstructor() : rv[0];
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
Evaluator::bytecodeEndReference(const ArrayOf& v, indexType index, size_t count)
{
    return EndReference(v, index, count);
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
