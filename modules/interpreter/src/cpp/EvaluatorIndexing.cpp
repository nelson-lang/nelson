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
#include "ClassdefHandleObject.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefPropertyAccess.hpp"
#include "ClassdefMixinNames.hpp"
#include "ContainersMapFastPaths.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "FunctionsInMemory.hpp"
#include "Operators.hpp"
#include "characters_encoding.hpp"
#include "OverloadName.hpp"
#include "ClassName.hpp"
#include "ClassToString.hpp"
#include "StringHelpers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define EVALUATOR_INDEXING_FIELD_SUBS "subs"
#define EVALUATOR_INDEXING_FIELD_TYPE "type"
#define EVALUATOR_INDEXING_FUNCTION_BRACE_ASSIGN "braceAssign"
#define EVALUATOR_INDEXING_FUNCTION_BRACE_REFERENCE "braceReference"
#define EVALUATOR_INDEXING_FUNCTION_DELETE "delete"
#define EVALUATOR_INDEXING_FUNCTION_INVOKE "invoke"
#define EVALUATOR_INDEXING_FUNCTION_ISVALID "isvalid"
#define EVALUATOR_INDEXING_FUNCTION_DOT_ASSIGN "dotAssign"
#define EVALUATOR_INDEXING_FUNCTION_DOT_REFERENCE "dotReference"
#define EVALUATOR_INDEXING_FUNCTION_PAREN_ASSIGN "parenAssign"
#define EVALUATOR_INDEXING_FUNCTION_PAREN_REFERENCE "parenReference"
//=============================================================================
namespace {
    //=============================================================================
    // A class counts as user-defined when the file backing its subsref method lives
    // outside the Nelson installation tree. Native Nelson classes ship their methods
    // under the Nelson root; user classes are loaded from an arbitrary path.
    bool
    classSubsrefIsUserDefined(const std::wstring& subsrefFile)
    {
        if (subsrefFile.empty()) {
            return false;
        }
        const std::wstring nelsonRoot
            = NelsonConfiguration::getInstance()->getNelsonRootDirectory();
        if (nelsonRoot.empty()) {
            return false;
        }
        auto normalizePath = [](std::wstring path) {
            for (wchar_t& c : path) {
                if (c == L'\\') {
                    c = L'/';
                } else if (c >= L'A' && c <= L'Z') {
                    c = static_cast<wchar_t>(c - L'A' + L'a');
                }
            }
            return path;
        };
        const std::wstring file = normalizePath(subsrefFile);
        const std::wstring root = normalizePath(nelsonRoot);
        return file.compare(0, root.size(), root) != 0;
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Evaluator::bytecodeGetHandle(ArrayOf r, const std::string& fieldname, const ArrayOfVector& params)
{
    return getHandle(r, fieldname, params);
}
//=============================================================================
bool
Evaluator::bytecodeInvokeObjectMethodIfExists(const ArrayOf& r, const std::string& methodName,
    const ArrayOfVector& params, int nLhs, ArrayOfVector& result)
{
    // isKey(M, key) and M.isKey(key) on a char-key map in native storage
    if (nLhs <= 1 && params.size() == 1 && methodName == "isKey"
        && tryContainersMapIsKey(r, params[0], result)) {
        return true;
    }
    Context* _context = this->getContext();
    FunctionDef* funcDef = nullptr;
    std::string currentType = r.isHandle() ? r.handleCategory()
        : r.isClassType()                  ? r.classType()
                                           : "";
    if (currentType.empty()) {
        return false;
    }
    std::string directMethod = getOverloadFunctionName(currentType, methodName);
    bool canUseDirectMethod = true;
    if (r.isClassType() || r.isHandle()) {
        if (classdefLoadClass(currentType)) {
            // obj.staticMethod(args): a Static method reached through an instance is called without
            // the object as first argument. This runs before the instance-method resolution, which
            // would otherwise pick up a static method implemented by an external file.
            std::string staticMethod;
            FunctionDef* staticFuncDef = nullptr;
            if (classdefResolveStaticMethodFunction(
                    currentType, methodName, staticMethod, getClassdefAccessContext())
                && _context->lookupFunction(staticMethod, staticFuncDef) && staticFuncDef != nullptr
                && ((staticFuncDef->type() == NLS_BUILT_IN_FUNCTION)
                    || (staticFuncDef->type() == NLS_MACRO_FUNCTION))) {
                int requestedOutputs
                    = (nLhs == 1 && staticFuncDef->outputArgCount() == 0) ? 0 : nLhs;
                result = staticFuncDef->evaluateFunction(this, params, requestedOutputs);
                return true;
            }
            canUseDirectMethod = classdefResolveMethodFunction(
                currentType, methodName, directMethod, getClassdefAccessContext());
            if (!canUseDirectMethod && classdefHasMethodDefinition(currentType, methodName)) {
                Error("Nelson:interpreter:noSuchProperty", "No such property: %s", methodName);
            }
        }
    }
    if (canUseDirectMethod && lookupOverloadFunctionCached(_context, directMethod, funcDef)) {
        if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                || (funcDef->type() == NLS_MACRO_FUNCTION))) {
            Error(L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
        }
        ArrayOfVector argIn;
        argIn.reserve(params.size() + 1);
        argIn.push_back(r);
        for (const ArrayOf& a : params) {
            argIn.push_back(a);
        }
        int requestedOutputs = nLhs == 1 && funcDef->outputArgCount() == 0 ? 0 : nLhs;
        result = funcDef->evaluateFunction(this, argIn, requestedOutputs);
        return true;
    }
    if (!isObjectMethod(r, methodName)) {
        return false;
    }
    directMethod = getOverloadFunctionName(currentType, methodName);
    canUseDirectMethod = true;
    if (r.isClassType() || r.isHandle()) {
        if (classdefLoadClass(currentType)) {
            canUseDirectMethod = classdefResolveMethodFunction(
                currentType, methodName, directMethod, getClassdefAccessContext());
            if (!canUseDirectMethod && classdefHasMethodDefinition(currentType, methodName)) {
                Error("Nelson:interpreter:noSuchProperty", "No such property: %s", methodName);
            }
        }
    }
    if (canUseDirectMethod && lookupOverloadFunctionCached(_context, directMethod, funcDef)) {
        if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                || (funcDef->type() == NLS_MACRO_FUNCTION))) {
            Error(L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
        }
        ArrayOfVector argIn;
        argIn.reserve(params.size() + 1);
        argIn.push_back(r);
        for (const ArrayOf& a : params) {
            argIn.push_back(a);
        }
        int requestedOutputs = nLhs == 1 && funcDef->outputArgCount() == 0 ? 0 : nLhs;
        result = funcDef->evaluateFunction(this, argIn, requestedOutputs);
        return true;
    }
    std::string functionNameCurrentType
        = getOverloadFunctionName(currentType, EVALUATOR_INDEXING_FUNCTION_INVOKE);
    if (!lookupOverloadFunctionCached(_context, functionNameCurrentType, funcDef)) {
        return false;
    }
    if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION))) {
        Error(L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
    }
    ArrayOfVector argIn;
    argIn.push_back(r);
    argIn.push_back(ArrayOf::characterArray(methodName));
    for (const ArrayOf& a : params) {
        argIn.push_back(a);
    }
    int requestedOutputs = nLhs == 1 && funcDef->outputArgCount() == 0 ? 0 : nLhs;
    result = funcDef->evaluateFunction(this, argIn, requestedOutputs);
    return true;
}
//=============================================================================
bool
Evaluator::bytecodeInvokeObjectMethodIfExistsWithInputs(const ArrayOf& r,
    const std::string& methodName, const ArrayOfVector& argIn, int nLhs, ArrayOfVector& result)
{
    Context* _context = this->getContext();
    FunctionDef* funcDef = nullptr;
    std::string currentType = r.isHandle() ? r.handleCategory()
        : r.isClassType()                  ? r.classType()
                                           : "";
    if (currentType.empty()) {
        return false;
    }
    std::string directMethod = getOverloadFunctionName(currentType, methodName);
    bool canUseDirectMethod = true;
    if (r.isClassType() || r.isHandle()) {
        if (classdefLoadClass(currentType)) {
            canUseDirectMethod = classdefResolveMethodFunction(
                currentType, methodName, directMethod, getClassdefAccessContext());
            if (!canUseDirectMethod && classdefHasMethodDefinition(currentType, methodName)) {
                Error("Nelson:interpreter:noSuchProperty", "No such property: %s", methodName);
            }
        }
    }
    if (!canUseDirectMethod || !lookupOverloadFunctionCached(_context, directMethod, funcDef)) {
        return false;
    }
    if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION))) {
        Error(L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
    }
    int requestedOutputs = nLhs == 1 && funcDef->outputArgCount() == 0 ? 0 : nLhs;
    result = funcDef->evaluateFunction(this, argIn, requestedOutputs);
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
    const ArrayOfVector& subsindices, bool& haveFunction, int nLhs)
{
    return extractClass(r, subtypes, subsindices, haveFunction, nLhs);
}
//=============================================================================
ArrayOf
Evaluator::bytecodeAssignClass(const ArrayOf& r, const stringVector& subtypes,
    const ArrayOfVector& subsindices, const ArrayOf& value, bool& haveFunction)
{
    haveFunction = false;
    if (r.isHandle() && subtypes.size() == 1 && subtypes.front() == "()"
        && r.handleCategory() == NLS_HANDLE_CONTAINERS_MAP_CATEGORY_STR) {
        // hot path: keyed write on a char-key map backed by native storage
        std::string key;
        if (containersMapCharKey(subsindices, key)) {
            ClassdefHandleObject* object = nullptr;
            ContainersMapObject* nativeMap = containersMapNativeStorage(r, &object);
            // only a char-key map stores a char key directly; numeric-key maps
            // also use native storage but must validate the key type first
            if (nativeMap != nullptr && object != nullptr
                && object->getProperty("KeyTypeValue").asUtf8String() == "char") {
                const std::string valueType = object->getProperty("ValueTypeValue").asUtf8String();
                if (valueType == "any" || ClassName(value) == valueType) {
                    nativeMap->set(key, value);
                    haveFunction = true;
                    return r;
                }
            }
        }
    }
    // Within a classic (struct-based) class's own method, obj.field = value writes
    // the object's data field directly rather than through its overloaded subsasgn.
    if (r.isClassType() && r.dataClass() == NLS_CLASS_ARRAY && subtypes.size() == 1
        && subtypes.front() == "." && subsindices.size() == 1
        && subsindices.front().isRowVectorCharacterArray()
        && getClassdefAccessContext() == r.classType()) {
        haveFunction = true;
        ArrayOf result = r;
        ArrayOfVector fieldValue;
        fieldValue.push_back(value);
        result.setFieldAsList(subsindices.front().asUtf8String(), fieldValue);
        return result;
    }
    // Within a user-defined class's own method, obj(i) on an object of that class uses the built-in
    // structural assignment, not the overloaded subsasgn, like the read side. This also covers the
    // write-back of a decomposed obj(i).field = val chain. Native Nelson classes drive structural
    // indexing through their own subsasgn and are excluded by the user-defined test.
    if ((r.isClassType() || r.isHandle()) && subtypes.size() == 1 && subtypes.front() == "()") {
        const std::string ownClass = r.isHandle() ? r.handleCategory() : r.classType();
        if (!ownClass.empty() && getClassdefAccessContext() == ownClass
            && !classdefHasMethodDefinition(
                ownClass, EVALUATOR_INDEXING_FUNCTION_PAREN_REFERENCE)) {
            std::string resolvedSubsasgn;
            FunctionDef* subsasgnDef = nullptr;
            if (classdefResolveMethodFunction(
                    ownClass, SUBSASGN_OPERATOR_STR, resolvedSubsasgn, getClassdefAccessContext())
                && getContext()->lookupFunction(resolvedSubsasgn, subsasgnDef)
                && subsasgnDef != nullptr && subsasgnDef->type() == NLS_MACRO_FUNCTION
                && classSubsrefIsUserDefined(subsasgnDef->getFilename())) {
                return {};
            }
        }
    }
    std::string currentClass = r.isGraphicsObject() ? ClassToString(r.dataClass())
        : r.isHandle()                              ? r.handleCategory()
                                                    : ClassName(r);
    bool isClassdefObject = (r.isClassType() || r.isHandle()) && classdefLoadClass(currentClass);
    FunctionDef* funcDef = nullptr;
    std::string functionName = getOverloadFunctionName(currentClass, SUBSASGN_OPERATOR_STR);
    if (r.isHandle() && !isClassdefObject) {
        // Native handle objects (e.g. gpuArray) are not classdef classes but may still provide an
        // overloaded subsasgn. Dispatch to it when present, otherwise use the default assignment.
        FunctionDef* handleSubsasgn = nullptr;
        if (!lookupOverloadFunctionCached(getContext(), functionName, handleSubsasgn)) {
            return {};
        }
    }
    bool useClassSpecificSubsasgn = true;
    if (isClassdefObject) {
        // builtin('subsasgn', obj, S, val) clears withOverload so the assignment uses the default
        // class-array subsasgn. A subsasgn that falls back to builtin('subsasgn', ...) relies on
        // this to avoid infinite recursion.
        useClassSpecificSubsasgn
            = withOverload && classdefHasMethod(currentClass, SUBSASGN_OPERATOR_STR);
        if (useClassSpecificSubsasgn) {
            // subsasgn may be inherited from a superclass: resolve the implementing overload (e.g.
            // base_subsasgn) along the inheritance chain. The exact lookup below misses the
            // inherited method and would use the generic class-array subsasgn.
            std::string resolvedSubsasgn;
            if (classdefResolveMethodFunction(currentClass, SUBSASGN_OPERATOR_STR, resolvedSubsasgn,
                    getClassdefAccessContext())) {
                functionName = resolvedSubsasgn;
            }
        }
        if (subtypes.size() == 1 && subtypes.front() == "." && subsindices.size() == 1
            && subsindices.front().isRowVectorCharacterArray()) {
            std::string propertyName = subsindices.front().asUtf8String();
            if (isDirectPropertyAccess(currentClass, propertyName, useClassSpecificSubsasgn)) {
                if (classdefIsBuiltinClass(currentClass)
                    && !classdefCanSetProperty(
                        currentClass, propertyName, getClassdefAccessContext(), true, this)) {
                    Error(L"Nelson:interpreter:readOnlyProperty",
                        L"Cannot set read-only property of %s.", utf8_to_wstring(currentClass));
                }
                useClassSpecificSubsasgn = false;
                const ClassdefPropertyDefinition* plainProperty = r.isClassType()
                    ? classdefPlainStoredProperty(currentClass, propertyName, true)
                    : nullptr;
                if (plainProperty != nullptr) {
                    // plain stored property: assign directly, the generic subsasgn
                    // builtin would redo these checks and copy every stored value
                    haveFunction = true;
                    ArrayOf res = r;
                    ArrayOfVector propertyValue;
                    if (plainProperty->validationExpression.empty()) {
                        propertyValue << value;
                    } else {
                        propertyValue << classdefValidatePropertyValue(this, *plainProperty, value);
                    }
                    res.setFieldAsList(propertyName, propertyValue);
                    return res;
                }
            } else if (!classdefHasMethodDefinition(currentClass, propertyName)
                && classdefHasMethodDefinition(
                    currentClass, EVALUATOR_INDEXING_FUNCTION_DOT_ASSIGN)) {
                const std::string dotAsgnMethod
                    = classdefIsA(currentClass, NLS_MIXIN_INDEXING_REDEFINES_DOT)
                    ? std::string("__nls_ridx_dot_asgn__")
                    : std::string(EVALUATOR_INDEXING_FUNCTION_DOT_ASSIGN);
                if (classdefResolveMethodFunction(
                        currentClass, dotAsgnMethod, functionName, getClassdefAccessContext())
                    && lookupOverloadFunctionCached(getContext(), functionName, funcDef)) {
                    haveFunction = true;
                    if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                            || (funcDef->type() == NLS_MACRO_FUNCTION))) {
                        Error(
                            L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
                    }
                    ArrayOfVector args;
                    args.push_back(r);
                    args.push_back(ArrayOf::characterArray(propertyName));
                    args.push_back(value);
                    CallStack backupCallStack = callstack;
                    int requestedOutputs = r.isHandle() && funcDef->outputArgCount() == 0 ? 0 : 1;
                    ArrayOfVector rv = funcDef->evaluateFunction(this, args, requestedOutputs);
                    callstack = backupCallStack;
                    return rv.empty() ? r : rv[0];
                }
            }
        }
        if (!useClassSpecificSubsasgn && subtypes.size() == 1 && subtypes.front() == "()"
            && subsindices.size() == 1
            && classdefHasMethodDefinition(
                currentClass, EVALUATOR_INDEXING_FUNCTION_PAREN_ASSIGN)) {
            // RedefinesParen declares parenAssign protected; route through the public
            // dispatcher which builds the IndexingOperation and calls the protected hook.
            const std::string parenAsgnMethod
                = classdefIsA(currentClass, NLS_MIXIN_INDEXING_REDEFINES_PAREN)
                ? std::string("__nls_ridx_paren_asgn__")
                : std::string(EVALUATOR_INDEXING_FUNCTION_PAREN_ASSIGN);
            if (classdefResolveMethodFunction(
                    currentClass, parenAsgnMethod, functionName, getClassdefAccessContext())
                && lookupOverloadFunctionCached(getContext(), functionName, funcDef)) {
                haveFunction = true;
                if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                        || (funcDef->type() == NLS_MACRO_FUNCTION))) {
                    Error(L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
                }
                ArrayOfVector args;
                args.push_back(r);
                args.push_back(subsindices.front());
                args.push_back(value);
                CallStack backupCallStack = callstack;
                int requestedOutputs = r.isHandle() && funcDef->outputArgCount() == 0 ? 0 : 1;
                ArrayOfVector rv = funcDef->evaluateFunction(this, args, requestedOutputs);
                callstack = backupCallStack;
                return rv.empty() ? r : rv[0];
            }
        }
        if (!useClassSpecificSubsasgn && subtypes.size() == 1 && subtypes.front() == "{}"
            && subsindices.size() == 1
            && classdefHasMethodDefinition(
                currentClass, EVALUATOR_INDEXING_FUNCTION_BRACE_ASSIGN)) {
            const std::string braceAsgnMethod
                = classdefIsA(currentClass, NLS_MIXIN_INDEXING_REDEFINES_BRACE)
                ? std::string("__nls_ridx_brace_asgn__")
                : std::string(EVALUATOR_INDEXING_FUNCTION_BRACE_ASSIGN);
            if (classdefResolveMethodFunction(
                    currentClass, braceAsgnMethod, functionName, getClassdefAccessContext())
                && lookupOverloadFunctionCached(getContext(), functionName, funcDef)) {
                haveFunction = true;
                if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                        || (funcDef->type() == NLS_MACRO_FUNCTION))) {
                    Error(L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
                }
                ArrayOfVector args;
                args.push_back(r);
                args.push_back(subsindices.front());
                args.push_back(value);
                CallStack backupCallStack = callstack;
                int requestedOutputs = r.isHandle() && funcDef->outputArgCount() == 0 ? 0 : 1;
                ArrayOfVector rv = funcDef->evaluateFunction(this, args, requestedOutputs);
                callstack = backupCallStack;
                return rv.empty() ? r : rv[0];
            }
        }
    }
    if (!useClassSpecificSubsasgn && isClassdefObject && r.isHandle() && subtypes.size() == 1
        && subtypes.front() == "." && subsindices.size() == 1
        && subsindices.front().isRowVectorCharacterArray()) {
        // default h.name = value (builtin('subsasgn', ...)): the handle property write
        setHandle(r, subsindices.front().asUtf8String(), { value });
        haveFunction = true;
        return r;
    }
    if (!useClassSpecificSubsasgn
        || !lookupOverloadFunctionCached(getContext(), functionName, funcDef)) {
        if (isClassdefObject && !subtypes.empty() && subtypes.front() == "()") {
            if (classdefIsA(currentClass, NLS_MIXIN_SCALAR)) {
                RaiseError("Indexed assignment with parentheses is not supported for '"
                        + currentClass + "' objects.",
                    "Nelson:class:parenAssignScalar");
            }
            return {};
        }
        functionName
            = getOverloadFunctionName(std::string(NLS_CLASS_ARRAY_STR), SUBSASGN_OPERATOR_STR);
        if (!lookupOverloadFunctionCached(getContext(), functionName, funcDef)) {
            return {};
        }
    }
    haveFunction = true;
    if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION))) {
        Error(L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
    }

    stringVector fieldnames = { EVALUATOR_INDEXING_FIELD_TYPE, EVALUATOR_INDEXING_FIELD_SUBS };
    Dimensions dims(1, subtypes.size());
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), fieldnames, false));
    ArrayOf substruct = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);

    ArrayOfVector typesVector;
    ArrayOfVector subsVector;
    typesVector.reserve(subtypes.size());
    subsVector.reserve(subsindices.size());
    for (const auto& t : subtypes) {
        typesVector.push_back(ArrayOf::characterArray(t));
    }
    for (const auto& t : subsindices) {
        subsVector.push_back(t);
    }
    substruct.setFieldAsList(EVALUATOR_INDEXING_FIELD_TYPE, typesVector);
    substruct.setFieldAsList(EVALUATOR_INDEXING_FIELD_SUBS, subsVector);

    ArrayOfVector args;
    args.push_back(r);
    args.push_back(substruct);
    args.push_back(value);

    CallStack backupCallStack = callstack;
    ArrayOfVector rv = funcDef->evaluateFunction(this, args, 1);
    callstack = backupCallStack;
    return rv.empty() ? ArrayOf::empty() : rv[0];
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
// interchangeable.  As the name implies, @|cell|-indexing applies
// only to @|cell| arrays.  For those familiar with @|C|, cell-
// indexing is equivalent to pointer dereferencing in @|C|.  First,
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
// x.yellow = 105
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
    const ArrayOfVector& subsindices, bool& haveFunction, int nLhs)
{
    haveFunction = false;
    // Inside a classic (struct-based) class's own method, a dot field access reads the object's
    // data field directly, not through the overloaded subsref.
    if (r.isClassType() && r.dataClass() == NLS_CLASS_ARRAY && subtypes.size() == 1
        && subtypes.front() == "." && subsindices.size() == 1
        && subsindices.front().isRowVectorCharacterArray()
        && getClassdefAccessContext() == r.classType()) {
        const std::string field = subsindices.front().asUtf8String();
        for (const std::string& existing : r.fieldNames()) {
            if (existing == field) {
                haveFunction = true;
                ArrayOfVector fieldValues = r.fieldAsList(field);
                return fieldValues.empty() ? ArrayOfVector { ArrayOf::empty() }
                                           : ArrayOfVector { fieldValues[0] };
            }
        }
    }
    // Inside a class's own subsref/subsasgn, parenthesis indexing on an object of
    // that class uses the built-in subset instead of re-entering the overloaded
    // subsref, so f(1) inside subsref does not recurse.
    if ((r.isClassType() || r.isHandle()) && subtypes.size() == 1 && subtypes.front() == "()") {
        const std::string ownClass = r.isHandle() ? r.handleCategory() : r.classType();
        if (!ownClass.empty() && getClassdefAccessContext() == ownClass) {
            const std::string currentFunction = getCurrentFunctionName();
            if (currentFunction == SUBSREF_OPERATOR_STR
                || currentFunction == SUBSASGN_OPERATOR_STR) {
                return {};
            }
            // User-defined classes use the built-in object-array subset for obj(i) in every method,
            // so a method such as feval doing f(1) reads the first element instead of recursing. A
            // class is user-defined when its subsref is a macro outside the Nelson installation
            // tree. Native classes (table, datetime, ...) implement structural indexing through
            // their overloaded subsref and keep it inside their own methods.
            if (!classdefHasMethodDefinition(
                    ownClass, EVALUATOR_INDEXING_FUNCTION_PAREN_REFERENCE)) {
                std::string resolvedSubsref;
                FunctionDef* subsrefDef = nullptr;
                if (classdefResolveMethodFunction(
                        ownClass, SUBSREF_OPERATOR_STR, resolvedSubsref, getClassdefAccessContext())
                    && getContext()->lookupFunction(resolvedSubsref, subsrefDef)
                    && subsrefDef != nullptr && subsrefDef->type() == NLS_MACRO_FUNCTION
                    && classSubsrefIsUserDefined(subsrefDef->getFilename())) {
                    return {};
                }
            }
        }
    }
    if (r.isHandle() && subtypes.size() == 1 && subtypes.front() == "()" && nLhs <= 1
        && r.handleCategory() == NLS_HANDLE_CONTAINERS_MAP_CATEGORY_STR) {
        // hot path: keyed read on a char-key map backed by native storage
        std::string key;
        if (containersMapCharKey(subsindices, key)) {
            ClassdefHandleObject* object = nullptr;
            ContainersMapObject* nativeMap = containersMapNativeStorage(r, &object);
            // char-key maps only; a numeric-key map rejects a char key upstream
            if (nativeMap != nullptr && object != nullptr
                && object->getProperty("KeyTypeValue").asUtf8String() == "char") {
                ArrayOf value;
                if (!nativeMap->lookup(key, value)) {
                    Error(L"Nelson:Containers:Map:NoKey",
                        L"The specified key is not present in this container.");
                }
                haveFunction = true;
                ArrayOfVector result;
                result.push_back(value);
                return result;
            }
        }
    }
    if (r.isClassType() && subtypes.size() == 1 && subtypes.front() == "()"
        && classdefLoadClass(r.classType())
        && !classdefHasMethod(r.classType(), SUBSREF_OPERATOR_STR)
        && !classdefHasMethodDefinition(
            r.classType(), EVALUATOR_INDEXING_FUNCTION_PAREN_REFERENCE)) {
        // no paren-read hooks: skip the overload-name construction and lookups,
        // the caller applies its direct subset fallback
        return {};
    }
    std::string currentClass = r.isGraphicsObject() ? ClassToString(r.dataClass())
        : r.isHandle()                              ? r.handleCategory()
                                                    : ClassName(r);
    Context* _context = this->getContext();
    FunctionDef* funcDef = nullptr;
    std::string functionNameSimpleExtractClass
        = getOverloadFunctionName(currentClass, SUBSREF_OPERATOR_STR);
    if ((r.isClassType() || r.isHandle()) && classdefLoadClass(currentClass)) {
        // builtin('subsref', obj, S) clears withOverload: it performs default structural indexing
        // and must not re-enter the overloaded subsref, which would recurse when a class uses
        // builtin('subsref', ...) as its fallback. Forcing useClassSpecificSubsref off routes
        // dot/paren/brace access to the default path below.
        bool useClassSpecificSubsref
            = withOverload && classdefHasMethod(currentClass, SUBSREF_OPERATOR_STR);
        if (useClassSpecificSubsref) {
            // subsref may be inherited from a superclass: resolve the implementing overload (e.g.
            // base_subsref) along the inheritance chain. The gate above already walks the chain, so
            // without this the generic dispatch would look up a missing name and fall back to
            // default property lookup.
            std::string resolvedSubsref;
            if (classdefResolveMethodFunction(currentClass, SUBSREF_OPERATOR_STR, resolvedSubsref,
                    getClassdefAccessContext())) {
                functionNameSimpleExtractClass = resolvedSubsref;
            }
        }
        if (subtypes.size() == 1 && subtypes.front() == "." && subsindices.size() == 1
            && subsindices.front().isRowVectorCharacterArray()) {
            std::string propertyName = subsindices.front().asUtf8String();
            if (isDirectPropertyAccess(currentClass, propertyName, useClassSpecificSubsref)) {
                return {};
            }
            if (!classdefHasMethodDefinition(currentClass, propertyName)
                && classdefHasMethodDefinition(
                    currentClass, EVALUATOR_INDEXING_FUNCTION_DOT_REFERENCE)) {
                // RedefinesDot declares dotReference protected; route via the public
                // dispatcher (builds the IndexingOperation, calls the protected hook).
                const std::string dotRefMethod
                    = classdefIsA(currentClass, NLS_MIXIN_INDEXING_REDEFINES_DOT)
                    ? std::string("__nls_ridx_dot_ref__")
                    : std::string(EVALUATOR_INDEXING_FUNCTION_DOT_REFERENCE);
                if (classdefResolveMethodFunction(currentClass, dotRefMethod,
                        functionNameSimpleExtractClass, getClassdefAccessContext())
                    && lookupOverloadFunctionCached(
                        _context, functionNameSimpleExtractClass, funcDef)) {
                    haveFunction = true;
                    if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                            || (funcDef->type() == NLS_MACRO_FUNCTION))) {
                        Error(
                            L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
                    }
                    ArrayOfVector argIn;
                    argIn.push_back(r);
                    argIn.push_back(ArrayOf::characterArray(propertyName));
                    CallStack backupCallStack = callstack;
                    ArrayOfVector rv = funcDef->evaluateFunction(this, argIn, nLhs);
                    if (!rv.empty()) {
                        rv[0].name("");
                    }
                    callstack = backupCallStack;
                    return rv;
                }
            }
        }
        if (!useClassSpecificSubsref && subtypes.size() == 1 && subtypes.front() == "()"
            && subsindices.size() == 1
            && classdefHasMethodDefinition(
                currentClass, EVALUATOR_INDEXING_FUNCTION_PAREN_REFERENCE)) {
            // RedefinesParen subclasses declare parenReference protected: route through the public
            // dispatcher, which builds a nelson.indexing.IndexingOperation and calls the protected
            // hook. classdefIsA runs only on this slow path (the class already defines
            // parenReference).
            const std::string parenRefMethod
                = classdefIsA(currentClass, NLS_MIXIN_INDEXING_REDEFINES_PAREN)
                ? std::string("__nls_ridx_paren_ref__")
                : std::string(EVALUATOR_INDEXING_FUNCTION_PAREN_REFERENCE);
            if (classdefResolveMethodFunction(currentClass, parenRefMethod,
                    functionNameSimpleExtractClass, getClassdefAccessContext())
                && lookupOverloadFunctionCached(
                    _context, functionNameSimpleExtractClass, funcDef)) {
                haveFunction = true;
                if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                        || (funcDef->type() == NLS_MACRO_FUNCTION))) {
                    Error(L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
                }
                ArrayOfVector argIn;
                argIn.push_back(r);
                argIn.push_back(subsindices.front());
                CallStack backupCallStack = callstack;
                ArrayOfVector rv = funcDef->evaluateFunction(this, argIn, nLhs);
                if (!rv.empty()) {
                    rv[0].name("");
                }
                callstack = backupCallStack;
                return rv;
            }
        }
        if (!useClassSpecificSubsref && subtypes.size() == 1 && subtypes.front() == "{}"
            && subsindices.size() == 1
            && classdefHasMethodDefinition(
                currentClass, EVALUATOR_INDEXING_FUNCTION_BRACE_REFERENCE)) {
            // RedefinesBrace declares braceReference protected; route via the public
            // dispatcher (builds the IndexingOperation, calls the protected hook).
            const std::string braceRefMethod
                = classdefIsA(currentClass, NLS_MIXIN_INDEXING_REDEFINES_BRACE)
                ? std::string("__nls_ridx_brace_ref__")
                : std::string(EVALUATOR_INDEXING_FUNCTION_BRACE_REFERENCE);
            if (classdefResolveMethodFunction(currentClass, braceRefMethod,
                    functionNameSimpleExtractClass, getClassdefAccessContext())
                && lookupOverloadFunctionCached(
                    _context, functionNameSimpleExtractClass, funcDef)) {
                haveFunction = true;
                if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                        || (funcDef->type() == NLS_MACRO_FUNCTION))) {
                    Error(L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
                }
                ArrayOfVector argIn;
                argIn.push_back(r);
                argIn.push_back(subsindices.front());
                CallStack backupCallStack = callstack;
                ArrayOfVector rv = funcDef->evaluateFunction(this, argIn, nLhs);
                if (!rv.empty()) {
                    rv[0].name("");
                }
                callstack = backupCallStack;
                return rv;
            }
        }
        if (!useClassSpecificSubsref) {
            // the class defines no subsref anywhere (declared methods, @-dir
            // and path files are all covered by the check above), so the
            // generic overload lookup below cannot succeed: skip its repeated
            // negative resolution
            return {};
        }
    }
    if (lookupOverloadFunctionCached(_context, functionNameSimpleExtractClass, funcDef)) {
        haveFunction = true;
        if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                || (funcDef->type() == NLS_MACRO_FUNCTION))) {
            Error(L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
        }
        ArrayOfVector argIn;
        argIn.push_back(r);

        stringVector fieldnames = { EVALUATOR_INDEXING_FIELD_TYPE, EVALUATOR_INDEXING_FIELD_SUBS };
        ArrayOfVector fieldvalues;

        Dimensions dims(1, subtypes.size());

        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), fieldnames, false));
        ArrayOf substruct = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);

        ArrayOfVector typesVector;
        ArrayOfVector subsVector;
        typesVector.reserve(subtypes.size());
        subsVector.reserve(subsindices.size());

        for (auto& t : subtypes) {
            typesVector.push_back(ArrayOf::characterArray(t));
        }
        for (auto& t : subsindices) {
            subsVector.push_back(t);
        }
        if (!typesVector.empty()) {
            substruct.setFieldAsList(EVALUATOR_INDEXING_FIELD_TYPE, typesVector);
            substruct.setFieldAsList(EVALUATOR_INDEXING_FIELD_SUBS, subsVector);
        }

        argIn.push_back(substruct);

        CallStack backupCallStack = callstack;
        ArrayOfVector rv = funcDef->evaluateFunction(this, argIn, nLhs);
        if (!rv.empty()) {
            rv[0].name("");
        }
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
    Dimensions dim(v.dimensions());
    ArrayOf res;
    bool useStandardMethod = true;
    if (v.isClassType()) {
        bool needToBeOverloaded = false;
        res = EndOverloadReference(v, index, count, needToBeOverloaded);
        useStandardMethod = needToBeOverloaded;
    }
    if (useStandardMethod) {
        if (count == 1) {
            return ArrayOf::doubleScalar(static_cast<double>(dim.elementCount()));
        }
        if (index + 1 == static_cast<indexType>(count)
            && static_cast<indexType>(count) < dim.length()) {
            // the last of fewer subscripts than dimensions spans the trailing
            // dimensions: A(end,end) on a 2x3x4 array is A(2,12)
            double folded = 1;
            for (indexType k = index; k < dim.length(); ++k) {
                folded *= static_cast<double>(dim.dimensionLength(k));
            }
            return ArrayOf::doubleScalar(folded);
        }
        return ArrayOf::doubleScalar(static_cast<double>(dim.dimensionLength(index)));
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
    lookupOverloadFunctionCached(_context, functionNameEndClass, funcDef);
    if (funcDef) {
        ArrayOfVector input;
        input << v;
        input << ArrayOf::doubleScalar((double)index + 1);
        input << ArrayOf::doubleScalar((double)count);
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
