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
#include "BytecodeVMInternal.hpp"
#include "ClassdefHandleObject.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefPropertyAccess.hpp"
#include "ContainersMapFastPaths.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Operators.hpp"
#include "OverloadName.hpp"
#include "ClassName.hpp"
#include "ClassToString.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define EVALUATOR_FUNCTION_GET "get"
#define EVALUATOR_FUNCTION_INVOKE "invoke"
#define EVALUATOR_FUNCTION_ISMETHOD "ismethod"
#define EVALUATOR_FUNCTION_SET "set"
#define EVALUATOR_FUNCTION_DOT_ASSIGN "dotAssign"
#define EVALUATOR_FUNCTION_DOT_REFERENCE "dotReference"
//=============================================================================
static bool
isContainersMapReadableProperty(
    const std::string& className, const std::string& fieldname, const std::string& accessContext);
//=============================================================================
void
Evaluator::setHandle(ArrayOf r, const std::string& fieldname, const ArrayOfVector& fieldvalue)
{
    if (r.isHandle() && !fieldvalue.empty() && tryAssignHandleDot(r, fieldname, fieldvalue[0])) {
        return;
    }
    std::string currentType;
    if (r.isGraphicsObject()) {
        currentType = ClassToString(r.dataClass());
    } else {
        currentType = r.handleCategory();
    }
    std::string functionNameSetHandle
        = getOverloadFunctionName(currentType, EVALUATOR_FUNCTION_SET);
    Context* _context = this->getContext();
    FunctionDef* funcDef = nullptr;
    if (r.isHandle() && classdefLoadClass(currentType)
        && !classdefHasProperty(currentType, fieldname)
        && !classdefHasMethodDefinition(currentType, fieldname)
        && classdefHasMethodDefinition(currentType, EVALUATOR_FUNCTION_DOT_ASSIGN)) {
        std::string functionNameDotAssign;
        if (classdefResolveMethodFunction(currentType, EVALUATOR_FUNCTION_DOT_ASSIGN,
                functionNameDotAssign, getClassdefAccessContext())
            && _context->lookupFunction(functionNameDotAssign, funcDef)) {
            if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                    || (funcDef->type() == NLS_MACRO_FUNCTION))) {
                Error(L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
            }
            ArrayOfVector argIn;
            argIn.push_back(r);
            argIn.push_back(ArrayOf::characterArray(fieldname));
            argIn.push_back(fieldvalue[0]);
            funcDef->evaluateFunction(this, argIn, 0);
            return;
        }
    }
    if (!_context->lookupFunction(functionNameSetHandle, funcDef)) {
        Error(L"Nelson:interpreter:functionNotFound:functionNotFound", L"Function not found.");
    }
    if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION))) {
        Error(L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
    }
    ArrayOfVector argIn;
    argIn.push_back(r);
    argIn.push_back(ArrayOf::characterArray(fieldname));
    argIn.push_back(fieldvalue[0]);
    funcDef->evaluateFunction(this, argIn, 0);
}
//=============================================================================
ArrayOfVector
Evaluator::getHandle(ArrayOf r, const std::string& fieldname, const ArrayOfVector& params)
{
    ArrayOfVector argIn;
    std::string currentType;
    std::string functionNameCurrentType;
    if (r.isGraphicsObject()) {
        currentType = ClassToString(r.dataClass());
        functionNameCurrentType = getOverloadFunctionName(currentType, fieldname);
    } else {
        currentType = r.handleCategory();
        functionNameCurrentType = getOverloadFunctionName(currentType, fieldname);
    }
    if ((r.isClassType() || r.isHandle()) && classdefLoadClass(currentType)
        && !classdefHasProperty(currentType, fieldname)
        && classdefHasMethodDefinition(currentType, fieldname)) {
        if (!classdefResolveMethodFunction(
                currentType, fieldname, functionNameCurrentType, getClassdefAccessContext())) {
            Error("Nelson:interpreter:noSuchProperty", "No such property: %s", fieldname);
        }
    }
    Context* _context = this->getContext();
    FunctionDef* funcDef = nullptr;
    if (r.isHandle() && classdefLoadClass(currentType)
        && !classdefHasProperty(currentType, fieldname)
        && !classdefHasMethodDefinition(currentType, fieldname)
        && classdefHasMethodDefinition(currentType, EVALUATOR_FUNCTION_DOT_REFERENCE)) {
        std::string functionNameDotReference;
        if (classdefResolveMethodFunction(currentType, EVALUATOR_FUNCTION_DOT_REFERENCE,
                functionNameDotReference, getClassdefAccessContext())
            && _context->lookupFunction(functionNameDotReference, funcDef)) {
            if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                    || (funcDef->type() == NLS_MACRO_FUNCTION))) {
                Error(L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
            }
            ArrayOfVector dotArgs;
            dotArgs.push_back(r);
            dotArgs.push_back(ArrayOf::characterArray(fieldname));
            ArrayOfVector result = funcDef->evaluateFunction(this, dotArgs, 1);
            if (!params.empty() && !result.empty()) {
                ArrayOf value = result[0];
                if (value.isFunctionHandle()) {
                    return bcvm_detail::callFunctionHandle(this, value, params, 1);
                }
                ArrayOfVector mutableParams = params;
                if (params.size() == 1) {
                    result[0] = value.vectorSubset(mutableParams[0]);
                } else {
                    result[0] = value.nDimSubset(mutableParams);
                }
            }
            return result;
        }
    }
    if (_context->lookupFunction(functionNameCurrentType, funcDef)) {
        if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                || (funcDef->type() == NLS_MACRO_FUNCTION))) {
            Error(L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
        }
        argIn.reserve(params.size() + 1);
        argIn.push_back(r);
        for (const ArrayOf& a : params) {
            argIn.push_back(a);
        }
        return funcDef->evaluateFunction(this, argIn, 1);
    }
    // A property the fast path skipped (declared in an attributed
    // 'properties (...)' block) is read through the canonical classdef accessor,
    // which applies access policy, dependent getters and Get events, instead of
    // falling through to a user-defined get() method.
    if (r.isHandle() && classdefLoadClass(currentType)
        && classdefHasProperty(currentType, fieldname)) {
        ArrayOfVector getArgs;
        getArgs.push_back(r);
        getArgs.push_back(ArrayOf::characterArray(fieldname));
        ArrayOfVector result = evaluateClassdefHandleGet(this, getArgs);
        if (!params.empty() && !result.empty()) {
            ArrayOf value = result[0];
            ArrayOfVector mutableParams = params;
            if (params.size() == 1) {
                result[0] = value.vectorSubset(mutableParams[0]);
            } else {
                result[0] = value.nDimSubset(mutableParams);
            }
        }
        return result;
    }
    std::string functionNameGetHandle
        = getOverloadFunctionName(currentType, EVALUATOR_FUNCTION_GET);
    if (!context->lookupFunction(functionNameGetHandle, funcDef)) {
        Error(
            "Nelson:interpreter:functionNotFound", "Function not found: %s", functionNameGetHandle);
    }
    if (!((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION))) {
        Error(L"Nelson:interpreter:invalidTypeFunction", L"Type function not valid.");
    }
    argIn.push_back(r);
    argIn.push_back(ArrayOf::characterArray(fieldname));
    ArrayOfVector result = funcDef->evaluateFunction(this, argIn, 1);
    if (!params.empty() && !result.empty()) {
        ArrayOf value = result[0];
        if (value.isFunctionHandle()) {
            return bcvm_detail::callFunctionHandle(this, value, params, 1);
        }
        ArrayOfVector mutableParams = params;
        if (params.size() == 1) {
            result[0] = value.vectorSubset(mutableParams[0]);
        } else {
            result[0] = value.nDimSubset(mutableParams);
        }
    }
    return result;
}
//=============================================================================
bool
Evaluator::isObjectMethod(const ArrayOf& r, const std::string& methodName)
{
    Context* _context = this->getContext();
    FunctionDef* funcDef = nullptr;
    if (r.isClassType()) {
        const std::string currentType = r.classType();
        std::string methodFunctionName;
        if (classdefResolveMethodFunction(
                currentType, methodName, methodFunctionName, getClassdefAccessContext())
            && _context->lookupFunction(methodFunctionName, funcDef)) {
            return true;
        }
        if (classdefLoadClass(currentType)) {
            return false;
        }
    }
    if (r.isHandle()) {
        const std::string currentType = r.handleClassName();
        std::string methodFunctionName;
        if (classdefLoadClass(currentType)) {
            return classdefResolveMethodFunction(
                       currentType, methodName, methodFunctionName, getClassdefAccessContext())
                && _context->lookupFunction(methodFunctionName, funcDef);
        }
    }
    if (_context->lookupFunction(EVALUATOR_FUNCTION_ISMETHOD, funcDef)) {
        ArrayOfVector argIn;
        argIn.push_back(r);
        argIn.push_back(ArrayOf::characterArray(methodName));
        ArrayOfVector res = funcDef->evaluateFunction(this, argIn, 1);
        if (res.size() != 1) {
            return false;
        }
        if (res[0].isLogical() && res[0].isScalar()) {
            return res[0].asLogicalScalar();
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

    std::string currentType = r.isGraphicsObject() ? ClassToString(r.dataClass())
        : r.isHandle()                             ? r.handleCategory()
        : r.isClassType()                          ? r.classType()
                                                   : "";

    if (!currentType.empty()) {
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
        if (canUseDirectMethod) {
            _context->lookupFunction(directMethod, funcDef);
        }
        if (canUseDirectMethod && funcDef != nullptr) {
            ArrayOfVector argIn;
            int nLhs = funcDef->outputArgCount() == 0 ? 0 : 1;
            argIn.reserve(params.size() + 1);
            argIn.push_back(r);
            for (const ArrayOf& a : params) {
                argIn.push_back(a);
            }
            return funcDef->evaluateFunction(this, argIn, nLhs);
        }
        std::string functionNameCurrentType
            = getOverloadFunctionName(currentType, EVALUATOR_FUNCTION_INVOKE);
        if (_context->lookupFunction(functionNameCurrentType, funcDef)) {
            ArrayOfVector argIn;
            int nLhs = funcDef->outputArgCount() == 0 ? 0 : 1;
            argIn.push_back(r);
            argIn.push_back(ArrayOf::characterArray(methodName));
            for (const ArrayOf& a : params) {
                argIn.push_back(a);
            }
            return funcDef->evaluateFunction(this, argIn, nLhs);
        }
        return getHandle(r, methodName, params);
    }
    Error("Nelson:interpreter:functionNotFound", "Function not found: %s",
        std::string(EVALUATOR_FUNCTION_INVOKE));
    return {};
}
//=============================================================================
ArrayOfVector
Evaluator::bytecodeGetOrInvokeHandle(
    const ArrayOf& r, const std::string& fieldname, const ArrayOfVector& params)
{
    // a single returned object: ArrayOfVector has no move operations
    ArrayOfVector result;
    if (!(r.isHandle() && params.empty() && tryReadHandleDot(r, fieldname, result))) {
        result = isObjectMethod(r, fieldname) ? invokeMethod(r, fieldname, params)
                                              : getHandle(r, fieldname, params);
    }
    return result;
}
//=============================================================================
bool
Evaluator::tryReadHandleDot(const ArrayOf& r, const std::string& fieldname, ArrayOfVector& result)
{
    if (fieldname == "Count" && tryContainersMapCount(r, result)) {
        return true;
    }
    auto* object = r.isScalar() ? dynamic_cast<ClassdefHandleObject*>(r.asHandleScalar()) : nullptr;
    if (r.isScalar() && object == nullptr) {
        return false;
    }
    const std::string className = object != nullptr ? object->category() : r.handleCategory();
    if (!classdefLoadClass(className)) {
        return false;
    }
    // the overload flag rides on the cached plain property entry
    bool subsrefOverloaded = false;
    const ClassdefPropertyDefinition* plainProperty
        = classdefPlainStoredProperty(className, fieldname, false, subsrefOverloaded);
    if (plainProperty == nullptr) {
        // h.method keeps invoking the method, as the h.method(args) call form
        subsrefOverloaded = classdefHasMethod(className, SUBSREF_OPERATOR_STR)
            && !classdefHasMethodDefinition(className, fieldname);
    }
    if (isOverloadedDotAccess(className, subsrefOverloaded)
        && !isContainersMapReadableProperty(className, fieldname, getClassdefAccessContext())) {
        bool haveFunction = false;
        result = extractClass(r, { "." }, { ArrayOf::characterArray(fieldname) }, haveFunction, 1);
        return haveFunction;
    }
    if (object == nullptr || object->hasDynamicProperty(fieldname)) {
        return false;
    }
    // plain stored property: read directly, the generated get wrapper would
    // redo the access checks through a full function call per read
    if (plainProperty != nullptr && object->hasStoredProperty(fieldname)) {
        result.push_back(object->getProperty(fieldname));
        return true;
    }
    // declared property with attributes (access, Dependent, ...): go straight
    // to the canonical accessor, as getHandle ends up doing, without first
    // resolving a method of that name (a miss probes every class of the
    // hierarchy and is never memoized)
    if (classdefHasProperty(className, fieldname)) {
        ArrayOfVector getArgs;
        getArgs.push_back(r);
        getArgs.push_back(ArrayOf::characterArray(fieldname));
        result = evaluateClassdefHandleGet(this, getArgs);
        return true;
    }
    return false;
}
//=============================================================================
bool
Evaluator::tryAssignHandleDot(const ArrayOf& r, const std::string& fieldname, const ArrayOf& value)
{
    auto* object = r.isScalar() ? dynamic_cast<ClassdefHandleObject*>(r.asHandleScalar()) : nullptr;
    if (r.isScalar() && object == nullptr) {
        return false;
    }
    const std::string className = object != nullptr ? object->category() : r.handleCategory();
    if (!classdefLoadClass(className)) {
        return false;
    }
    bool subsasgnOverloaded = false;
    const ClassdefPropertyDefinition* plainProperty
        = classdefPlainStoredProperty(className, fieldname, true, subsasgnOverloaded);
    if (plainProperty == nullptr) {
        subsasgnOverloaded = classdefHasMethod(className, SUBSASGN_OPERATOR_STR);
    }
    if (isOverloadedDotAccess(className, subsasgnOverloaded)) {
        bool haveFunction = false;
        bytecodeAssignClass(
            r, { "." }, { ArrayOf::characterArray(fieldname) }, value, haveFunction);
        return haveFunction;
    }
    if (object == nullptr || object->hasDynamicProperty(fieldname)) {
        return false;
    }
    // plain stored property: assign directly, the generated set wrapper would
    // redo the access checks through a full function call per assignment
    if (plainProperty != nullptr) {
        if (plainProperty->validationExpression.empty()) {
            object->setStoredProperty(fieldname, value);
        } else {
            object->setStoredProperty(
                fieldname, classdefValidatePropertyValue(this, *plainProperty, value));
        }
        return true;
    }
    // A property the fast path skipped (attributed 'properties (...)' block) is
    // written through the canonical classdef accessor, which applies access
    // policy, Constant/AbortSet, validation and Set events, instead of falling
    // through to a user set().
    if (classdefHasProperty(className, fieldname)) {
        ArrayOfVector setArgs;
        setArgs.push_back(r);
        setArgs.push_back(ArrayOf::characterArray(fieldname));
        setArgs.push_back(value);
        evaluateClassdefHandleSet(this, setArgs);
        return true;
    }
    return false;
}
//=============================================================================
bool
isContainersMapReadableProperty(
    const std::string& className, const std::string& fieldname, const std::string& accessContext)
{
    // containers.Map's subsref hands '.' to builtin('subsref'): a property
    // readable from the caller reads the same without the two function calls
    return className == NLS_HANDLE_CONTAINERS_MAP_CATEGORY_STR
        && classdefHasExactProperty(className, fieldname)
        && classdefCanGetProperty(className, fieldname, accessContext);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
