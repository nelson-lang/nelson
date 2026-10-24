//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cctype>
#include "ClassdefSuperCall.hpp"
#include "ClassdefHandleObject.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefParserConstants.hpp"
#include "ClassdefParserGenerator.hpp"
#include "Context.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
#define CLASSDEF_ERROR_NO_SUCH_METHOD "No such method: "
//=============================================================================
namespace Nelson {
//=============================================================================
// Generated constructor function name: __classdef_<sanitized Class>_constructor__.
static std::string
superConstructorFunctionName(const std::string& className)
{
    return std::string(CLASSDEF_GENERATED_FUNCTION_PREFIX)
        + classdef_detail::sanitizeGeneratedFunctionName(className) + "_constructor"
        + CLASSDEF_GENERATED_FUNCTION_SUFFIX;
}
//=============================================================================
static ClassdefHandleObject*
superCallHandleObject(const ArrayOf& value)
{
    if (!value.isHandle() || !value.isScalar()) {
        Error(L"Nelson:types:handleExpected:wrongTypeArgumentHandleExpected",
            L"Wrong type for argument. Handle expected.");
    }
    auto* object = dynamic_cast<ClassdefHandleObject*>(value.asHandleScalar());
    if (object == nullptr) {
        Error(L"Nelson:interpreter:invalidClassdefHandle", L"Invalid classdef handle.");
    }
    return object;
}
//=============================================================================
// Copy stored properties from source to destination (handle and value class forms).
static ArrayOf
superCallCopyStoredProperties(const ArrayOf& destination, const ArrayOf& source)
{
    if (destination.isHandle() && source.isHandle()) {
        superCallHandleObject(destination)
            ->copyStoredPropertiesFrom(*superCallHandleObject(source));
        return destination;
    }
    if (destination.isClassType() && source.isClassType()) {
        ArrayOf result = destination;
        const std::string sourceClassName = source.classType();
        for (const auto& property : classdefPropertyDefinitions(sourceClassName)) {
            if (classdefHasConstantProperty(sourceClassName, property.name)
                || classdefHasDependentProperty(sourceClassName, property.name)) {
                continue;
            }
            if (result.isField(property.name) && source.isField(property.name)) {
                ArrayOf value = source.field(property.name);
                result.setField(property.name, value);
            }
        }
        return result;
    }
    return destination;
}
//=============================================================================
static ArrayOfVector
superConstructorCall(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 2 || eval == nullptr || eval->getContext() == nullptr) {
        Error(L"Nelson:interpreter:invalidSuperclassConstructorCall",
            L"Invalid superclass constructor call.");
    }
    if (!argIn[1].isRowVectorCharacterArray()) {
        Error(L"Nelson:types:wrongArgumentTypeExpectedDot",
            L"Wrong type for argument #%d: %s expected.", 2, L"String");
    }
    std::string className = argIn[1].asUtf8String();
    classdefLoadClass(className);
    // obj = obj@double(v): the base call stores the base data in the hidden base
    // property of the object.
    if (argIn[0].isClassType()) {
        const std::string baseType = classdefBuiltinBaseType(argIn[0].classType());
        std::string requestedType = className;
        std::transform(requestedType.begin(), requestedType.end(), requestedType.begin(),
            [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });
        if (!baseType.empty() && requestedType == baseType) {
            FunctionDef* conversion = nullptr;
            if (!eval->getContext()->lookupFunction(baseType, conversion)
                || conversion == nullptr) {
                RaiseError(_(CLASSDEF_ERROR_NO_SUCH_METHOD) + className,
                    "Nelson:interpreter:noSuchMethod");
            }
            ArrayOfVector inputs;
            for (size_t k = 2; k < argIn.size(); ++k) {
                inputs << argIn[k];
            }
            ArrayOf baseValue;
            if (inputs.empty()) {
                baseValue = ArrayOf::empty();
            } else {
                ArrayOfVector result = conversion->evaluateFunction(eval, inputs, 1);
                baseValue = result.empty() ? ArrayOf::empty() : result[0];
            }
            ArrayOf objectValue = argIn[0];
            objectValue.setField(classdefBuiltinBasePropertyName(), baseValue);
            retval << objectValue;
            return retval;
        }
    }
    FunctionDef* functionDef = nullptr;
    if (!eval->getContext()->lookupFunction(superConstructorFunctionName(className), functionDef)
        && !eval->getContext()->lookupFunction(className, functionDef)) {
        functionDef = nullptr;
    }
    if (functionDef == nullptr) {
        RaiseError(_(CLASSDEF_ERROR_NO_SUCH_METHOD) + className, "Nelson:interpreter:noSuchMethod");
    }
    ArrayOfVector inputs;
    for (size_t k = 2; k < argIn.size(); ++k) {
        inputs << argIn[k];
    }
    ArrayOfVector result = functionDef->evaluateFunction(eval, inputs, 1);
    ArrayOf source = result.empty() ? ArrayOf::empty() : result[0];
    retval << superCallCopyStoredProperties(argIn[0], source);
    if (source.isHandle() && source.isScalar() && argIn[0].isHandle()) {
        // The base constructor built a transient object whose state now lives
        // in the derived one: dropping it must not run the base destructor.
        superCallHandleObject(source)->setFinalizerRequired(false);
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
superMethodCall(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() < 2 || eval == nullptr || eval->getContext() == nullptr) {
        Error(
            L"Nelson:interpreter:invalidSuperclassMethodCall", L"Invalid superclass method call.");
    }
    if (!argIn[0].isRowVectorCharacterArray()) {
        Error(L"Nelson:types:wrongArgumentTypeExpectedDot",
            L"Wrong type for argument #%d: %s expected.", 1, L"String");
    }
    if (!argIn[1].isRowVectorCharacterArray()) {
        Error(L"Nelson:types:wrongArgumentTypeExpectedDot",
            L"Wrong type for argument #%d: %s expected.", 2, L"String");
    }
    std::string className = argIn[0].asUtf8String();
    std::string methodName = argIn[1].asUtf8String();
    std::string functionName;
    if (!classdefResolveMethodFunction(
            className, methodName, functionName, eval->getClassdefAccessContext())) {
        RaiseError(
            _(CLASSDEF_ERROR_NO_SUCH_METHOD) + methodName, "Nelson:interpreter:noSuchMethod");
    }
    FunctionDef* functionDef = nullptr;
    if (!eval->getContext()->lookupFunction(functionName, functionDef) || functionDef == nullptr) {
        RaiseError(
            _(CLASSDEF_ERROR_NO_SUCH_METHOD) + methodName, "Nelson:interpreter:noSuchMethod");
    }
    ArrayOfVector inputs;
    for (size_t k = 2; k < argIn.size(); ++k) {
        inputs << argIn[k];
    }
    return functionDef->evaluateFunction(eval, inputs, nLhs);
}
//=============================================================================
ArrayOfVector
evaluateClassdefSuperCall(Evaluator* eval, bool isConstructor, const ArrayOfVector& argIn, int nLhs)
{
    return isConstructor ? superConstructorCall(eval, nLhs, argIn)
                         : superMethodCall(eval, nLhs, argIn);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
