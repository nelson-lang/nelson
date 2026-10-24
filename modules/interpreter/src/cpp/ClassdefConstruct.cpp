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
#include <vector>
#include "ClassdefConstruct.hpp"
#include "ClassdefHandleFinalization.hpp"
#include "ClassdefHandleObject.hpp"
#include "HandleManager.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefParserConstants.hpp"
#include "ClassdefParserGenerator.hpp"
#include "Context.hpp"
#include "FunctionDef.hpp"
#include "NelsonException.hpp"
#include "ClassdefLayout.hpp"
#include "Dimensions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
#define CLASSDEF_ERROR_VALUE_CLASS_NOT_FOUND "Value class not found: "
#define CLASSDEF_ERROR_HANDLE_CLASS_NOT_FOUND "Handle class not found: "
#define CLASSDEF_ERROR_CANNOT_ACCESS_PROPERTY_DEFAULT "Cannot access property default value: "
#define CLASSDEF_ERROR_CANNOT_SET_PROPERTY "Cannot set property: "
//=============================================================================
namespace Nelson {
//=============================================================================
static Dimensions
classdefEmptyDimensions(const ArrayOf& dimensionsArgument)
{
    if (!dimensionsArgument.isNumeric() || !dimensionsArgument.isVector()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
            L"numeric vector");
    }
    ArrayOf argument = dimensionsArgument;
    std::vector<indexType> dimensionsVector = argument.asIndexVector();
    if (dimensionsVector.empty()) {
        dimensionsVector = { 0, 0 };
    } else if (dimensionsVector.size() == 1) {
        dimensionsVector.push_back(dimensionsVector.front());
    }
    return Dimensions(dimensionsVector);
}
//=============================================================================
ArrayOf
createClassdefInstance(bool isHandle, const ArrayOfVector& argIn)
{
    if (argIn.empty() || !argIn[0].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    const std::string className = argIn[0].asUtf8String();
    if (isHandle) {
        if (!classdefLoadClass(className) || !classdefIsHandleClass(className)) {
            RaiseError(_(CLASSDEF_ERROR_HANDLE_CLASS_NOT_FOUND) + className,
                "Nelson:interpreter:classNotFound");
        }
        if (argIn.size() >= 2) {
            Dimensions dimensions = classdefEmptyDimensions(argIn[1]);
            return ArrayOf::emptyHandle(className, dimensions);
        }
        auto* handleObject = new ClassdefHandleObject(className);
        ClassdefLayoutPtr layout = classdefLayout(className);
        if (layout != nullptr) {
            handleObject->reserveStoredProperties(layout->storagePropertyNames.size());
        }
        return ArrayOf::handle(handleObject);
    }
    ClassdefLayoutPtr layout = classdefLayout(className);
    if (!layout || classdefIsHandleClass(className)) {
        RaiseError(_(CLASSDEF_ERROR_VALUE_CLASS_NOT_FOUND) + className,
            "Nelson:interpreter:classNotFound");
    }
    if (argIn.size() >= 2) {
        Dimensions dimensions = classdefEmptyDimensions(argIn[1]);
        ArrayOf object
            = ArrayOf::emptyClassdef(className, layout->storagePropertyNames, dimensions);
        object.setClassdefLayout(layout);
        return object;
    }
    Dimensions dims(1, 1);
    auto* elements = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
        NLS_CLASSDEF_ARRAY, dims.elementCount(), layout->storagePropertyNames, false));
    ArrayOf object(NLS_CLASSDEF_ARRAY, dims, elements, false, layout->storagePropertyNames);
    object.setClassType(className);
    object.setClassdefLayout(layout);
    return object;
}
//=============================================================================
ArrayOfVector
evaluateClassdefPropertyDefault(Evaluator* eval, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 2 || !argIn[0].isRowVectorCharacterArray()
        || !argIn[1].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    const std::string className = argIn[0].asUtf8String();
    const std::string propertyName = argIn[1].asUtf8String();
    const std::string accessContext
        = eval == nullptr ? std::string() : eval->getClassdefAccessContext();
    if (accessContext.empty()
        || (accessContext != className && !classdefIsA(accessContext, className))) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_ACCESS_PROPERTY_DEFAULT) + propertyName,
            "Nelson:interpreter:cannotAccessPropertyDefault");
    }
    if (argIn.size() >= 3) {
        classdefStorePropertyDefault(className, propertyName, argIn[2]);
        return retval;
    }
    ArrayOf value;
    const bool found = classdefStoredPropertyDefault(className, propertyName, value);
    retval << ArrayOf::logicalScalar(found);
    retval << (found ? value : ArrayOf::empty());
    return retval;
}
//=============================================================================
static ClassdefHandleObject*
storedWriteHandleObject(const ArrayOf& value)
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
ArrayOfVector
evaluateClassdefSetStored(Evaluator* eval, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 3 || !argIn[1].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }
    ClassdefHandleObject* object = storedWriteHandleObject(argIn[0]);
    const std::string className = object->category();
    const std::string propertyName = argIn[1].asUtf8String();
    const std::string accessContext
        = eval == nullptr ? std::string() : eval->getClassdefAccessContext();
    if (accessContext != className) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotSetProperty");
    }
    ClassdefLayoutPtr layout = classdefLayout(className);
    if (layout == nullptr
        || std::find(layout->directStoredPropertyNames.begin(),
               layout->directStoredPropertyNames.end(), propertyName)
            == layout->directStoredPropertyNames.end()) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotSetProperty");
    }
    object->setStoredProperty(propertyName, argIn[2]);
    return retval;
}
//=============================================================================
ArrayOfVector
evaluateClassdefValueSetStored(Evaluator* eval, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 3 || !argIn[1].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }
    ArrayOf object = argIn[0];
    if (object.dataClass() != NLS_CLASSDEF_ARRAY || !object.isScalar()) {
        RaiseError(_(CLASSDEF_ERROR_VALUE_CLASS_NOT_FOUND) + object.classType(),
            "Nelson:interpreter:classNotFound");
    }
    const std::string objectClass = object.classType();
    const std::string propertyName = argIn[1].asUtf8String();
    const std::string accessContext
        = eval == nullptr ? std::string() : eval->getClassdefAccessContext();
    // Only the class's own generated code (the class itself, or a subclass whose
    // constructor initializes the inherited part) may write stored fields
    // directly, bypassing the set method.
    if (accessContext.empty()
        || (accessContext != objectClass && !classdefIsA(objectClass, accessContext))) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotSetProperty");
    }
    ClassdefLayoutPtr layout = classdefLayout(objectClass);
    if (layout == nullptr
        || std::find(layout->storagePropertyNames.begin(), layout->storagePropertyNames.end(),
               propertyName)
            == layout->storagePropertyNames.end()) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotSetProperty");
    }
    object.ensureSingleOwner();
    ArrayOf value = argIn[2];
    object.setField(propertyName, value);
    retval << object;
    return retval;
}
//=============================================================================
ArrayOfVector
evaluateClassdefHandleElement(const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 2 || !argIn[0].isHandle()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    ArrayOf handles = argIn[0];
    ArrayOf index = argIn[1];
    retval << handles.vectorSubset(index);
    return retval;
}
//=============================================================================
ArrayOfVector
evaluateClassdefHandleStruct(const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.empty()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    retval << storedWriteHandleObject(argIn[0])->toStruct();
    return retval;
}
//=============================================================================
ArrayOfVector
evaluateClassdefHandleDelete(Evaluator* eval, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.empty() || !argIn[0].isHandle()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    const auto* qp = static_cast<const HandleRef*>(argIn[0].dataPointer());
    indexType elementCount = argIn[0].elementCount();
    for (indexType k = 0; qp != nullptr && k < elementCount; ++k) {
        // explicit delete(obj): errors raised by the user delete propagate
        RunClassdefDelete(eval, qp[k], false);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
evaluateClassdefHandleIsValid(const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.empty() || !argIn[0].isHandle()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    const ArrayOf& A = argIn[0];
    Dimensions dimsA = A.dimensions();
    auto* qp = static_cast<const HandleRef*>(A.dataPointer());
    if (qp == nullptr) {
        retval << ArrayOf::empty(dimsA);
        return retval;
    }
    indexType elementCount = dimsA.elementCount();
    auto* resArray = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, elementCount, stringVector(), false));
    for (indexType k = 0; k < elementCount; ++k) {
        bool valid = qp[k] && qp[k]->isValid();
        if (valid) {
            // an object is reported invalid as soon as its destructor runs
            auto* object = dynamic_cast<ClassdefHandleObject*>(qp[k]->object());
            valid = object == nullptr || !object->deleteInvoked();
        }
        resArray[k] = valid ? 1 : 0;
    }
    retval << ArrayOf(NLS_LOGICAL, dimsA, resArray);
    return retval;
}
//=============================================================================
static std::string
copyConstructorFunctionName(const std::string& className)
{
    return std::string(CLASSDEF_GENERATED_FUNCTION_PREFIX)
        + classdef_detail::sanitizeGeneratedFunctionName(className) + "_constructor"
        + CLASSDEF_GENERATED_FUNCTION_SUFFIX;
}
//=============================================================================
// Build a shallow copy of a single source handle: a new object of the same
// class, default-constructed (via the class constructor when it accepts no
// arguments, otherwise a bare instance) then populated with the source's
// stored, copyable properties. Returns a scalar handle ArrayOf.
static ArrayOf
shallowCopyScalarHandle(Evaluator* eval, ClassdefHandleObject* source, const std::string& className)
{
    ArrayOf copyValue;
    ClassdefHandleObject* copy = nullptr;
    FunctionDef* constructor = nullptr;
    try {
        if (eval != nullptr && eval->getContext() != nullptr
            && (eval->getContext()->lookupFunction(
                    copyConstructorFunctionName(className), constructor)
                || eval->getContext()->lookupFunction(className, constructor))
            && constructor != nullptr) {
            ArrayOfVector constructed = constructor->evaluateFunction(eval, {}, 1);
            if (!constructed.empty() && constructed[0].isHandle()) {
                auto* constructedObject
                    = dynamic_cast<ClassdefHandleObject*>(constructed[0].asHandleScalar());
                if (constructedObject != nullptr && constructedObject->category() == className) {
                    copyValue = constructed[0];
                    copy = constructedObject;
                }
            }
        }
    } catch (const NelsonException&) {
        copy = nullptr;
    }
    if (copy == nullptr) {
        copy = new ClassdefHandleObject(className);
        copyValue = ArrayOf::handle(copy);
    }
    copy->copyStoredPropertiesFrom(*source, false);
    return copyValue;
}
//=============================================================================
static HandleRef
scalarHandleRef(const ArrayOf& value)
{
    const auto* refs = static_cast<const HandleRef*>(value.dataPointer());
    return (refs != nullptr) ? refs[0] : HandleRef();
}
//=============================================================================
ArrayOfVector
evaluateClassdefCopy(Evaluator* eval, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.empty()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    // Value objects (and anything that is not a handle) copy by value already.
    if (!argIn[0].isHandle()) {
        retval << argIn[0];
        return retval;
    }
    const ArrayOf& source = argIn[0];
    Dimensions dims = source.dimensions();
    const indexType elementCount = dims.elementCount();
    const auto* sourceRefs = static_cast<const HandleRef*>(source.dataPointer());
    // An empty handle array (or one with no backing storage) copies to itself.
    if (elementCount == 0 || sourceRefs == nullptr) {
        retval << source;
        return retval;
    }
    // Determine the class from the first live element; a handle array is
    // homogeneous, so this names the whole array.
    std::string className;
    for (indexType k = 0; k < elementCount; ++k) {
        if (sourceRefs[k]) {
            auto* object = dynamic_cast<ClassdefHandleObject*>(sourceRefs[k]->object());
            if (object != nullptr) {
                className = object->category();
                break;
            }
        }
    }
    if (className.empty()) {
        retval << source;
        return retval;
    }
    // A subclass may override copyElement to customise the per-element copy
    // (e.g. deep-copying handle-valued properties). Resolve it as if called
    // from within the class so a protected override is reachable.
    std::string copyElementFunctionName;
    FunctionDef* copyElementDef = nullptr;
    if (eval != nullptr && eval->getContext() != nullptr
        && classdefResolveMethodFunction(
            className, CLASSDEF_FUNCTION_COPY_ELEMENT, copyElementFunctionName, className)) {
        eval->getContext()->lookupFunction(copyElementFunctionName, copyElementDef);
    }
    // Allocate the result array with the same shape; its class is carried by
    // the stored element categories (see ClassName for NLS_HANDLE).
    auto* copyRefs = static_cast<HandleRef*>(
        ArrayOf::allocateArrayOf(NLS_HANDLE, elementCount, stringVector(), false));
    ArrayOf result(NLS_HANDLE, dims, copyRefs);
    for (indexType k = 0; k < elementCount; ++k) {
        if (!sourceRefs[k]) {
            continue;
        }
        auto* sourceObject = dynamic_cast<ClassdefHandleObject*>(sourceRefs[k]->object());
        if (sourceObject == nullptr) {
            continue;
        }
        if (copyElementDef != nullptr) {
            ArrayOf element = ArrayOf::handle(sourceRefs[k]->id());
            ArrayOfVector produced = copyElementDef->evaluateFunction(eval, { element }, 1);
            if (produced.empty() || !produced[0].isHandle() || !produced[0].isScalar()) {
                Error(L"Nelson:interpreter:invalidCopyElement",
                    L"copyElement must return a scalar handle object.");
            }
            copyRefs[k] = scalarHandleRef(produced[0]);
        } else {
            copyRefs[k] = scalarHandleRef(shallowCopyScalarHandle(eval, sourceObject, className));
        }
    }
    retval << result;
    return retval;
}
//=============================================================================
static Dimensions
invalidHandleDimensionsFromInputs(const ArrayOfVector& argIn)
{
    if (argIn.size() == 1) {
        return Dimensions(1, 1);
    }
    std::vector<indexType> dimensionsVector;
    if (argIn.size() == 2 && argIn[1].isNumeric() && argIn[1].isVector() && !argIn[1].isScalar()) {
        ArrayOf dimensionsArgument = argIn[1];
        dimensionsVector = dimensionsArgument.asIndexVector();
    } else {
        for (size_t k = 1; k < argIn.size(); ++k) {
            if (!argIn[k].isNumeric() || !argIn[k].isScalar()) {
                Error(L"Nelson:interpreter:invalidDimension",
                    L"Dimension argument must be a numeric scalar.");
            }
            dimensionsVector.push_back(argIn[k].asScalarIndex(true, true, true));
        }
    }
    if (dimensionsVector.empty()) {
        dimensionsVector = { 0, 0 };
    } else if (dimensionsVector.size() == 1) {
        dimensionsVector.push_back(dimensionsVector.front());
    }
    return Dimensions(dimensionsVector);
}
//=============================================================================
ArrayOfVector
evaluateClassdefInvalidHandle(const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.empty()
        || !(argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray())) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    const std::string className = argIn[0].asUtf8String();
    if (!classdefLoadClass(className) || !classdefIsHandleClass(className)) {
        RaiseError(_(CLASSDEF_ERROR_HANDLE_CLASS_NOT_FOUND) + className,
            "Nelson:interpreter:classNotFound");
    }
    Dimensions dimensions = invalidHandleDimensionsFromInputs(argIn);
    retval << ArrayOf::invalidHandle(className, dimensions);
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
